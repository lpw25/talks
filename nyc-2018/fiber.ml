open Import

module Eq = struct
  type ('a, 'b) t = T : ('a, 'a) t

  let cast (type a) (type b) (T : (a, b) t) (x : a) : b = x
end

module Var0 = struct
  module Key = struct
    type 'a t = ..
  end

  module type T = sig
    type t
    type 'a Key.t += T : t Key.t
    val id : int
  end

  type 'a t = (module T with type t = 'a)

  let next = ref 0

  let create (type a) () =
    let n = !next in
    next := n + 1;
    let module M = struct
      type t = a
      type 'a Key.t += T : t Key.t
      let id = n
    end in
    (module M : T with type t = a)

  let id (type a) (module M : T with type t = a) = M.id

  let eq (type a) (type b)
        (module A : T with type t = a)
        (module B : T with type t = b) : (a, b) Eq.t =
    match A.T with
    | B.T -> Eq.T
    | _ -> assert false
end

module Binding = struct
  type t = T : 'a Var0.t * 'a -> t
end

module Int_map = Map.Make(Int)

type ctx =
  { on_error : exn -> unit; (* This callback must never raise *)
    fibers   : int ref; (* Number of fibers running in this execution
                            context *)
    vars     : Binding.t Int_map.t;
    on_release : unit -> unit;
    suspended : task Queue.t; }

and 'a cont =
  ('a, ![], unit) continuation

and task =
  | Cont : 'a * 'a cont -> task
  | Cont_unit : unit cont -> task
  | Exec :
      ctx * 'a * ('a -[Async : 'o. 'o op -> 'o]-> 'b)
      * (ctx -> 'b -> unit) -> task

and 'a waiting =
    Waiting : ctx * 'a cont -> 'a waiting

and 'a ivar_state =
  | Full  of 'a
  | Empty of 'a waiting Queue.t

and 'a ivar = { mutable state : 'a ivar_state }

and mutex =
  { mutable locked  : bool;
    mutable waiters : unit waiting Queue.t; }

and 'a op =
  | Never : 'a op
  | Fork :
      'a * ('a -[Async : 'o. 'o op -> 'o]-> 'b) -> 'b ivar op
  | NFork :
      'a list * ('a -[Async : 'o. 'o op -> 'o]-> 'b) ->
      'b ivar list op
  | Fork_and_join :
      (unit -[Async : 'o. 'o op -> 'o]-> 'a) *
      (unit -[Async : 'o. 'o op -> 'o]-> 'b) ->
      ('a * 'b) op
  | Parallel_map :
      'a list * ('a -[Async : 'o. 'o op -> 'o]-> 'b) -> 'b list op
  | Parallel_iter :
      'a list * ('a -[Async : 'o. 'o op -> 'o]-> unit) -> unit op
  | Get : 'a Var0.t -> 'a option op
  | Get_exn : 'a Var0.t -> 'a op
  | Set :
      'a Var0.t * 'a * (unit -[Async : 'o. 'o op -> 'o]-> 'b) -> 'b op
  | With_error_handler :
      (unit -[Async : 'o. 'o op -> 'o]-> 'a) * (exn -> unit) -> 'a op
  | Wait_errors :
      (unit -[Async : 'o. 'o op -> 'o]-> 'a) -> ('a, unit) result op
  | Fill : 'b ivar * 'b -> unit op
  | Read : 'a ivar -> 'a op
  | Lock : mutex -> unit op
  | Unlock : mutex -> unit op
  | Yield : unit op

effect async = ![ Async : 'o. 'o op -> 'o ]

type ('a, 'b) fork_and_join_state =
  | Nothing_yet
  | Got_a of 'a
  | Got_b of 'b

let initial_context () =
  { on_error   = raise;
    fibers     = ref 1;
    vars       = Int_map.empty;
    on_release = ignore;
    suspended = Queue.create (); }

let subcontext ctx ~on_release =
  { ctx with on_release; fibers = ref 1 }

let set_vars ctx vars =
  { ctx with vars }

let set_error_handler ctx ~on_error =
  { ctx with on_error }

let enqueue ctx s =
  Queue.push s ctx.suspended

let release ctx =
  ctx.on_release ()

let add_refs ctx n =
  ctx.fibers := !(ctx.fibers) + n

let activate (Waiting(ctx, cont)) x =
  enqueue ctx (Cont(x, cont))

let list_of_option_array a =
  let rec loop arr i acc =
    if i = 0 then
      acc
    else
      let i = i - 1 in
      match arr.(i) with
      | None -> assert false
      | Some x ->
        loop arr i (x :: acc)
  in
  loop a (Array.length a) []

let rec exec :
  'a 'b. ctx -> 'a ->
  ('a -[async]-> 'b) -> (ctx -> 'b -> unit) -> unit =
  fun ctx x f g ->
    match f x with
    | res -> g ctx res
    | exception exn ->
        forward_error true ctx exn
    | effect Async(op), k -> begin
        match op with
        | Never -> never ctx k
        | Fork(x, f) -> fork x f ctx k
        | NFork(l, f) -> nfork l f ctx k
        | Fork_and_join(fa, fb) -> fork_and_join fa fb ctx k
        | Parallel_map(l, f) -> parallel_map l f ctx k
        | Parallel_iter(l, f) -> parallel_iter l f ctx k
        | Get var -> get var ctx k
        | Get_exn var -> get_exn var ctx k
        | Set(var, x, f) -> set var x f ctx k
        | With_error_handler(f, h) -> with_error_handler f h ctx k
        | Wait_errors f -> wait_errors f ctx k
        | Fill(ivar, x) -> fill ivar x ctx k
        | Read ivar -> read ivar ctx k
        | Lock mutex -> lock mutex ctx k
        | Unlock mutex -> unlock mutex ctx k
        | Yield -> yield ctx k
      end

and schedule : ctx -> unit =
  fun ctx ->
    match Queue.pop ctx.suspended with
    | exception Queue.Empty -> ()
    | Cont(x, k) -> continue k x
    | Cont_unit k -> continue k ()
    | Exec(ctx', x, f, g) -> exec ctx' x f g

and deref : 'a. ctx -> unit =
  fun ctx ->
    let n = !(ctx.fibers) - 1 in
    assert (n >= 0);
    ctx.fibers := n;
    if n = 0 then release ctx
    else schedule ctx

and forward_error : 'a. bool -> ctx -> exn -> unit =
  fun drf ctx exn ->
    let bt = Printexc.get_raw_backtrace () in
    match ctx.on_error exn with
    | () -> if drf then deref ctx
    | exception exn2 ->
        (* We can't abort the execution at this point, so we just dump
           the error on stderr *)
        let bt2 = Printexc.get_backtrace () in
        let s =
          (Printf.sprintf "%s\n%s\nOriginal exception was: %s\n%s"
             (Printexc.to_string exn2) bt2
             (Printexc.to_string exn) (Printexc.raw_backtrace_to_string bt))
          |> String.split_lines
          |> List.map ~f:(Printf.sprintf "| %s")
          |> String.concat ~sep:"\n"
        in
        let line = String.make 71 '-' in
        Format.eprintf
          "/%s\n\
           | @{<error>Internal error@}: \
           Fiber.Execution_context.forward_error: error handler raised.\n\
           %s\n\
           \\%s@."
          line s line

and never : 'a. ctx -> 'a cont -> unit =
 fun ctx _ ->
   schedule ctx

and finish : 'a. 'a ivar -> ctx -> 'a -> unit =
  fun ivar ctx x ->
    match ivar.state with
    | Full  _ -> assert false
    | Empty q ->
      ivar.state <- Full x;
      Queue.iter (fun handler -> activate handler x) q;
      schedule ctx

and fork :
  'a 'b. 'a -> ('a -[async]-> 'b) -> ctx ->
    'b ivar cont -> unit =
    fun x f ctx k ->
      let ivar = { state = Empty (Queue.create ()) } in
      add_refs ctx 1;
      enqueue ctx (Cont(ivar, k));
      exec ctx x f (finish ivar)

and nfork :
  'a 'b. 'a list -> ('a -[async]-> 'b) ->
    ctx -> 'b ivar list cont -> unit =
  fun l f ctx k ->
    match l with
    | [] -> continue k []
    | [x] ->
      let ivar = { state = Empty (Queue.create ()) } in
      add_refs ctx 1;
      enqueue ctx (Cont([ivar], k));
      exec ctx x f (finish ivar)
    | first :: rest ->
      let n = List.length rest in
      add_refs ctx n;
      let rest_ivars =
        List.map rest ~f:(fun x ->
          let ivar = { state = Empty (Queue.create ()) } in
          enqueue ctx (Exec(ctx, x, f, finish ivar));
          ivar)
      in
      let first_ivar = { state = Empty (Queue.create ()) } in
      let ivars = first_ivar :: rest_ivars in
      enqueue ctx (Cont(ivars, k));
      exec ctx first f (finish first_ivar)

and fork_and_join :
  'a 'b. (unit -[async]-> 'a) -> (unit -[async]-> 'b) ->
      ctx -> ('a * 'b) cont -> unit =
  fun fa fb ctx k ->
    let state = ref Nothing_yet in
    let finish_a ctx a =
      match !state with
      | Nothing_yet -> state := Got_a a; deref ctx
      | Got_a _ -> assert false
      | Got_b b -> continue k (a, b)
    in
    let finish_b ctx b =
      match !state with
      | Nothing_yet -> state := Got_b b; deref ctx
      | Got_a a -> continue k (a, b)
      | Got_b _ -> assert false
    in
    add_refs ctx 1;
    enqueue ctx (Exec(ctx, (), fb, finish_b));
    exec ctx () fa finish_a

and parallel_map :
      'a 'b. 'a list -> ('a -[async]-> 'b) ->
        ctx -> 'b list cont -> unit =
  fun l f ctx k ->
    match l with
    | [] -> continue k []
    | [x] ->
      exec ctx x f (fun _ x -> continue k [x])
    | first :: rest ->
      let n = List.length l in
      add_refs ctx (n - 1);
      let left_over = ref n in
      let results = Array.make n None in
      let finish_i i ctx x =
        results.(i) <- Some x;
        decr left_over;
        if !left_over = 0 then begin
          continue k (list_of_option_array results)
        end else begin
          deref ctx
        end
      in
      List.iteri rest ~f:(fun i x ->
        enqueue ctx (Exec(ctx, x, f, finish_i (i + 1))));
      exec ctx first f (finish_i 0)

and parallel_iter :
  'a. 'a list -> ('a -[async]-> unit) ->
    ctx -> unit cont -> unit =
  fun l f ctx k ->
    match l with
    | [] -> continue k ()
    | [x] -> exec ctx x f (fun _ _ -> continue k ())
    | first :: rest ->
      let n = List.length l in
      add_refs ctx (n - 1);
      let left_over = ref n in
      let finish ctx () =
        decr left_over;
        if !left_over = 0 then begin
          continue k ()
        end else begin
          deref ctx
        end
      in
      List.iter rest ~f:(fun x ->
        enqueue ctx (Exec(ctx, x, f, finish)));
      exec ctx first f finish

and get :
  'a. 'a Var0.t -> ctx -> 'a option cont -> unit =
  fun var ctx k ->
    match Int_map.find (Var0.id var) ctx.vars with
    | exception Not_found -> continue k None
    | Binding.T (var', v) ->
      let eq = Var0.eq var' var in
      continue k (Some (Eq.cast eq v))

and get_exn :
  'a. 'a Var0.t -> ctx -> 'a cont -> unit =
  fun var ctx k ->
    match Int_map.find (Var0.id var) ctx.vars with
    | exception Not_found -> discontinue k (Failure "Fiber.Var.find_exn")
    | Binding.T (var', v) ->
      let eq = Var0.eq var' var in
      continue k (Eq.cast eq v)

and set :
  'a 'b. 'a Var0.t -> 'a -> (unit -[async]-> 'b) ->
    ctx -> 'b cont -> unit =
  fun (type t) (var : t Var0.t) x f ctx k ->
    let (module M) = var in
    let data = Binding.T (var, x) in
    let ctx' = set_vars ctx (Int_map.add M.id data ctx.vars) in
    exec ctx' () f (fun _ res -> continue k res)

and with_error_handler :
  'a. (unit -[async]-> 'a) -> (exn -> unit) ->
      ctx -> 'a cont -> unit =
  fun f err ctx k ->
    let on_error exn =
      try
        err exn
      with exn ->
        forward_error false ctx exn
    in
    let ctx' = set_error_handler ctx ~on_error in
    exec ctx' () f (fun _ x -> continue k x)

and wait_errors :
  'a. (unit -[async]-> 'a) ->
    ctx -> ('a, unit) Result.t cont -> unit =
  fun f ctx k ->
    let result = ref (Result.Error ()) in
    let on_release () = continue k !result in
    let ctx' = subcontext ctx ~on_release in
    let finish ctx' x =
      result := Ok x;
      deref ctx'
    in
    exec ctx' () f finish

and fill : 'a. 'a ivar -> 'a -> ctx -> unit cont -> unit =
  fun ivar x ctx k ->
    match ivar.state with
    | Full  _ -> discontinue k (Failure "Fiber.Ivar.fill")
    | Empty q ->
      ivar.state <- Full x;
      Queue.iter (fun handler -> activate handler x) q;
      enqueue ctx (Cont_unit(k));
      schedule ctx

and read : 'a. 'a ivar -> ctx -> 'a cont -> unit =
  fun ivar ctx k ->
    match ivar.state with
    | Full  x -> continue k x
    | Empty q ->
      Queue.push (Waiting(ctx, k)) q;
      schedule ctx

and lock : 'a. mutex -> ctx -> unit cont -> unit  =
  fun lock ctx k ->
    if lock.locked then begin
      Queue.push (Waiting(ctx, k)) lock.waiters;
      schedule ctx
    end else begin
      lock.locked <- true;
      continue k ()
    end

and unlock : 'a. mutex -> ctx -> unit cont -> unit  =
  fun lock _ctx k ->
    assert lock.locked;
    if Queue.is_empty lock.waiters then begin
      lock.locked <- false
    end else begin
      activate (Queue.pop lock.waiters) ()
    end;
    continue k ()

and yield : 'a. ctx -> unit cont -> unit =
  fun ctx k ->
    enqueue ctx (Cont_unit(k));
    schedule ctx

let never : unit -[async]-> 'a =
  fun () ->
    perform Async(Never)

let fork : (unit -[async]-> 'b) -[async]-> 'b ivar =
  fun f ->
    perform Async(Fork((), f))

let nfork_map : 'a list -> f:('a -[async]-> 'b) -[async]-> 'b ivar list =
  fun l ~f ->
    perform Async(NFork(l, f))

let nfork : (unit -[async]-> 'a) list -[async]-> 'a ivar list =
  fun l ->
    perform Async(NFork(l, fun f -> f ()))

let fork_and_join :
  (unit -[async]-> 'a) -> (unit -[async]-> 'b) -[async]->
  'a * 'b =
  fun fa fb ->
    perform Async(Fork_and_join(fa, fb))

let fork_and_join_unit :
  (unit -[async]-> unit) -> (unit -[async]-> 'a)
    -[async]-> 'a =
  fun fa fb ->
    snd (perform Async(Fork_and_join(fa, fb)))

let parallel_map :
  'a list -> f:('a -[async]-> 'b) -[async]-> 'b list =
  fun l ~f ->
    perform Async(Parallel_map(l, f))

let parallel_iter :
  'a list -> f:('a -[async]-> unit) -[async]-> unit =
  fun l ~f ->
    perform Async(Parallel_iter(l, f))

module Var = struct

  type 'a t = 'a Var0.t

  let create = Var0.create

  let get : 'a Var0.t -[async]-> 'a option =
    fun t -> perform Async(Get t)

  let get_exn : 'a Var0.t -[async]-> 'a =
    fun t -> perform Async(Get_exn t)

  let set : 'a Var0.t -> 'a -> (unit -[async]-> 'b) -[async]-> 'b =
    fun t x f -> perform Async(Set(t, x, f))

end

let with_error_handler :
  (unit -[async]-> 'a) ->
  on_error:(exn -> unit) -[async]-> 'a =
  fun f ~on_error ->
    perform Async(With_error_handler(f, on_error))

let wait_errors :
  (unit -[async]-> 'a) -[async]-> ('a, unit) result =
  fun f ->
    perform Async(Wait_errors f)

let fold_errors f ~init ~on_error =
  let acc = ref init in
  let on_error exn =
    acc := on_error exn !acc
  in
  match wait_errors (fun () -> with_error_handler f ~on_error) with
  | Ok _ as ok -> ok
  | Error ()   -> Error !acc

let collect_errors f =
  fold_errors f
    ~init:[]
    ~on_error:(fun e l -> e :: l)

let finalize :
  (unit -[async]-> 'a) -> finally:(unit -[async]-> unit)
  -[async]-> 'a =
  fun f ~finally ->
    let res = wait_errors f in
    finally ();
    match res with
    | Ok x -> x
    | Error () -> never ()

module Ivar = struct

  type 'a t = 'a ivar

  let create () = { state = Empty (Queue.create ()) }

  let fill : 'a ivar -> 'a  -[async]-> unit =
    fun t x -> perform Async(Fill(t, x))

  let read : 'a ivar -[async]-> 'a =
    fun t -> perform Async(Read t)

end

module Future = struct

  type 'a t = 'a Ivar.t

  let wait = Ivar.read

end

module Mutex = struct
  type t = mutex

  let create () =
    { locked  = false;
      waiters = Queue.create (); }

  let with_lock : mutex -> (unit -[async]-> 'a) -[async]-> 'a =
    fun t f ->
      perform Async(Lock t);
      finalize f
        ~finally:(fun () -> perform Async(Unlock t))

end

let yield : unit -[async]-> unit =
  fun () -> perform Async(Yield)

exception Never

let run f x =
  let result = ref None in
  let ctx = initial_context () in
  let finish ctx y =
    result := Some y
  in
  exec ctx x f finish;
  match !result with
  | None -> raise Never
  | Some res -> res
