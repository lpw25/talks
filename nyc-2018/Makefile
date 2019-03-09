
all: fibers.cma fiber.cmi fiberM.cmi

fiber.cmi: fiber.mli import.cmi
	~/Repositories/ocaml-effects-simple/_install/bin/ocamlc fiber.mli

fiber.cmo: fiber.ml fiber.cmi import.cmi
	~/Repositories/ocaml-effects-simple/_install/bin/ocamlc -c fiber.ml

fiberM.cmi: fiberM.mli import.cmi
	~/Repositories/ocaml-effects-simple/_install/bin/ocamlc fiberM.mli

fiberM.cmo: fiberM.ml fiberM.cmi import.cmi
	~/Repositories/ocaml-effects-simple/_install/bin/ocamlc -c fiberM.ml

import.cmo import.cmi: import.ml
	~/Repositories/ocaml-effects-simple/_install/bin/ocamlc -c import.ml

int.cmo int.cmi: int.ml
	~/Repositories/ocaml-effects-simple/_install/bin/ocamlc -c int.ml

fibers.cma: import.cmo fiber.cmo fiberM.cmo int.cmo
	~/Repositories/ocaml-effects-simple/_install/bin/ocamlc -a -o fibers.cma import.cmo fiber.cmo fiberM.cmo int.cmo
