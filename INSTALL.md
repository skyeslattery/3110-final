To run the code, make sure opam is installed and you are able to run dune projects on your computer. Then download the code off and navigate to the root folder in your terminal.
It might be something like:
``` sh
% cd [path to the file]/3110-final-main
```
Then install ocaml-canvas, the GUI library we are using to display our graphics:
``` sh
% opam install ocaml-canvas
```
Finally, in the project root folder, build and run the program!
``` sh
% dune build
% dune exec bin/main.exe
```
If ocaml-canvas is not available in your opam repository, try the following steps (from ocaml-canvas installation https://ocamlpro.github.io/ocaml-canvas/sphinx/install.html#build-and-install-with-dune):
 
Build and install with dune:
Checkout the sources of ocaml-canvas in a directory.

You need a switch with at least version 4.03.0 of OCaml, you can for example create it with:

``` sh
% opam switch create 4.14.0
```
Then, you need to install all the dependencies:
``` sh
% opam install --deps-only .
```
Finally, you can build the package and install it:
``` sh
% eval $(opam env)
% dune build @configure --auto-promote
% dune build
% dune install
```

Note that a Makefile is provided, it contains the following targets:

build: build the code

install: install the generated files

build-deps: install opam dependencies

sphinx: build sphinx documentation (from the sphinx/ directory)

dev-deps: build development dependencies, in particular ocamlformat, odoc and merlin

doc: build documentation with odoc

fmt: format the code using ocamlformat

test: run tests
