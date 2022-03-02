# Exp: SmartPy Lib

Imports:

```sh
spytmp=$HOME/tmp/smartpy-public
git clone https://gitlab.com/SmartPy/smartpy.git "$spytmp"
cp -r $spytmp/smartML/core smartml-core
cp -r $spytmp/smartML/utils_pure smartml-utils-pure
cp -r $spytmp/smartML/michelson_base smartml-michelson-base
cp -r $spytmp/smartML/tools smartml-tools
cp -r $spytmp/smartML/michel smartml-michel
cp -r $spytmp/smartML/utils smartml-utils

echo '(lang dune 1.10)' > dune-project
touch smartML.opam
touch utils_pure.opam
touch michelson_base.opam
touch tools.opam
touch utils.opam
touch michel.opam
```

Build:

```sh
opam switch link <some compatible opam switch> .
opam pin add base58 https://github.com/vbmithr/ocaml-base58.git

eval $(opam env)
dune exec --profile release testgen/main.exe
```
