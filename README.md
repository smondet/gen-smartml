# Exp: SmartPy Lib

Imports:

```sh
spytmp=$HOME/tmp/smartpy-public
git clone https://gitlab.com/SmartPy/smartpy.git "$spytmp"
cp -r $spytmp/smartML/core smartml-core
cp -r $spytmp/smartML/utils_pure smartml-utils-pure
cp -r $spytmp/smartML/michelson_base smartml-michelson-base

echo '(lang dune 1.10)' > dune-project
touch smartML.opam
touch utils_pure.opam
touch michelson_base.opam
```

Build:

```sh
opam switch link <some compatible opam switch> .
eval $(opam env)
dune exec --profile release testgen/main.exe
```
