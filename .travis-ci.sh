case "$OCAML_VERSION,$OPAM_VERSION" in
3.12.1,1.0.0) ppa=avsm/ocaml312+opam10 ;;
3.12.1,1.1.0) ppa=avsm/ocaml312+opam11 ;;
4.00.1,1.0.0) ppa=avsm/ocaml40+opam10 ;;
4.00.1,1.1.0) ppa=avsm/ocaml40+opam11 ;;
4.01.0,1.0.0) ppa=avsm/ocaml41+opam10 ;;
4.01.0,1.1.0) ppa=avsm/ocaml41+opam11 ;;
*) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac

echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam time

export OPAMYES=1
export OPAMVERBOSE=1
echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version


opam init
eval `opam config env`
export CAML_LD_LIBRARY_PATH="$EXTRA_LD_LIBRARY_PATH:$CAML_LD_LIBRARY_PATH"
opam install ${OPAM_DEPENDS}

for fdep in $FRENETIC_DEPENDS; do
    echo $fdep HEAD
    curl "https://api.github.com/repos/frenetic-lang/$fdep/git/refs/heads/master" 2>/dev/null \
      | grep sha | cut -d\" -f4
done
opam repository add frenetic-opam https://github.com/frenetic-lang/opam-bleeding.git
opam install ${FRENETIC_DEPENDS//ocaml-/}

ocaml setup.ml -configure ${CONFIG_FLAGS}
make
make test
