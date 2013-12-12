OPAM_DEPENDS="lwt cstruct ocamlgraph ounit pa_ounit quickcheck"

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
opam install ${OPAM_DEPENDS}

eval `opam config env`

function github_install {
    git clone "https://github.com/frenetic-lang/$1" &&
        (cd "$1" && echo "$1 HEAD" && git rev-parse HEAD &&
         ocaml setup.ml -configure ${@:2} && make && make install)
}

github_install ocaml-packet
github_install ocaml-openflow --enable-lwt
github_install ocaml-topology
github_install dprle

ocaml setup.ml -configure --enable-tests --enable-quickcheck
make
make test
