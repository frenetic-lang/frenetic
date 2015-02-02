### User-defined variables

# The package name
pkg=${PACKAGE:-my-package}

# Run the basic installation step
install_run=${INSTALL:-true}

# Run the optional dependency step
depopts_run=${DEPOPTS:-false}

# Run the test step
tests_run=${TESTS:-true}

# Run the reverse dependency rebuild step
revdep_run=${REVDEPS:-false}

# other variables
EXTRA_DEPS=${EXTRA_DEPS:-""}
PRE_INSTALL_HOOK=${PRE_INSTALL_HOOK:-""}
POST_INSTALL_HOOK=${POST_INSTALL_HOOK:-""}

# the ocaml version to test
OCAML_VERSION=${OCAML_VERSION:-latest}

### Script

set -ue
unset TESTS

install() {
  if [ "$EXTRA_DEPS" != "" ]; then
    opam install $EXTRA_DEPS
  fi

  eval ${PRE_INSTALL_HOOK}
  echo "opam install ${pkg} $@"
  opam install ${pkg} $@
  eval ${POST_INSTALL_HOOK}

  if [ "$EXTRA_DEPS" != "" ]; then
    opam remove $EXTRA_DEPS
  fi
}

case "$OCAML_VERSION" in
3.12) ppa=avsm/ocaml312+opam12 ;;
4.00) ppa=avsm/ocaml40+opam12  ;;
4.01) ppa=avsm/ocaml41+opam12  ;;
4.02) ppa=avsm/ocaml42+opam12  ;;
*)    ppa=avsm/ocaml42+opam12  ;;
esac

sudo add-apt-repository "deb mirror://mirrors.ubuntu.com/mirrors.txt trusty main restricted universe"
sudo add-apt-repository --yes ppa:${ppa}
sudo apt-get update -qq
sudo apt-get install -y ocaml-compiler-libs ocaml-interp ocaml-base-nox ocaml-base ocaml ocaml-nox ocaml-native-compilers camlp4 camlp4-extra opam

export OPAMYES=1

# Init opam
opam init -a
opam pin add ${pkg} . -n
eval $(opam config env)

# Install the external dependencies
depext=`opam list --required-by ${pkg} --rec -e ubuntu -s | tr '\n' ' ' | sed 's/ *$//'`
if [ "$depext" != "" ]; then
  echo Ubuntu depexts: "${depext}"
  sudo apt-get install -qq ${depext}
fi

# Install the external source dependencies
srcext=`opam list --required-by ${pkg} --rec -e source,linux -s | tr '\n' ' ' | sed 's/ *$//'`
if [ "$srcext" != "" ]; then
  echo Ubuntu srcext: "${srcext}"
  curl -sL ${srcext} | bash
fi

# Install the OCaml dependencies and test-dependencies
echo "opam install ${pkg} --deps-only"
opam install ${pkg} --deps-only -t

# Compile with optional dependencies
# pick from $DEPOPTS if set or query OPAM
depopts=${DEPOPTS:-$(opam show ${pkg} | grep -oP 'depopts: \K(.*)' | sed 's/ | / /g')}
opam install ${depopts}

./configure --enable-tests
make
make test
