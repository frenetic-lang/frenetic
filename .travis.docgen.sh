#!/bin/bash
# From https://github.com/simonjbeaumont/ocaml-travis-gh-pages

if [ -z "$TRAVIS" -o "$TRAVIS_PULL_REQUEST" != "false" ]; then
  echo "[docgen] This is not a push Travis-ci build, doing nothing..."
  exit 0
else
  echo "[docgen] Updating docs on Github pages..."
fi

DOCSRC=$(opam config var ${PACKAGE}:build)/_build/default/_doc
DOCDIR=.gh-pages
if [ -n "$KEEP" ]; then trap "rm -rf $DOCDIR" EXIT; fi
rm -rf $DOCDIR

# Error out if $GH_TOKEN is empty or unset
: ${GH_TOKEN:?"GH_TOKEN need to be uploaded via travis-encrypt"}

git clone https://${GH_TOKEN}@github.com/${TRAVIS_REPO_SLUG} $DOCDIR 2>&1 | sed -e "s/$GH_TOKEN/!REDACTED!/g"
git -C $DOCDIR checkout gh-pages || git -C $DOCDIR checkout --orphan gh-pages

cp $DOCSRC/* $DOCDIR

git -C $DOCDIR config user.email "travis@travis-ci.org"
git -C $DOCDIR config user.name "Travis"
git -C $DOCDIR add .
git -C $DOCDIR commit --allow-empty -m "Travis build $TRAVIS_BUILD_NUMBER pushed docs to gh-pages"
git -C $DOCDIR push origin gh-pages 2>&1 | sed -e "s/$GH_TOKEN/!REDACTED!/g"
echo "[docgen] updated docs successfully!"

