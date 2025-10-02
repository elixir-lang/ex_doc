#!/bin/bash
PKG=test/tmp

# build package
mix hex.build

TAR=$(ls ex_doc-*.tar | head -n 1)

# extract package
rm -rf $PKG && mkdir -p $PKG/contents && tar xf $TAR -C $PKG && tar xzf $PKG/contents.tar.gz -C $PKG/contents

# compile and build docs
cd $PKG/contents
MIX_ENV=prod mix do deps.get --only prod + compile + docs
MIX_ENV=prod mix docs --proglang erlang --output doc/erlang

# run assertions
test -f doc/index.html || echo "doc/index.html missing"
test -f doc/erlang/index.html || echo "doc/erlang/index.html missing"
test -f doc/ExDoc.epub || echo "doc/ExDoc.epub missing"
