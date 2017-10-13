#!/bin/bash
PKG=test/tmp/package

# build package
mix hex.build

TAR=$(ls ex_doc-*.tar | head -n 1)

# extract package
rm -rf $PKG && mkdir -p $PKG/contents && tar xf $TAR -C $PKG && tar xzf $PKG/contents.tar.gz -C $PKG/contents

# compile and build docs
cd $PKG/contents && MIX_ENV=prod mix do deps.get --only prod, compile, docs
