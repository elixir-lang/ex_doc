#!/bin/bash
PKG=test/tmp/package

# build package
mix hex.build

# extract package
rm -rf $PKG && mkdir -p $PKG/contents && tar xf ex_doc-*.tar -C $PKG && tar xzf $PKG/contents.tar.gz -C $PKG/contents

# compile and build docs
cd $PKG/contents && MIX_ENV=prod mix do deps.get --only prod, compile, docs
