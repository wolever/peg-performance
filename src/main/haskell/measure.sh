#!/usr/bin/env bash

dir=$(cd $(dirname $0); pwd -P)
cd $dir

tries=5
if [ ! -z "$1" ]; then
  tries=$1
fi

echo "Performance for $(basename $dir)"
ghc --version

echo "Building..."
rm -rf target
mkdir target
time ghc -o target/main Main.hs

echo "Running..."
for i in $(jot - 1 $tries); do
  time ./target/main
done

rm Main.hi

echo "Done!"