#!/bin/sh

# $1 is our source haskell
rm -rf tmp.old
mv tmp tmp.old
mkdir tmp
tmp="tmp/$1"
cat $1 >> $tmp
echo "main :: IO ()" >> $tmp
echo "main = return ()"  >> $tmp

cd tmp
jhc "$1"
sed -i 's/^main(/disregard_main(/' hs.out_code.c
gcc '-std=gnu99' -D_GNU_SOURCE '-falign-functions=4' -ffast-math -Wshadow -Wextra -Wall -Wno-unused-parameter -o libdynhs.so \
  hs.out_code.c -DNDEBUG -O3 -fPIC -shared
mv libdynhs.so ..