#!/bin/sh

set -e

entry_point="./src/Rain.elm"
build_dir="./build"
js="$build_dir/rain.js"
min="$build_dir/rain.min.js"

rm $min

elm make --optimize --output=$js $entry_point $@

uglifyjs $js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=$min

echo "Compiled size:$(cat $js | wc -c) bytes  ($js)"
echo "Minified size:$(cat $min | wc -c) bytes  ($min)"
echo "Gzipped size: $(cat $min | gzip -c | wc -c) bytes"

rm $js