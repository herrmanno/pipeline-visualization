dir="$(dirname "$0")"
out=docs/dist

mkdir -p $out
fswatch -o "$dir/public" "$dir/src" -e "$dir/public/dist" | xargs -n1 -I{} elm make src/Main.elm --output=$out/app.js