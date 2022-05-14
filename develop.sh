dir="$(dirname "$0")"

mkdir -p public/dist
fswatch -o "$dir/public" "$dir/src" -e "$dir/public/dist" | xargs -n1 -I{} elm make src/Main.elm --output=public/dist/app.js