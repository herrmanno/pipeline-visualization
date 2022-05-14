out=docs/dist
mkdir -p $out
elm make src/Main.elm --output=$out/app.js