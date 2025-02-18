#!/usr/bin/env sh
file=runnable-texturepacker.jar
if [ ! -e "$file" ]; then
    echo "$file not found, downloading..."
    curl "https://libgdx-nightlies.s3.amazonaws.com/libgdx-runnables/$file" --output "$file"
    echo "Download complete."
fi
# java -cp "$file" com.badlogic.gdx.tools.texturepacker.TexturePacker textures out atlas
# java -cp "$file" com.badlogic.gdx.tools.texturepacker.TexturePacker textures/minesweeper out/minesweeper atlas
java -cp "$file" com.badlogic.gdx.tools.texturepacker.TexturePacker textures/memory out/memory atlas