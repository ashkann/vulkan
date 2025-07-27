#!/usr/bin/env sh
if [ -z "$1" ]
  then
    echo "Project name is required."
    exit 1
fi
project=$1
packer=runnable-texturepacker.jar
if [ ! -e "$packer" ]; then
    echo "$packer not found, downloading..."
    curl "https://libgdx-nightlies.s3.amazonaws.com/libgdx-runnables/$packer" --output "$packer"
    echo "Download complete."
fi
java -cp "$packer" com.badlogic.gdx.tools.texturepacker.TexturePacker textures/$project out/$project atlas