# Vulkan 2D Engine
Trying to learn Vulkan and Haskell hand in hand by doing a 2D engine.

# How to
Compile the shaders:
```
./compile-shaders.sh
```
Generate the atlas
```
./pack-texture.sh
```
### Initial attempts
![](screenshots/screenshot.gif)
- Vulkan pipeline
- Basic sprites
- Basic transformations
- No separate Screen and World spaces

### Affine transformations
![](screenshots/affine.gif)
- SRT (Scale, Rotate, Translate)
- View Projection
- Screen and World spaces

### Text
![](screenshots/first-line-of-text.png)
- Gnu unifont
- Font atlas

### Colored Text
![](screenshots/text-color.jpg)
- Colored vertices
- Text in Screen space (fixed on screen, doesn't move with the world)

### World Text
![](screenshots/world-text.jpg)
-  Text in World space (moves with the world)
