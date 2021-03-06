# JuicyPixels-scale-dct

> Scale [friday](https://hackage.haskell.org/package/friday) images with DCT.
 
This library is based on the work of [Oleg Grenrus](https://github.com/phadej) on [JuicyPixels-scale-dct](https://github.com/phadej/JuicyPixels-scale-dct) and improves upon this work be allowing images of any type to be scaled (as long as their underlying pixel components are `Integral` and 'linear', is not `HSV`), not just RGBA8 images.

This will probably only work on colour spaces which are "linear", HSV is unlikely to work because hue is is an angle, and taking the DCT of angles probably doesn't make sense.

[friday](https://hackage.haskell.org/package/friday) is a Haskell library
for manipulating images in a functional way.

---

## Example image

![phadej](https://raw.githubusercontent.com/axman6/friday-scale-dct/master/phadej.png)

### smaller

![phadej](https://raw.githubusercontent.com/axman6/friday-scale-dct/master/phadej-small.png)

### larger

![phadej](https://raw.githubusercontent.com/axman6/friday-scale-dct/master/phadej-large.png)
