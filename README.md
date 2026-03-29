# png-distance-map

`png-distance-map` is a small Haskell command-line tool for baking PNG distance maps and nearest-mask coordinate maps.

It is aimed at asset pipelines where you want explicit control over:

- signed vs unsigned distance encoding,
- output PNG sample depth,
- distance scale,
- which source channel defines the mask,
- threshold and polarity,
- and how the output channels are composed, including copying channels from the original image.

## Build

With Cabal:

```bash
cabal build
```

With Nix:

```bash
nix build
nix run . -- --help
```

## Usage

```text
png-distance-map INPUT.png OUTPUT.png [OPTIONS]
```

Examples:

Unsigned exterior map from alpha, written as a 16-bit grayscale PNG:

```bash
png-distance-map in.png out.png \
  --mode unsigned \
  --mask-channel auto \
  --threshold 0.5 \
  --output-bit-depth 16 \
  --scale 64 \
  --output-channels d
```

Nearest-mask coordinates in red and green, distance in blue, original alpha preserved:

```bash
png-distance-map in.png out.png \
  --mode unsigned \
  --mask-channel a \
  --output-bit-depth 16 \
  --scale 64 \
  --output-channels x,y,d,a
```

Signed distance in alpha while copying RGB from the input:

```bash
png-distance-map in.png out.png \
  --mode signed \
  --mask-channel a \
  --output-bit-depth 16 \
  --scale 128 \
  --output-channels r,g,b,d
```

## Input vs output bit depth

`--output-bit-depth` controls the PNG that this tool writes.

Input PNG depth is detected automatically by JuicyPixels during decoding. The transform itself works on a normalized internal representation, so input and output depth are independent:

- a low-bit grayscale input can be written back out as 16-bit,
- a 16-bit input can be written out as 8-bit,
- copied input channels are quantized to the requested output depth.

In practice, with JuicyPixels PNG support today:

- PNG reading covers the ordinary PNG grayscale storage depths 1, 2, 4, 8, and 16 bits, and also 8/16-bit grayscale+alpha, RGB, and RGBA forms as decoded by the library,
- PNG writing supports `ImageY8`, `ImageY16`, `ImageYA8`, `ImageYA16`, `ImageRGB8`, `ImageRGB16`, and `ImageRGBA8`, `ImageRGBA16`.

That means this tool accepts the full range JuicyPixels can decode through `readImage`, while output sample depth is deliberately exposed as an explicit 8-or-16-bit choice because those are the PNG sample depths JuicyPixels exposes for direct non-paletted PNG writing.

## Mask semantics

The chosen mask channel is normalized to `[0,1]` and compared to the threshold.

- Without `--invert-mask`, values `>= threshold` are treated as inside the mask.
- With `--invert-mask`, the result is flipped.

`--mask-channel auto` means:

- use alpha when the source PNG has an alpha channel,
- otherwise use luma.

## Output channel syntax

`--output-channels` takes a comma-separated list of 1 to 4 channel sources.
The number of entries determines the output PNG format:

- 1 entry → grayscale
- 2 entries → grayscale + alpha
- 3 entries → RGB
- 4 entries → RGBA

Allowed channel source tokens:

- `d` – generated distance value
- `x` – x-coordinate of the nearest inside-mask pixel
- `y` – y-coordinate of the nearest inside-mask pixel
- `r`, `g`, `b`, `a` – copy that channel from the input
- `l` – copy luma from the input
- `0` – constant zero
- `1` – constant full scale

Examples:

- `d`
- `d,a`
- `d,d,d`
- `x,y,d,a`
- `r,g,b,d`
- `0,0,0,d`

`--compose` remains accepted as an alias for compatibility.

## Distance encoding

For unsigned mode, the encoded value is:

```text
encoded = clamp(distanceOutside * scale)
```

Inside-mask pixels are therefore encoded as zero.

PNG stores unsigned integers, so signed distance maps are biased around the midpoint.
For signed mode, the encoded value is:

```text
encoded = clamp(midpoint + signedDistance * scale)
```

with negative distances inside the mask and positive distances outside.

## Coordinate encoding

The `x` and `y` channel tokens encode the coordinates of the nearest pixel that lies inside the thresholded mask.

They are encoded independently of `--scale`, by mapping the pixel coordinate range onto the full numeric range of the chosen output bit depth:

```text
xEncoded = round(x * maxSample / (width  - 1))
yEncoded = round(y * maxSample / (height - 1))
```

So, for 16-bit output, you get much finer coordinate precision than with 8-bit output.
If the mask is empty, the encoded coordinate channels are written as zero.

In signed mode, `d` is signed, but `x` and `y` still refer to the nearest pixel inside the mask.

## Notes

The distance transform is exact Euclidean distance, implemented in pure Haskell using the Felzenszwalb–Huttenlocher separable linear-time algorithm on rows and columns.
