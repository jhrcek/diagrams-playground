# diagrams-playground

```bash
stack build \
diagrams-playground:exe:play \
--file-watch \
--exec 'play --width 500 --output a.svg'
```

## Animations

```bash
# generate sequence of frames
stack build \
diagrams-playground:exe:anim \
--file-watch \
--exec 'anim --width 500 --output .png'
# join the frames into one gif
ffmpeg -i %02d.png output.gif
```

## TODO
- read [active](https://www.stackage.org/haddock/lts-13.15/active-0.2.0.13/Data-Active.html) haddock
