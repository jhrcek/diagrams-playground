# diagrams-playground

```bash
stack build \
diagrams-playground:exe:play \
--file-watch \
--exec 'play --width 500 --output a.svg'
```

## Animations

- using animMain
```bash
# generate sequence of frames
stack build \
diagrams-playground:exe:anim \
--file-watch \
--exec 'anim --width 500 --output .png'
# join the frames into one gif
ffmpeg -i %02d.png output.gif
```

- using gifMain
```bash
stack build diagrams-playground:exe:anim --file-watch --exec 'anim -o output.gif -w 400'
```
