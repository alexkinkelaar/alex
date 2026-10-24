# alex
A customized theme and color palette for use in R's `ggplot2` library.


### Installing package from Github
```
library(devtools)
install_github("alexkinkelaar/alex")
```

### Theme

```
theme_alex()
```

### Palette

```
scale_fill_alex()
scale_color_alex()
```

| Palette | Colors (in order) | Use |
|---|---|---|
| `dark` (discrete default) | light blue, salmon, light gray, green | Discrete scales on the dark `theme_alex()` background |
| `light` | salmon, green, purple, dark blue | Discrete scales on white/light backgrounds |
| `main` (continuous default) | light blue, salmon, green, purple, light gray, dark blue | Continuous gradients (`discrete = FALSE`) |

Discrete scales use the palette's colors in order (no interpolation), and only interpolate if a chart needs more colors than the palette has.

```
scale_color_alex(palette = "light")       # white background
scale_fill_alex(discrete = FALSE)         # continuous gradient
```
