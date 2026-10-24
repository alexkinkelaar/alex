##Custom ggplot theme
theme_alex = function(){
  theme_void() %+replace%
    #modifying void theme
  theme(
    #plot elements
    plot.caption = element_text(face = "italic", size = 10, color = "white"),
    plot.title = element_text(face = "bold", size = 14, color = "white"),
    plot.subtitle = element_text(face = "italic", size = 11, color = "white"),
    plot.background = element_rect(fill = "#262626", color = NA),
    #panel/grid elements
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #axis elements
    axis.text.x = element_text(color = "white", size = 9),
    axis.title.y = element_text(color = "white", size = 11, angle = 90),
    axis.title.x = element_text(color = "white", size = 11),
    axis.ticks.x = element_line(color = "white"),
    axis.text.y = element_text(color = "white", size = 9),
    axis.ticks.y = element_line(color = "white"),
    axis.line.x = element_line(color = "white"),
    axis.line.y = element_line(color = "white"),
    #legend elements
    legend.position = "bottom",
    legend.text = element_text(color = "white"),
    legend.title = element_blank()
  )
}
#custom colors
custcolors = c(
  `light blue` = "#85D4E3",
  `salmon` = "#FD7F71",
  `green` = "#0B775E",
  `purple`= "#35274A",
  #`orange` = "#F2300F", <- Clashes with salmon?
  `light gray` = "#D9D0D3",
  `dark blue` = "#00268E"
)
#helper function for palette function
#extracts given colors as hex codes
colors_alex = function(...){

  cols = c(...)

  if(is.null(cols))
    return(custcolors)

  custcolors[cols]
}
#Palette list -> allows additional palettes to be added modularly
#Changes to individual palettes are made in this list - additional palettes can be added, modified, or removed here
# main  - full palette, used for continuous gradients
# dark  - discrete colors readable on the dark theme_alex() background (#262626)
#         (purple and dark blue are ~1.1-1.2:1 contrast on #262626, so they are left out)
# light - discrete colors readable on a white/light background
#         (light blue and light gray are ~1.5-1.7:1 contrast on white, so they are left out)
palettes_alex = list(
  `main` = colors_alex("light blue", "salmon", "green", "purple", "light gray", "dark blue"),
  `dark` = colors_alex("light blue", "salmon", "light gray", "green"),
  `light` = colors_alex("salmon", "green", "purple", "dark blue")
)
#Function to interpolate custom color palette
# palette - the palette to return
# reverse - boolean input to reverse (or not) the palette's ordering
# ... - additional arguments
palette_alex = function(palette = "main", reverse = FALSE, ...){
  pal = palettes_alex[[palette]]
  if(is.null(pal)) stop("Unknown palette '", palette, "'. Options: ", paste(names(palettes_alex), collapse = ", "))
  if(reverse) pal = rev(pal)
  colorRampPalette(pal,...)
}
#Discrete palette -> returns the palette's colors in order (no interpolation)
#Only interpolates if more levels are requested than the palette has colors
discrete_palette_alex = function(palette = "dark", reverse = FALSE){
  pal = palettes_alex[[palette]]
  if(is.null(pal)) stop("Unknown palette '", palette, "'. Options: ", paste(names(palettes_alex), collapse = ", "))
  if(reverse) pal = rev(pal)
  pal = unname(pal)
  function(n){
    if(n > length(pal)){
      warning("Palette '", palette, "' has ", length(pal), " colors but ", n, " are needed; interpolating.")
      return(colorRampPalette(pal)(n))
    }
    pal[seq_len(n)]
  }
}
#Builds a discrete scale across ggplot2 versions (scale_name was deprecated in ggplot2 3.5.0)
discrete_scale_alex = function(aesthetic, palette, reverse, ...){
  pal = discrete_palette_alex(palette = palette, reverse = reverse)
  if(utils::packageVersion("ggplot2") >= "3.5.0"){
    discrete_scale(aesthetic, palette = pal, ...)
  } else {
    discrete_scale(aesthetic, paste0("alex_", palette), palette = pal, ...)
  }
}
#Function for COLOR argument in ggplot
#palette defaults to "dark" for discrete scales (matches theme_alex) and "main" for continuous scales
scale_color_alex = function(palette = NULL, discrete = TRUE, reverse = FALSE,...){
  if(is.null(palette)) palette = if(discrete) "dark" else "main"
  if(discrete){
    discrete_scale_alex("colour", palette = palette, reverse = reverse, ...)
  } else {
    pal = palette_alex(palette = palette, reverse = reverse)
    scale_color_gradientn(colors = pal(256), ...)
  }
}
scale_colour_alex = scale_color_alex
#Function for FILL argument in ggplot
#palette defaults to "dark" for discrete scales (matches theme_alex) and "main" for continuous scales
scale_fill_alex = function(palette = NULL, discrete = TRUE, reverse = FALSE, ...){
  if(is.null(palette)) palette = if(discrete) "dark" else "main"
  if(discrete){
    discrete_scale_alex("fill", palette = palette, reverse = reverse, ...)
  } else {
    pal = palette_alex(palette = palette, reverse = reverse)
    scale_fill_gradientn(colors = pal(256), ...)
  }
}
