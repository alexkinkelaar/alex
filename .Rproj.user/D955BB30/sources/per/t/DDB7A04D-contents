##Custom ggplot theme
require(ggplot2)
theme_alex = function(){
  theme_void() %+replace%
    #modifying void theme
  theme(
    #plot elements
    plot.caption = element_text(face = "italic", size = 10, color = "white"),
    plot.title = element_text(face = "bold", size = 14, color = "white"),
    plot.subtitle = element_text(face = "italic", size = 11, color = "white"),
    plot.background = element_rect(fill = "grey12", color = NA),
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
palettes_alex = list(
  `main` = colors_alex("light blue", "salmon", "green", "purple", "light gray", "dark blue")
)
#Function to interpolate custom color palette
# palette - the palette to return
# reverse - boolean input to reverse (or not) the palette's ordering
# ... - additional arguments
palette_alex = function(palette = "main", reverse = FALSE, ...){
  pal = palettes_alex[[palette]]
  if(reverse) pal = rev(pal)
  colorRampPalette(pal,...)
}
#Function for COLOR argument in ggplot
scale_color_alex = function(palette = "main", discrete = TRUE, reverse = FALSE,...){
  pal = palette_alex(palette = palette, reverse = reverse)
  if(discrete){
    discrete_scale("coluor", paste0("alex_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colors = pal(256), ...)
  }
}
#Function for FILL argument in ggplot
scale_fill_alex = function(palette = "main", discrete = TRUE, reverse = FALSE, ...){
  pal = palette_alex(palette = palette, reverse = reverse)
  if(discrete){
    discrete_scale("fill", pase0("alex_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colors = pal(256), ...)
  }
}