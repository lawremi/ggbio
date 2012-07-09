theme_null<- function()
{
  size <- 12
  o = list(axis.line=theme_blank(), 
    axis.text.x=theme_blank(), 
    axis.text.y=theme_blank(), 
    axis.ticks=theme_blank(), 
    axis.ticks.length=unit(0.3, "lines"), 
    axis.ticks.margin=unit(0.5, "lines"), 
    axis.title.x=theme_blank(), 
    axis.title.y=theme_blank(), 
    legend.background=theme_rect(fill="white", colour=NA), 
    legend.key=theme_rect(colour="white"), 
    legend.key.size=unit(1.2, "lines"), 
    legend.position="right", 
    legend.text=theme_text(size=size*0.8), 
    legend.title=theme_text(size=size*0.8, face="bold", 
      hjust=0), 
    panel.background=theme_blank(), 
    panel.border=theme_blank(), 
    panel.grid.major=theme_blank(), 
    panel.grid.minor=theme_blank(), 
    panel.margin=unit(0, "lines"), 
    plot.background=theme_blank(), 
    plot.margin=unit(c(1, 1, 0.5, 0.5), "lines"), 
    plot.title=theme_text(size=size*1.2), 
    strip.background=theme_rect(fill="grey90", 
      colour="grey50"), 
    strip.text.x=theme_text(size=size*0.8), 
    strip.text.y=theme_text(size=size*0.8, angle=-90)) 
  return(structure(o, class="options"))}   


## TODO: with axis?
theme_alignment <-   function (ylabel = FALSE, base_size = 12,
                               base_family = "", 
                               axis = TRUE, border = TRUE, grid = TRUE) 
{
  res <- structure(
            list(
                 axis.line = theme_blank(),
                 ## axis.text.x = theme_blank(),
                 axis.text.y = {if(ylabel)
                 theme_text(family = base_family, size = base_size * 0.8,
                            lineheight = 0.9, hjust = 1)
                                else
                   theme_blank()},
                 ## axis.ticks = theme_blank(),
                 axis.title.x = theme_text(family = base_family, size = base_size, vjust = 1),
                 axis.title.y = theme_text(family = base_family, size = base_size,
                   angle = 90, vjust = 0.5, colour = "white"),
                 axis.ticks.length = unit(0.3, "lines"),
                 axis.ticks.margin = unit(0.5, "lines"),

                 panel.background = theme_blank(),
                 panel.border = {if(border)
                                   theme_rect(fill = NA, colour = "grey50")
                                 else
                                   theme_blank()},
                 panel.grid.major = {if(grid)
                                       theme_line(colour = "grey90", size = 0.2)
                                     else
                                       theme_blank()},
                 panel.grid.minor = theme_blank(),
                 panel.margin = unit(0.25, "lines"),
                 
                 strip.background = theme_rect(fill = "grey80", colour = "grey50"), 
                 strip.text.x = theme_text(family = base_family, size = base_size * 0.8),
                 strip.text.y = theme_text(family = base_family, 
                   size = base_size * 0.8, angle = -90),
                 plot.background = theme_rect(colour = NA), 
                 plot.title = theme_text(family = base_family, size = base_size * 
                   1.2),
                 plot.margin = unit(c(1, 1, 0.5, 0.5), "lines")), 
            class = "options")
  if(!ylabel){
    res <- list(res, list(scale_y_continuous(breaks = NULL)))
  }
  res
}

## need a theme_bw2()
