theme_null <- function(){theme_bw() + theme(
                axis.text.x=element_blank(),
                axis.text.y=element_blank(), 
                axis.ticks=element_blank(), 
                axis.title.x=element_blank(), 
                axis.title.y=element_blank(), 
                legend.background=element_rect(fill="white", colour=NA), 
                legend.key=element_rect(colour="white"), 
                panel.background=element_blank(), 
                panel.border=element_blank(), 
                panel.grid.major=element_blank(), 
                panel.grid.minor=element_blank(), 
                plot.background=element_blank() 
                )}



## TODO: with axis?

theme_alignment <-   function (ylabel = FALSE, base_size = 12,
                               base_family = "", 
                               axis = TRUE, border = TRUE, grid = TRUE) 
{
  res <- theme_gray() + 
    theme(
      axis.line = element_blank(),
      axis.text.y = {if(ylabel)
                       element_text(family = base_family, size = base_size * 0.8,
                                    lineheight = 0.9, hjust = 1)
      else
        element_blank()},
      axis.title.x = element_text(family = base_family, size = base_size, vjust = 1),
      axis.title.y = element_text(family = base_family, size = base_size,
        angle = 90, vjust = 0.5, colour = "white"),
      axis.ticks.length = unit(0.3, "lines"),
      axis.ticks.margin = unit(0.5, "lines"),
      panel.background = element_blank(),
      panel.border = {if(border)
                        element_rect(fill = NA, colour = "grey50")
      else
        element_blank()},
      panel.grid.major = {if(grid)
                            element_line(colour = "grey90", size = 0.2)
      else
        element_blank()},
      panel.grid.minor = element_blank(),
      panel.margin = unit(0.25, "lines"),
      strip.background = element_rect(fill = "grey80", colour = "grey50"), 
      strip.text.x = element_text(family = base_family, size = base_size * 0.8),
      strip.text.y = element_text(family = base_family, 
        size = base_size * 0.8, angle = -90),
      plot.background = element_rect(colour = NA), 
      plot.title = element_text(family = base_family, size = base_size * 
        1.2),
      plot.margin = unit(c(1, 1, 0.5, 0.5), "lines")) 
  if(!ylabel){
    res <- list(res, list(scale_y_continuous(breaks = NULL)))
  }
  res
}

theme_pack_panels <- function(strip.bg = FALSE, strip.text.y = TRUE){
  res <- theme(panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.y = element_blank(),
              strip.background = if(strip.bg){
                element_rect(fill = "grey80", colour = NA)
              }else{
                element_blank()
              },
              strip.text.y = if(strip.text.y){element_text(angle = 0)}else{element_blank()},
              panel.margin = grid::unit(0, "lines"))
  res <- c(list(res),
           list(scale_y_continuous(breaks = NULL)))
                       }

theme_noexpand <- function(){
  c(list(scale_x_continuous(expand = c(0, 0))),
  list(scale_y_continuous(expand = c(0, 0))))
}



