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
                plot.background=element_blank(),
                strip.background = element_blank(),
                strip.text.y = element_blank(),
                strip.text.x = element_blank()
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
      axis.text = element_text(margin=unit(0.5, "lines")),
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
      panel.spacing = unit(0.25, "lines"),
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
              panel.spacing = grid::unit(0, "lines"))
  res <- c(list(res),
           list(scale_y_continuous(breaks = NULL)))
                       }

theme_noexpand <- function(){
  c(list(scale_x_continuous(expand = c(0, 0))),
  list(scale_y_continuous(expand = c(0, 0))))
}



theme_clear <- function(grid.y = FALSE,
                        grid.x.minor = FALSE,
                        grid.x.major = FALSE,
                        panel.background.fill = "white",
                        panel.border.color = NA,
                        axis.ticks.x = FALSE,
                        axis.ticks.y = TRUE,
                        grid.color = "gray95",
                        axis.line.color = "gray80"){

  res <- theme_gray() + theme(panel.background = element_rect(fill = NA, color = NA),
               panel.border = element_rect(fill = NA, color = panel.border.color))
  if(!grid.y)
    res <- res + theme(panel.grid.major.y = element_blank(),
                       panel.grid.minor.y = element_blank())
  
  if(!grid.x.minor)
    res <- res + theme(panel.grid.minor.x = element_blank())
  
  if(!grid.x.major)
    res <- res + theme(panel.grid.major.x = element_blank())
  else
    res <- res + theme(panel.grid.major.x = element_line(color = grid.color))

  if(!axis.ticks.x)
    res <- res + theme(axis.ticks.x = element_blank())
  else
    res <- res + theme(axis.ticks.x = element_line(colour = "grey50"))
  if(!axis.ticks.y)
    res <- res + theme(axis.ticks.y = element_blank())
  else
    res <- res + theme(axis.ticks.x = element_line(colour = "grey50"))
  
   res <-  res + theme(axis.line = element_line(color = axis.line.color))
  
  res
}


theme_tracks_sunset <- function(bg = "#fffedb", alpha = 1, ...){
  res <- theme_clear(grid.x.major = FALSE, ...)
  attr(res, "track.plot.color") <- sapply(bg, scales::alpha, alpha)
  attr(res, "track.bg.color") <- bg
  attr(res, "label.text.color") <- "white"
  attr(res, "label.bg.fill") <- "#a52a2a"
  res
}


theme_tracks_fancy <- function(bg = c("white", "#F2C545"), alpha = 0.3,
                              label.bg.fill = c("gray80", "darkblue"),
                              label.text.color = "white"){
  res <- theme_clear(grid.x.major = FALSE)
  attr(res, "track.plot.color") <- sapply(bg, scales::alpha, alpha)
  attr(res, "label.bg.fill") <- label.bg.fill
  attr(res, "label.text.color") <- label.text.color
  res
}
