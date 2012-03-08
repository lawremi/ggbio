---
layout: static
title: geom_rect
---




## Introduction
**geom_rect** is lower level API for creating rectangles for interval data,
such as *GRanges* object.

## Objects
  * *GRanges*
  * *data.frame* , just like ggplot2::geom_rect
## Usage
  upcomming
  
## Examples

Let's generate some simulated interval data and store it as *GRanges* object.




Default is use stat stepping, which laying out the intervals randomly and assign
those intervals different stepping levels as y axis to avoid overlapped
plotting, it's a very rough exploration as first step for some interval data.

![plot of chunk unnamed-chunk-2](http://i.imgur.com/nEe4h.png) 


Facetting and aesthetics mapping are supported, make sure you put your
aesthetics mapping in constructor **aes()**, and those variables are not quoted.

![plot of chunk unnamed-chunk-3](http://i.imgur.com/xiSXY.png) 


Stat "identity" allows you to specify a y value to use as y-axis instead of
default stepping level.

![plot of chunk unnamed-chunk-4](http://i.imgur.com/sPoUi.png) 


**Group** make sure grouped intervals are on the same levels when stat =
"stepping",  notice that it's could be possible that those
intervals assigned in the same group are overlapped with each other.

![plot of chunk unnamed-chunk-5](http://i.imgur.com/ivMRG.png) 


**group.selfish** force the grouped intervals to take unique stepping level,
  this is useful when you want to show the labels for each group as y axis, when
  it's disabled, the y-label will be automatically hided to avoid overlapped
  group labels as y axis.

![plot of chunk unnamed-chunk-6](http://i.imgur.com/yXDNr.png) 


