---
layout: static
title: geom_arch
---




### Introduction
`geom_arch` is lower level API for creating arches for interval data,
such as *GRanges* object. It could be used for showing splicing events or any
defined interactions in linear view.

A special thanks to **Jennifer Change**, a PhD student in
Iowa state univeristy, who initialize the first visualization of arches in
**ggbio** and distributed most of her code into this package.

### Objects
  * *GRanges*
  * *data.frame* 
  
### Usage
  upcomming
  
### Examples




Let's generate some simulated interval data and store it as *GRanges* object.




Default use equal height to show the arches, for each interval, it's being
connect by two ends, so make sure if you want to connect two exons, you have to
get gaps intervals to show the links between exons. 




Facetting and aesthetics mapping are supported, make sure you put your
aesthetics mapping in constructor `aes()`, and those variables are not quoted.





