setOldClass("options")
setOldClass("unit")
setOldClass("gtable")
setOldClass("theme")
setOldClass("gTree")
setOldClass("grob")
setClassUnion("themeORNULL", c("theme", "NULL"))
setClassUnion("optionsORNULL", c("options", "NULL"))
setClassUnion("numericORunit", c("numeric", "unit"))



setOldClass("ggplot")
setClassUnion("ggplotORNULL", c("ggplot", "NULL"))
setOldClass("gg")
setClassUnion("ggORNULL", c("gg", "NULL"))

