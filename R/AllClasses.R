setOldClass("options")
setOldClass("unit")
setOldClass("gtable")
setOldClass("theme")
setOldClass("gTree")
setOldClass("grob")
setClassUnion("themeORNULL", c("theme", "NULL"))
setClassUnion("optionsORNULL", c("options", "NULL"))
setClassUnion("numericORunit", c("numeric", "unit"))
setClassUnion("numericORNULL", c("numeric", "NULL"))

setClassUnion("GRangesORNULL", c("GRanges", "NULL"))

setClassUnion("TxDbOREnsDb", c("TxDb", "EnsDb"))
## setClassUnion("GRangesORANY", c("GRanges", "ANY"))
setClassUnion("GRangesORBasicFilterORlistORNULL", c("GRanges", "BasicFilter", "list", "NULL"))
setClassUnion("BasicFilterORlist", c("BasicFilter", "list"))


setOldClass("ggplot")
setClassUnion("ggplotORNULL", c("ggplot", "NULL"))
setOldClass(c("gg", "ggplot"))
setClassUnion("ggORNULL", c("gg", "NULL"))

setOldClass("grob")
setOldClass("trellis")
setOldClass("lattice")


