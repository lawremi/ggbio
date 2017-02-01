setOldClass("options")
setOldClass("unit")
setOldClass("gtable")
setOldClass("theme")
setOldClass("gTree")
setOldClass("grob")
setClassUnion("theme_OR_NULL", c("theme", "NULL"))
setClassUnion("options_OR_NULL", c("options", "NULL"))
setClassUnion("numericORunit", c("numeric", "unit"))
setClassUnion("numeric_OR_NULL", c("numeric", "NULL"))

setClassUnion("GRanges_OR_NULL", c("GRanges", "NULL"))

setClassUnion("TxDbOREnsDb", c("TxDb", "EnsDb"))
## setClassUnion("GRangesORANY", c("GRanges", "ANY"))
setClassUnion("GRanges_OR_BasicFilter_OR_list_OR_NULL", c("GRanges", "BasicFilter", "list", "NULL"))
setClassUnion("BasicFilterORlist", c("BasicFilter", "list"))


setOldClass("ggplot")
setClassUnion("ggplot_OR_NULL", c("ggplot", "NULL"))
setOldClass(c("gg", "ggplot"))
setClassUnion("gg_OR_NULL", c("gg", "NULL"))

setOldClass("grob")
setOldClass("trellis")
setOldClass("lattice")


