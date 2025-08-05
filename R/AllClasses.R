if (!isClass("unit")) setOldClass("unit")
if (!isClass("simpleUnit")) setOldClass(c("simpleUnit", "unit"))
if (!isClass("gtable")) setOldClass("gtable")
if (!isClass("theme")) setOldClass("theme")
if (!isClass("gTree")) setOldClass("gTree")
if (!isClass("grob")) setOldClass("grob")
setClassUnion("theme_OR_NULL", c("theme", "NULL"))
setClassUnion("numericORunit", c("numeric", "unit"))
setClassUnion("numeric_OR_NULL", c("numeric", "NULL"))

setClassUnion("GRanges_OR_NULL", c("GRanges", "NULL"))

setClassUnion("TxDbOREnsDb", c("TxDb", "EnsDb"))
## setClassUnion("GRangesORANY", c("GRanges", "ANY"))
## setClassUnion("GRangesORBasicFilterORlistORNULL",
setClassUnion("GRanges_OR_BasicFilter_OR_list_OR_NULL",
              c("GRanges", "AnnotationFilter", "AnnotationFilterList",
                "formula", "list", "NULL"))
setClassUnion("BasicFilterORlist",
              c("AnnotationFilter", "AnnotationFilterList", "formula", "list"))

setOldClass(c("ggplot2::ggplot", "gg", "ggplot"))
setClassUnion("gg_OR_NULL", c("gg", "NULL"))

setOldClass("grob")
setOldClass("trellis")
setOldClass("lattice")


