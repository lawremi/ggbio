## TODO: for RleList?
setGeneric("layout_stacked", function(data,...)
           standardGeneric("layout_stacked"))

setMethod("layout_stacked", "GRanges", 
          function(data,..., geom = c("rectangle","line", "area")){
            geom <- match.arg(geom)
            args <- as.list(match.call(expand.dots = TRUE)[-1])
            args <- args[!names(args) %in% c("data", "geom")]
            aes.lst <- unlist(lapply(args, function(x) class(eval(x)) == "uneval"))
            if(length(aes.lst)){
              idx <- which(aes.lst)
              aes.lst <- eval(args[[idx]])
            }else{
              aes.lst <- list()
            }
            df <- as.data.frame(data)
            if(geom == "rectangle"){
              args <- c(aes.lst, list(xmin = substitute(start),
                                      xmax = substitute(end),
                                      ymin = 0,
                                      ymax = 10))
              if(any(c("colour", "fill") %in% names(args))){
                if(!all(c("colour", "fill") %in% names(args))){
                  idx <- which(c("colour", "fill") %in% names(args))
                  known <- c("colour", "fill")[idx]
                  unknown <- c("colour", "fill")[-idx]
                  args[[unknown]] <- args[[known]]
                }
                return(geom_rect(data = df, do.call(aes, args)))
              }
              else
                return(geom_rect(data = df, do.call(aes, args), color = "black", fill = "black"))
            }
            
            if(geom == "area"){
              if("y" %in% names(aes.lst)){
                y.val <- eval(aes.lst$y, df)
                y.val.new <- rescale(y.val, to = c(0, 10))
                df.temp <- df
                df.temp[,as.character(aes.lst$y)] <- y.val.new
                df.temp$midpoint <- c(df.temp$start + df.temp$end)/2
              }
              args <- c(aes.lst, list(x = substitute(midpoint)))
              return(geom_area(data = df.temp, do.call(aes, args)))
            }
            
            if(geom == "line"){
              if("y" %in% names(aes.lst)){
                y.val <- eval(aes.lst$y, df)
                y.val.new <- rescale(y.val, to = c(0, 10))
                df.temp <- df
                df.temp[,as.character(aes.lst$y)] <- y.val.new
                df.temp$midpoint <- c(df.temp$start + df.temp$end)/2
              }
              args <- c(aes.lst, list(x = substitute(midpoint)))
              return(geom_line(data = df.temp, do.call(aes, args)))
            }
          })

