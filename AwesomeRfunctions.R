#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#                                                                     #
#                        AWESOME R FUNCTIONS                          #
#                                                                     #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# source can be used to load the functions
# myurl = "https://raw.githubusercontent.com/BavoDC/AwesomeRfunctions/master/AwesomeRfunctions.R"
# z     = tempfile()
# download.file(myurl, z, mode = "wb")
# source(z)

# the functions in this document are copied and/or modified functions found on
# stackoverflow or self-written functions

#### 1. Public functions ####

#### 1.1 Functions related to working directory ####
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing = FALSE, head = FALSE, n = 5, UnitsObj = "Mb",
                         DigitsSize = 3,
                         ...) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  ObjSize <- function(x) format(object.size(x), units = "Mb", digits = DigitsSize, quote = F)
  names <- ls(pos = pos, pattern = pattern, ...)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size <- napply(names, ObjSize)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.dim)
  names(out) <- c("Type", "Size", "Rows", "Columns")
  if (!missing(order.by)) {
    if(order.by != "Size") {
      out <- out[order(out[[order.by]], decreasing=decreasing), ]
    } else if(order.by == "Size") {
      out <- out[order(napply(names, object.size), decreasing=decreasing), ]
    }
  }
  if (head)
    out <- head(out, n)
  out
}

.ls.objects2 <- function (pos = 1, pattern, order.by,
                          decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    format(utils::object.size(x), units = "auto") })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Length/Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

# shorthand
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

RmAll <- function() rm(list = ls(pos = 1), pos = 1)

RmJunk <- function(){
  if (length(ls(pos = 1)) == 0)
    stop("Nothing to be found in your environment.")
  Fntions = .ls.objects(order.by = "Type")
  Fntions = rownames(Fntions)[Fntions$Type == "function"]
  rm(list = ls(pos = 1)[!ls(pos = 1) %in% Fntions], pos = 1)
  cat("#", rep("-", 18), "#\n")
  cat("# Your environment has been cleaned!  #\n")
  cat("#", rep("-", 18), "#\n")
  invisible(gc())
}

RmType <- function(Types, AdjOpenMessage = F, Keep = NULL,
                   GarbColl = T) {
  if(!is.character(Types))
    stop("Type has to be of type character.")
  ObjEnv  = .ls.objects()
  if(!any(ObjEnv$Type %in% Types))
    warning(paste0("No objects of class ", Types, " found."))
  
  RmObj   = rownames(ObjEnv[ObjEnv$Type %in% Types, ])
  if(!is.null(Keep))
    RmObj = RmObj[!RmObj %in% Keep]
  if("function" %in% Types & !AdjOpenMessage)
    warning("Start and end message R are not adjusted !!!")
  if(AdjOpenMessage)
    AdjOpenR()
  rm(list = ls(pos = 1)[ls(pos = 1) %in% RmObj], pos = 1)
  if(GarbColl) {
    cat("\n\nCollecting garbage\n\n")
    invisible(gc())
  }
}


#### 1.2 Making life easier ####
rm.NArows <- function(data){
  if (!is.data.frame(data))
    stop("Only objects of type dataframe are allowed!")
  data[!apply(data, 1, function(x) {
    all(is.na(x))
  }), ]
}

rm.NAcols <- function(data){
  if (!is.data.frame(data))
    stop("Only objects of type dataframe are allowed!")
  data[, !apply(data, 2, function(x) {
    all(is.na(x))
  })]
}

allNA <- function(x) {
  if (!is.null(dim(x)))
    stop("Only vectors allowed")
  all(is.na(x))
}

AnyAllNArows <- function(x) any(apply(x, 1, allNA))

sumNA <- function(x) sum(is.na(x))

ListToVector <- function(x, KeepNULL = T) {
  if (!is.list(x))
    stop("Function is only to be used with objects of type list!")
  if (KeepNULL & any(unlist(lapply(x, is.null))))
    x[unlist(lapply(x, is.null))] = NA
  unname(unlist(x))
}

NrUnique <- function(x, na.rm = T) {
  if (na.rm)
    length(unique(na.omit(x)))
  else
    length(unique(x))
}

ShowAllDecs <- function(x) sprintf("%.54f", x)
SeqLength <- function(x) seq_len(length(x))

is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))

rmNullObs <- function(x) {
  x <- Filter(Negate(is.NullOb), x)
  lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
}

sumNA <- function(x) sum(is.na(x))

AdjOpenR <- function(Begin = cat("\n\nHello world!\n\n"), 
                     End = cat("\n\nGoodbye!\n\n")) {
  .First <<- function() Begin
  .Last   <<- function() End
}

ObjectSize <- function(x) print(object.size(x), unit = "Mb")


WP <-
  function(from.clip = TRUE, quotes = TRUE, copy2clip = interactive()) {
    if (!from.clip) {
      message("Please enter the path:\n\n")
      path <- readline()
    } else {
      path <- readClipboard()
    }
    z <- chartr("\\", "/", path)
    if (quotes) {
      x <- paste0("\"", z, "\"")
    } else {
      x <- z
    }
    if(copy2clip){
      writeClipboard(x)
    }
    return(z)
  }

ConvertPath <-
  function(from.clip = TRUE, quotes = TRUE, copy2clip = interactive()) {
    if (!from.clip) {
      message("Please enter the path:\n\n")
      path <- readline()
    } else {
      path <- readClipboard()
    }
    z <- chartr("\\", "/", path)
    if (quotes) {
      x <- paste0("\"", z, "\"")
    } else {
      x <- z
    }
    if(copy2clip){
      writeClipboard(x)
    }
    return(z)
  }


LibraryM <- function(...) {
  libs = as.list(substitute(list(...)))[-1L]
  if(!is.character(libs)) {
    if(length(libs[[1]]) > 1)
      libs = sapply(substitute(libs)[-1], as.character)
    else
      libs = as.character(substitute(libs))
  }
  InstPkgs = installed.packages()
  Inst = all(libs %in% InstPkgs)
  if(!Inst)
    lapply(libs[!libs %in% InstPkgs], install.packages)
  lapply(libs, library, character.only = T)
}

is.formula <- function(x) inherits(x,"formula")

FactorToNumeric <- function(x) {
  if(!is.factor(x))
    stop("Provide variable of type factor.")
  as.numeric(as.character(x))
}

hline <- function(x, ...) abline(h = x, ...)
vline <- function(x, ...) abline(v = x, ...)

ModelMatrixKeepNA <- function(myformula, df) {
  if(!is.formula(myformula))
    stop("Has to be of type formula.")
  if(!is.data.frame(df))
    stop("Has to be of type data.frame")
  model.matrix(myformula, model.frame(myformula, df, na.action = function(x) x))
}

FindFile <- function(x, wd = getwd(), ...)
  list.files(wd, full.names = T, ...)[grepl(x, list.files(wd, ...), fixed = T)]

FixProfile <- function()
  source('~/.Rprofile')

GetRCodeWeb <- function(url, NameFile = "Code", FileExt = ".R",
                        pathFile = getwd()) {
  if(!"rvest" %in% names(sessionInfo()$otherPkgs))
    library(rvest)
  Error = tryCatch(read_html(url), error = function(e) T)
  if(is.logical(Error))
    stop("Unable to use read_html() on provided url.")
  
  iframe_link = url
  code <- iframe_link %>% 
    read_html() %>% 
    html_nodes("pre") %>% 
    html_text()
  
  code <- iframe_link %>% 
    read_html() %>% 
    html_nodes("pre.r") %>% 
    html_text()
  
  writeLines(code, con = paste0(pathFile, "/", NameFile, FileExt))
}

name.df <- function(x, df, ignore.case = T){
  if(!is.character(x)) 
    stop("Must be a character variable")
  names(df)[grepl(x,names(df),ignore.case = ignore.case)]
}

# Combine elements in list to dataframe
rbindList = function(.list, VarNamesOnce = F){
  if (!inherits(.list, "list"))
    stop("Object must be of type list")
  if (length(unique(lapply(.list, ncol))) != 1)
    stop("The number of columns of the different objects are not the same.")
  VarNames = NULL
  if (!VarNamesOnce)
    VarNames = rep(names(.list), lapply(.list, function(x)
      if (is.null(dim(x)))
        length(x)
      else
        nrow(x)))
  else
    for (i in names(.list)) {
      VarNames = c(VarNames, i, rep(NA, if(is.null(dim(.list[[i]]))) length(.list[[i]]) - 1 else nrow(.list[[i]]) - 1))
    }
  if (is.null(dim(.list[[1]])))
    CombinedList = do.call("c", .list)
  else
    CombinedList = do.call("rbind", .list)
  if (is.null(dim(.list[[1]]))) {
    Res = cbind.data.frame(Variable = VarNames, CombinedList, row.names = NULL)
  } else{
    Res = if (all(unlist(lapply(.list, nrow)) == 1))
      cbind.data.frame(
        Variable = VarNames,
        Level = unname(unlist(lapply(.list, function(x)
          rownames(x)))),
        CombinedList,
        row.names = NULL
      )
    else
      cbind.data.frame(Variable = VarNames,
                       Level = unname(unlist(lapply(.list, function(x)
                         rownames(x)))),
                       CombinedList)
    rownames(Res) = NULL
  }
  return(Res)
}

Prev <- function(x) {
  if(is.factor(x))
    x = FactorToNumeric(x)
  sum(x) / length(x)
}


#### 1.3 Functions for descriptive analysis ####

#### 1.3.1 New functions ####
print.descr <- function(x, ...) {
  print(x$Descriptives, row.names = F)
}
DescrContinuous <- function(x, Type = c("MeanSD", "MedianRange", "MedianIQR", "MedianAll"), 
                            digits = options()$digits) {
  if(!is.numeric(x))
    stop("Only numeric variables are allowed.")
  if(anyNA(x))
    warning(paste0(sumNA(x), " missing observations were detected and removed."))
  Q1 <- function(x, ...) quantile(x, 0.25, ...)
  Q3 <- function(x, ...) quantile(x, 0.75, ...)
  Type = match.arg(Type)
  Res = switch(Type,
               MeanSD = paste0(round(mean(x, na.rm = T), digits),
                               " (", round(sd(x, na.rm = T), digits), ")"),
               MedianRange = paste0(round(median(x, na.rm = T), digits),
                                    " (",
                                    round(range(x, na.rm = T)[1], digits),
                                    " to ",
                                    round(range(x, na.rm = T)[2], digits),
                                    ")"),
               MedianIQR = paste0(
                 round(median(x, na.rm = T), digits),
                 " (",
                 round(quantile(x, 0.25, na.rm = T), digits),
                 " to ",
                 round(quantile(x, 0.75, na.rm = T), digits),
                 ")"
               ),
               MedianAll = do.call("cbind", lapply(list(median, min, max, Q1, Q3), function(f) f(x, na.rm = T)))
               )
  Res = data.frame(Res, row.names = NULL)
  colnames(Res) = switch(Type,
                         MeanSD = "Mean (SD)",
                         MedianRange = "Median (min to max)",
                         MedianIQR = "Median (Q1 to Q3)",
                         MedianAll = c("Median", "Min", "Max", "Q1", "Q3"))
  Res = list(Descriptives = Res)
  class(Res) = "descr"
  Res
}

DescrCat <- function(x, Proportion = T, digits = options()$digits, ...) {
  Res           = cbind(table(x, ...), round(prop.table(table(x, ...)), digits))
  colnames(Res) = c("Frequency", "Percentage")
  if(any(is.na(rownames(Res))))
    rownames(Res)[is.na(rownames(Res))] <- "Missing"
  rNames = rownames(Res)
  if(!Proportion) {
    Res = data.frame("N (%)" = paste0(Res[, 1], " (", Res[, 2] * 100, "%)"),
                     check.names = F)
    rownames(Res) = rNames
  }
  Res = list(Descriptives = Res)
  class(Res) = "descr"
  Res
}

DescrContBy <- function(Var, By, Df, Type = c("MeanSD", "MedianRange", "MedianIQR", "MedianAll"),
                        AddMargins = F, digits = options()$digits) {
  Argz = as.list(match.call())[-1]
  if(!is.data.frame(Df))
    stop("Must be of type data.frame")
  
  Df$var = eval(Argz$Var, Df)
  Df$by  = eval(Argz$By, Df)
  Res = plyr::ddply(Df, .(by), function(x) DescrContinuous(x$var, Type = Type, digits = digits)$Descriptives)
  if(AddMargins) {
    AddRes = DescrContinuous(Df$var, Type = Type, digits = digits)$Descriptives
    Res = rbind.data.frame(Res,
                           data.frame(by = "Total", AddRes, check.names = F))
  }
  colnames(Res)[1] = as.character(substitute(By))
  Res = list(Descriptives = Res)
  class(Res) = "descr"
  Res
}

DescrCatBy <- function(Var, By, Df, Percentage = c("none", "row", "col", "all"), AppendPerc = T,
                       AddMargins = T, digits = options()$digits, ...) {
  Argz = as.list(match.call())[-1]
  if(!is.data.frame(Df))
    stop("Must be of type data.frame")
  Percentage = match.arg(Percentage)
  if(!is.logical(AppendPerc))
    stop("Argument AppendPerc has to be of type logical.")
  if(!is.logical(AddMargins))
    stop("Argument Addmargins has to be of type logical.")
  
  Df$var = eval(Argz$Var, Df)
  Df$by  = eval(Argz$By, Df)
  Tab = table(Df$var, Df$by, ...)
  if(Percentage != "none") {
    Tab = 
      if(Percentage == "row") {
        Rtot   = rowSums(Tab)
        Rperc  = round(sweep(Tab, 1, Rtot, FUN = "/"), digits)
        NewTab = 
          if(AppendPerc){
            Mat = matrix(paste0(Tab, " (", Rperc, "%)"), nrow(Tab), ncol(Tab))
            colnames(Mat) = colnames(Tab)
            rownames(Mat) = rownames(Tab)
            Mat
          } else {
            Mat = cbind(Tab, Rperc)
            colnames(Mat) = paste(rep(c("Frequency", "Percentage"), each = ncol(Tab)), colnames(Tab))
            Mat
          }
        if(AddMargins)
          NewTab = cbind.data.frame(NewTab, "Total"= Rtot)
        NewTab
      } else if(Percentage == "col") {
        Ctot   = colSums(Tab)
        Cperc  = round(sweep(Tab, 2, Ctot, FUN = "/"), digits)
        NewTab = 
          if(AppendPerc){
            Mat = matrix(paste0(Tab, " (", Cperc, "%)"), nrow(Tab), ncol(Tab))
            colnames(Mat) = colnames(Tab)
            rownames(Mat) = rownames(Tab)
            Mat
          } else {
            Mat = rbind(Tab, Cperc)
            rownames(Mat) = paste(rep(c("Frequency", "Percentage"), each = nrow(Tab)), rownames(Tab))
            Mat
          }
        if(AddMargins)
          NewTab = rbind.data.frame(NewTab, "Total"= Ctot, stringsAsFactors = F)
        NewTab
      } else {
        PercTab = round(Tab / sum(Tab), digits)
        Mat = as.data.frame(matrix(paste0(Tab, " (", PercTab, "%)"), nrow(Tab), ncol(Tab)),
                            stringsAsFactors = F)
        colnames(Mat) = colnames(Tab)
        rownames(Mat) = rownames(Tab)
        NewTab = 
          if (AddMargins) {
            NewTab = rbind.data.frame(Mat, "Total" = colSums(Tab))
            NewTab = cbind.data.frame(NewTab, "Total" = c(rowSums(Tab), sum(Tab)))
            NewTab
          } else {
            Mat
          }
        NewTab
      }
  }
  Tab
}


#### 1.4 Fixes ####

#### 1.4.1 Kable and htmlTable ####
view_kable <- function(x, viewHtml = F,...){
  if(!any(grepl("knitr", names(sessionInfo()$otherPkgs)))) require(knitr, quietly = T)
  tab <- paste(capture.output(kable(x, ...)), collapse = '\n')
  tf <- tempfile(fileext = ".html")
  writeLines(tab, tf)
  if(!viewHtml) rstudioapi::viewer(tf) else opendir(tf)
}

view_htmlTable <- function(x, viewHtml = F,...){
  if(!any(grepl("htmlTable", names(sessionInfo()$otherPkgs)))) require(htmlTable, quietly = T)
  tab <- paste(capture.output(htmlTable(x, ...)), collapse = '\n')
  tf <- tempfile(fileext = ".html")
  writeLines(tab, tf)
  rstudioapi::viewer(tf)
  if(!viewHtml) rstudioapi::viewer(tf) else shell(paste("start chrome.exe ", tf))
}

#### 1.5 Calculating C-index ####
# C++ function can be given on request
CindexOld <- function(p, y, data)
{
  if(missing(data))
    if(!is.vector(p) | !is.vector(y)) stop("p and y have to be vectors if dataframe is not provided.")
  Argz = as.list(match.call())[-1]
  if(!missing(data)){
    p = eval(Argz$p, data)
    y = eval(Argz$y, data)
  }
  Data = cbind(y, p)
  ones = Data[Data[,1] == 1,]
  zeros = Data[Data[,1] == 0,]
  conc = matrix(0, dim(zeros)[1], dim(ones)[1])
  disc = matrix(0, dim(zeros)[1], dim(ones)[1])
  ties = matrix(0, dim(zeros)[1], dim(ones)[1])
  for (j in 1:dim(zeros)[1])
  {
    for (i in 1:dim(ones)[1])
    {
      if (ones[i,2]>zeros[j,2])
      {conc[j,i]=1}
      else if (ones[i,2]<zeros[j,2])
      {disc[j,i]=1}
      else if (ones[i,2]==zeros[j,2])
      {ties[j,i]=1}
    }
  }
  Pairs = dim(zeros)[1] * dim(ones)[1]
  PercentConcordance = (sum(conc) / Pairs) * 100
  PercentDiscordance = (sum(disc) / Pairs) * 100
  PercentTied = (sum(ties) / Pairs) * 100
  concord.measure = PercentConcordance / (PercentConcordance + PercentDiscordance + PercentTied)
  return(concord.measure)
}