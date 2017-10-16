#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#                                                                     #
#                        AWESOME R FUNCTIONS                          #
#                                                                     #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# the functions in this document are copied and/or modified functions found on stackoverflow or self-written functions

#===========================#
# XLSX WITH MULTIPLE SHEETS #
#===========================#
save.xlsx <- function (path, ...)
{
  require(xlsx, quietly = TRUE)
  objects <- list(...)
  if(any(sapply(objects, class)=="list")) stop("Objects of type list are not permitted in this function.")
  fargs <- as.list(match.call(expand.dots = TRUE))
  objnames <- as.character(fargs)[-c(1, 2)]
  nobjects <- length(objects)
  for (i in 1:nobjects) {
    if (i == 1)
      write.xlsx(objects[[i]], path, sheetName = objnames[i], row.names = F)
    else write.xlsx(objects[[i]], path, sheetName = objnames[i],
                    append = TRUE, row.names = F)
  }
  print(paste("Workbook", path, "has", nobjects, "worksheets."))
}

# when object is of type list
save.excel.list <- function(.list, default = 'var1', path = getwd(),name.path=T,begin=1,end=31,row.names=F,
                            col.width=5.1E3){
  require("XLConnect",quietly = T)
  .name <- as.list(match.call())[2]
  if(isTRUE(name.path)){
    wb_name <- path
  }else{
    if(is.language(.name[[1]])) wb_name <- paste0(paste0(path, default, collapse = '/'), '.xlsx')
    if(is.symbol(.name[[1]])) wb_name <- paste0(paste0(path, as.character(.name), collapse = '/'), '.xlsx')
  }
  
  wb <- XLConnect::loadWorkbook(wb_name, create = TRUE)
  
  if(isTRUE(row.names)) them.names <- lapply(.list,function(x){c("",rownames(x))})
  
  # sheet names
  sheet.names <- substr(names(.list),begin,end)
  if(length(unique(sheet.names)) < length(sheet.names)){
    for(i in sheet.names){
      if(length(which(i == sheet.names))>1){
        NrSheets = which(i == sheet.names)
        SheetyName = paste(substr(i, 1, 30), 1:length(NrSheets), sep="")
        sheet.names[which(i == sheet.names)] = SheetyName
      }
    }
  }
  
  XLConnect::createSheet(wb, sheet.names)
  
  if(isTRUE(row.names)){
    XLConnect::writeWorksheet(wb,.list, sheet.names,header=T,rownames = them.names)
  }else{
    XLConnect::writeWorksheet(wb,.list, sheet.names,header=T)
  }
  
  for(i in sheet.names){
    if(!is.null(dim(ncol(.list[[which(i == sheet.names)]])))){
      nr.cols <- ncol(.list[[which(i == sheet.names)]])+1
    }else{nr.cols <- length(.list[[which(i == sheet.names)]])}
    XLConnect::setColumnWidth(wb, sheet=i,column = 1:nr.cols,width = col.width)
  }
  
  XLConnect::saveWorkbook(wb)
}

#====================================#
# Open all sheets excel file in list #
#====================================#
read_excel_allsheets <- function(filename) {
  if(!any(grepl("readxl",names(sessionInfo()$otherPkgs)))) require(readxl,quietly = T)
  sheets <- readxl::excel_sheets(filename)
  x <-    lapply(sheets, function(X){
    File = read_excel2(filename, sheet = X)
    if(any(class(File)=="tbl")) File = as.data.frame(File)
    File
  })
  names(x) <- sheets
  x
}

# Transform object to dataframe if tibble #Goddamnit
read_excel2 <- function(path, ...){
  if(!(any(grepl("readxl", names(sessionInfo()$otherPkgs))))) require(readxl, quietly = T)
  Df = read_excel(path, ...)
  if(any(class(Df)=="tbl")) Df = as.data.frame(Df)
  Df
}



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#    AWESOME FUNCTION TO GET OBJECTS WD     #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.dim)
  names(out) <- c("Type", "Size", "Rows", "Columns")
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


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# TOO LAZY TO USE FILE EXPLORER #
#       ~~ SIMSALABIM ~~        #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

opendir <- function(dir = getwd()){
  if (.Platform['OS.type'] == "windows"){
    shell.exec(dir)
  } else {
    system(paste(Sys.getenv("R_BROWSER"), dir))
  }
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#    Functions to make life easier    #
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#

rm.NArows <- function(data){
  if(!is.data.frame(data)) stop("Only objects of type dataframe are allowed!")
  data[!apply(data,1,function(x){all(is.na(x))}),]
}

rm.NAcols <- function(data){
  if(!is.data.frame(data)) stop("Only objects of type dataframe are allowed!")
  data[,!apply(data,2,function(x){all(is.na(x))})]
}

allNA <- function(x){
  if(!is.null(dim(x))) stop("Only vectors allowed")
  all(is.na(x))
}

AnyAllNArows <- function(x) any(apply(x, 1, allNA))

sumNA <- function(x) sum(is.na(x))

ListToVector <- function(x){
  if(!is.list(x)) stop("Function is only to be used with objects of type list!")
  unname(unlist(x))
}


# REMOVE NULL ENTRIES IN A LIST #


is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))

rmNullObs <- function(x) {
  x <- Filter(Negate(is.NullOb), x)
  lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
}

# Number of missing values #

sumNA <- function(x) sum(is.na(x))

# Clean environment
RmAll <- function() rm(list=ls(pos = 1), pos=1)
RmJunk <- function(){
  if(length(ls(pos = 1))==0) stop("Nothing to be found in your environment.")
  Fntions = .ls.objects(order.by = "Type")
  Fntions = rownames(Fntions)[Fntions$Type=="function"]
  rm(list=ls(pos = 1)[!ls(pos = 1)%in%Fntions], pos=1)
  cat("#",rep("-", 18),"#\n")
  cat("# Your environment has been cleaned!  #\n")
  cat("#",rep("-", 18),"#\n")
}



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
# FUNCTIONS FOR DESCRIPTIVE ANALYSES #
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
descr.cont <- function(x, digits = options()$digits) paste0(round(mean(x,na.rm = T), digits),
                                                            " (",round(sd(x,na.rm = T), digits),")")
descr.median <- function(x, digits = options()$digits){
  x   = as.numeric(x)
  Res = paste0(round(median(x,na.rm = T), digits),
               " (",round(range(x,na.rm = T)[1], digits),"-",
               round(range(x,na.rm = T)[2], digits),")")
  return(Res)
}

descr.medianIQR <- function(x, digits = options()$digits){
  x   = as.numeric(x)
  Res = paste0(round(median(x,na.rm = T), digits),
               " (",round(quantile(x, 0.25, na.rm = T), digits),"-",
               round(quantile(x, 0.75, na.rm = T), digits),")")
  return(Res)
}


descr.median2 <- function(x){
  x <- na.omit(as.numeric(x))
  df <- cbind.data.frame("Median"=median(x),"Minimum"=min(x),"Maximum"=max(x),"Q1"=quantile(x,0.25),"Q3"=quantile(x,0.75))
  rownames(df) <- ""
  df
}

descr.cat <- function(x, Proportion = T, ...){
  result <- cbind(table(x,...),round(prop.table(table(x,...)),3))
  colnames(result) <- c("Frequency","Percentage")
  if(any(is.na(rownames(result)))) rownames(result)[is.na(rownames(result))] <- "Missing"
  rNames = rownames(result)
  if(!Proportion){
    result = data.frame("N (%)" = paste(result[,1]," (",result[,2]*100,"%)", sep=""), check.names = F)
    rownames(result) = rNames
  }
  result
}

dcat.outc <- function(var,by.var,data,PercName = "Percentage", Merge = F, digits = 3,
                      AddMargins = F,
                      ...){
  argz <- as.list(match.call())[-1]
  if(!is.data.frame(data)) stop("Must be dataframe")
  AddArgz = as.list(substitute(list(...)))[-1L]
  
  data$var <- eval(argz$var,data)
  data$by.var <- eval(argz$by.var,data)
  results <- aggregate(data$var,list(data$by.var),descr.cat,...)
  x.names <- colnames(aggregate(data$var,list(data$by.var),table,...)$x)
  if(is.null(x.names)) x.names = "Var"
  colnames(results$x) <- c(x.names,paste(PercName,x.names))
  X <- results
  X2 <- cbind(X[[ncol(X)]])
  rownames(X2) <- X[,1]
  if(AddMargins){
    X2 = cbind(X2, addmargins(X2[,1:length(unique(data$var))], 2)[,length(unique(data$var)) + 1])
    colnames(X2)[ncol(X2)] = "Total"
  }
  if(Merge){
    n = if(length(AddArgz)==0) length(na.omit(unique(data$var)))
    else if(any(names(AddArgz)=="useNA") & AddArgz$useNA=="always") length(unique(data$var))
    Merged = list()
    for(i in 1:n) Merged[[i]] = data.frame(paste(X2[,i], " (", round(X2[,i+n],3)*100,"%)", sep=""))
    Merged = do.call("cbind", Merged)
    colnames(Merged) = x.names
    rownames(Merged) = X[,1]
    if(AddMargins) Merged = cbind(Merged, Total = X2[,ncol(X2)])
    X2 = Merged
  }
  X2
}

dmed.outc <- function(var, by.var, data, AddMargins = F, digits = 2){
  argz <- as.list(match.call())[-1]
  if(!is.data.frame(data)) stop("Must be dataframe")
  if(missing(data)) stop("Provide data frame")
  if(missing(var)) stop("Provide argument for var")
  if(missing(by.var)) stop("Provide argument for by.var")
  
  data$var <- eval(argz$var,data)
  data$by.var <- eval(argz$by.var,data)
  results <- aggregate(data$var,list(data$by.var),descr.median, digits = digits)
  X <- results
  X2 <- cbind(X[[ncol(X)]])
  rownames(X2) <- X[,1]; colnames(X2) <- "Median (Range)"
  if(AddMargins) X2 = rbind(X2, Total = descr.median(data$var, digits = digits))
  X2
}

dmed2.outc <- function(var,by.var,data){
  argz <- as.list(match.call())[-1]
  if(!is.data.frame(data)) stop("Must be dataframe")
  if(length(argz)!=3) stop("Provide all arguments of the function!")
  
  data$var <- eval(argz$var,data)
  data$by.var <- eval(argz$by.var,data)
  results <- aggregate(data$var,list(data$by.var),descr.median2)
  X <- results
  X2 <- cbind(X[[ncol(X)]])
  rownames(X2) <- X[,1]; colnames(X2) <- c("Median","Minimum","Maximum","Q1","Q3")
  X2
}

dcont.outc <- function(var,by.var,data){
  argz <- as.list(match.call())[-1]
  if(!is.data.frame(data)) stop("Must be dataframe")
  if(length(argz)!=3) stop("Provide all arguments of the function!")
  
  data$var <- eval(argz$var,data)
  data$by.var <- eval(argz$by.var,data)
  results <- aggregate(data$var,list(data$by.var),descr.cont)
  X <- results
  X2 <- cbind(X[[ncol(X)]])
  rownames(X2) <- X[,1]; colnames(X2) <- "Mean (Standard Deviation)"
  X2
}


# name in dataframe
name.df <- function(x,df,ignore.case=T){
  if(!is.character(x)) stop("Must be a character variable")
  names(df)[grepl(x,names(df),ignore.case = ignore.case)]
}

# Combine elements in list to dataframe
rbindList = function(.list, VarNamesOnce = F){
  if(!inherits(.list, "list")) stop("Object must be of type list")
  if(length(unique(lapply(.list, ncol)))!=1) stop("The number of columns of the different objects are not the same.")
  VarNames = NULL
  if(!VarNamesOnce) VarNames = rep(names(.list), lapply(.list, function(x) if(is.null(dim(x))) length(x) else nrow(x)))
  else for(i in names(.list)) VarNames = c(VarNames, i, rep(NA, nrow(.list[[i]]) - 1))
  if(is.null(dim(.list[[1]]))) CombinedList = do.call("c", .list)
  else CombinedList = do.call("rbind", .list)
  if(is.null(dim(.list[[1]]))){
    Res = cbind.data.frame(Variable = VarNames, CombinedList, row.names = NULL)
  }else{
    Res = if(all(unlist(lapply(.list, nrow))==1)) cbind.data.frame(Variable = VarNames,
                                                                   Level = unname(unlist(lapply(.list, function(x) rownames(x)))),
                                                                   CombinedList, row.names = NULL)
    else cbind.data.frame(Variable = VarNames, Level = unname(unlist(lapply(.list, function(x) rownames(x)))),
                          CombinedList)
    rownames(Res) = NULL
  }
  return(Res)
}

#@@@@@@@@@@@@@#
# PC COMMANDS #
#@@@@@@@@@@@@@#

pc.hibernate <- function() system('shutdown -h')
pc.shutdown <- function() system('shutdown -s')
pc.restart <- function() system('shutdown -r')
open.firefox <- function() system('open firefox.exe')
open.word <- function() system('open winword.exe')
open.excel <- function() system('open excel.exe')
close.firefox <- function() system('taskkill /F firefox.exe')
close.word <- function() system('taskkill /IM winword.exe')
close.excel <- function() system('taskkill /IM excel.exe')
close.drive <- function() system('taskkill /F /IM googledrivesync.exe')


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
# Correlation transformation #
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
corr.transf <- function(x) (1 / (sqrt(length(x) - 1))) * scale(x)

#@@@@@@@@@@@@@@@@@@@@@@@@#
# Fix for viewing tables #
#@@@@@@@@@@@@@@@@@@@@@@@@#
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

#%%%%%%%%%%%%%%%%%%%%%%%%#
# Calculating statistics #
#%%%%%%%%%%%%%%%%%%%%%%%%#

#~~ AUC ~~#
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













