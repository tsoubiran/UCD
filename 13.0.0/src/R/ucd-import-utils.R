##__________________________________________________________________________________________________
##
##
##
ucd.mkdir <- function(destdir, dnm=c("ucd", "unihan", "uca", "ivd", "security"), exec=F){
  ##
  mkdir <- function(nm, exec){
    ##
    cat( nm )
    ##
    if(
      !dir.exists(nm)
    ){
      # cat("", nm, "")
      ##
      if( exec ){
        # cat("", nm, "")
        ##
        if( dir.create(nm) ){
          cat(" created", "\n")
          # T
          "C"
        }else{
          ##
          stop("could not be create", nm, " file")
          ##
          F
        }
      }else{
        cat(" does not exists", "\n")
        # T
        NA_character_
      }
    }else{
      cat(" already exists", "\n")
      # T
      "E"
    }
  }
  ##
  if( missing(destdir)) stop("missing destdir")
  ##
  status <- character(0L)
  ## 
  for( i in 1:length(dnm) ){
    ##
    rv <- mkdir(
        paste( destdir, dnm[i], sep="/")
        , exec=exec
    )
    ##
    status <- c(status, rv)
    
  }
  ##
  names(status) <- dnm
  ##
  status
}
##
##
##
ucd.getzip <- function(
    ucd.version ##
  , destdir
  , db = "UCD"
  , ucd.url = "https://www.unicode.org/Public"
  , zipfnm = c( "UCD.zip", "Unihan.zip", "uts39-data-<version/>.zip")
  ##
  , exec = F
  , force = F
  ##
  , verbose = 0
){
  ##
  if( missing(ucd.version)) stop("missing ucd.version")
  ##
  if( missing(destdir)) stop("missing destdir")
  ##
  if(!dir.exists(destdir)){
    stop("destdir ", destdir, "does not exist")
  }
  ##
  db = match.arg(db, dbnm <- c("UCD", "Unihan", "security"))
  ##
  zipfnm <- zipfnm[match(db, dbnm)]
  ##
  if( isSec <- db==dbnm[3] ) zipfnm <- sub("<version/>", ucd.version, zipfnm)
  ##
  url <- if( !isSec ) paste(ucd.url, "zipped", ucd.version, zipfnm, sep="/") else paste(ucd.url, dbnm[3], ucd.version, zipfnm, sep="/")
  ## 
  destfile <- paste(destdir, zipfnm, sep="/")
  ##
  if( verbose>0 ) cat(
      ##
      "url:"
      ##
      , url
      ##
      , "destination:"
      ## 
      , destfile 
      , "\n"
    )
  ##
  if( file.exists(destfile) && !force){
    ##
    msg <- paste("file", zipfnm, "has already been downloaded. Use force=T to download it again.\n")
    ##
    if( verbose>0 ) cat(msg) else warning(msg)
    ##
    return()
  }
  ##
  if( exec ){
    ##
    download.file(
      url 
      ##
      , destfile 
    )
  }else -1
}

##
ucd.unzip <- function(
  basedir
  , ucd.version ## 
  , db = "UCD"
  , exec=F
  , zipfnm = c( "UCD.zip", "Unihan.zip", "uts39-data-<version/>.zip" )
  ## 
  , overwrite=F
  ##
  , verbose = 0
  ,...
){
  ##
  db = match.arg(db, dbnm <- c("UCD", "Unihan", "security"))
  ##
  zipfnm <- zipfnm[match(db, dbnm)]
  ##
  if( isSec <- db==dbnm[3] ) zipfnm <- sub("<version/>", ucd.version, zipfnm)
  ##
  zipfile <- paste(basedir, zipfnm, sep="/")
  ##
  cat(db, "\n")
  ##
  if( isSec & missing(ucd.version)) stop("missing ucd.version")
  ##
  if( file.exists(zipfile) ){
    ##
    exdir <- paste(basedir, tolower(db), sep="/")
    ##
    cat("zip:", zipfile, "exdir:", exdir, "\n")
    ##
    if( !exec ) return(NA_character_)
    ##
    if(!dir.exists(exdir)){
      ##
      stop("exdir ", exdir, "does not exist")
    }
    ##
    unzip(
      zipfile
      , exdir = exdir
      , overwrite=overwrite
      ,...
    )
  }else{
    stop("zipfile ", zipfile, "not found")
  }
}
##
## 
##
extractFileUrl <- extractUrlFromHtmlFile <- function(html, fext =".txt"){
  ## 
  fnm <- XML::getNodeSet(
    html
    # , paste0("//a[contains(@href,'", fext, "') ]/@href")
    ##
    , paste0(
      "//a["
      , paste( sprintf("contains(@href,'%s')", fext), collapse=" or " )
      , "]/@href"
    )
  )
  ##
  if( length(fnm) ) {
    if( all(sapply(fnm,length)==1 ) ) unlist(fnm)
    else{
      print(fnm)
      stop("xpath result is not a vector")
    }
  }else{
    stop("empty node set")
  }
}
##
getFilesFromHtmlPage <- function(
  url
  , destdir
  , ext = ".txt"
  ## 
  , exec = F
  , force = F
  ##
  , verbose = 0
){
  ##
  r <- RCurl::getURL(url, verbose=verbose>2)
  ## parse html
  html <- XML::htmlParse(r)
  ##
  fnm <- extractUrlFromHtmlFile(html, ext)
  ##
  rv <- lapply(
    fnm 
    , function(nm, url, destdir, exec){
      ##
      cat( furl <- paste(url, nm, sep="/"), destfile <- paste(destdir, nm, sep="/"))
      if( 
        exec & !( 
          x <- file.exists(destfile) & !force 
        ) 
      ){
        cat( "\n")
        download.file(
          furl, destfile
        )
      }else{
        if( x ){cat(destfile, "already exists\n")}
        -1
      }
    } 
    ##
    , url = url
    , destdir = destdir
    , exec = exec
  )
  ##
  names(rv) <- fnm
  ##
  rv
}
##
##
##
getIVD <- function(
  basedir
  # , latest = T
  , dt.idx = length(dt)
  , url = "https://unicode.org/ivd/data"
  , ext = formals(getFilesFromHtmlPage)$ext
  ##
  , exec = F
  , force = F
  ##
  , verbose = 0
){
  ##
  if( 
    !dir.exists(destdir <- paste(basedir, "ivd", sep="/")) 
  )stop("output dir ", destdir, " does not exist")
  ##
  # latest.url <- if( latest ){
  p <- RCurl::getURL(
    ## ¡¡¡ trailing '/' !!!
    if(!grepl("\\/$", url) ) paste0(url, "/") else url
    ##
    , verbose=verbose>2
  )
  ## parse html
  html <- XML::htmlParse(p)
  ## 
  href <- XML::xpathSApply(
    html
    ##
    , paste0(
      "//td/a" ## /@href"
    )
    , XML::xmlGetAttr ## XML::xmlAttrs ## xmlGetAttr		
    , "href"
  )
  ##
  dt <- sort( as.Date(href, format="%Y-%m-%d/") )
  ##
  if( !exec | verbose>0 ){
    cat("IVD repertories:", as.character( dt ), sep="\n")
  }
  ##
  if( !exec ) return( dt ) 
  ##
  dt <- dt[dt.idx]
  ##
  destdir = paste(basedir, "ivd", as.character(dt), sep="/")
  ##
  dir.create(destdir)
  ##
  ivd.url <- paste0(url, "/", dt, "/")
  ##
  cat("downloading ", ivd.url, "/*{", paste(ext, sep=","), "}\n\n", sep="")
  # }else{
  #   destdir <- basedir
  #   url
  # }
  ##
  getFilesFromHtmlPage(
    ivd.url
    , destdir = destdir
    , ext=ext, exec=exec, force=force, verbose=verbose
  )
}
##
##
##
getUCD <-function(
  destdir
  , ucd.version
  , ucd.url = "https://www.unicode.org/Public"
  , ivd.url = formals(getIVD)$url
  , force = F
  , verbose = 1
){
  ##
  if( verbose>0 ) cat("\nmkdir ", destdir, "…\n", sep ="")
  ## 
  ucd.mkdir(
    destdir <- destdir ## 
    , exec = T
  )
  ##
  if( verbose>0 ) cat("\nget UCD zip…\n")
  ##
  ucd.getzip(
    ucd.version = ucd.version
    , destdir = destdir
    , db = "UCD" 
    , exec = T
    , force = force
    , verbose = verbose
  )
  ##
  if( verbose>0 ) cat("\nunzip UCD zip…\n")
  ##
  ucd.unzip(
    destdir
    ,  ucd.version
    , db = "UCD" 
    , exec = T
    # , verbose = verbose
  )
  ##
  if( verbose>0 ) cat("\nget Unihan zip…\n")
  ##
  ucd.getzip(
    ucd.version = ucd.version
    , destdir = destdir
    , db = "Unihan" 
    , exec = T
    , force = force
    , verbose = verbose
  )
  ##
  if( verbose>0 ) cat("\nunzip Unihan zip…\n")
  ##
  ucd.unzip(
    destdir
    ,  ucd.version
    , db ="Unihan" 
    , exec = T
    # , verbose = verbose
  )
  ##
  if( verbose>0 ) cat("\nget security zip…\n")
  ##
  ucd.getzip(
    ucd.version = ucd.version
    , destdir = destdir
    , db = "security" 
    , exec = T
    , force = force
    , verbose = verbose
  )
  ##
  if( verbose>0 ) cat("\nunzip security zip…\n")
  ##
  ucd.unzip(
    destdir
    ,  ucd.version
    , db = "security" 
    , exec = T
    # , verbose = verbose
  )
  ##
  ##
  ##
  if( verbose>0 ) cat("\nget UCA files…\n\n")
  ##
  getFilesFromHtmlPage(
    url = paste(ucd.url, "UCA", ucd.version, "/" , sep="/" )
    , destdir = paste(destdir, "uca",  sep="/" ) 
    , exec = T
    , force = force
  )
  ##
  ##
  if( verbose>0 ) cat("\nget IVD files…\n\n")
  ##
  rv <- getIVD(
    destdir
    , url = ivd.url
    , ext = c(".txt") ## , ".pdf"
    , exec = T
    # , force = force
    , verbose = verbose
  )
  ##
  invisible()
}
##__________________________________________________________________________________________________

##__________________________________________________________________________________________________
##
##
## ¡¡¡ FIXME: leading blanks : cf. ucd.prop$propname !!!
##__________________________________________________________________________________________________
##
ucdCommParse <- function(x, j = c(2,4)){
  ##
  m <- stringi::stri_match(x,regex="([^#]*)(#\\s*(.+))?")[,j]
  ## blanks
  m <- apply(m, 2, function(x) gsub("(^\\s+)|(\\s+$)", "", x) )
}
##
##
##
ucdCsvParse <- function(x, cnm, sep=";", comm.parse = T, penult.drop = F){
  ##
  txt <- ucd.getVal(x)
  ##
  h <- htxt(txt)
  ##
  if( comm.parse ){
    ##
    rv <- as.data.frame(ucdCommParse(txt), stringsAsFactors = F) 
    ##
    txt <- rv[,1]
  }
  ##
  rv <- read.table(textConnection(txt), sep=sep, stringsAsFactors = F,comment.char ="")
  ##
  if(penult.drop){rv <- rv[-ncol(rv)]}
  ##
  rv <- structure(
    if( comm.parse){
      data.frame(rv, comm)
    } else rv
    , htxt = h
  )
  ##
  if(!missing(cnm)) setColnames(
    rv, cnm 
  ) else rv
}

##
##
##
##
ucdTblParse <- function(x, cnm = NULL, ncp = 1L, cp.drop=F, hex.drop = T, sep=";", comm.parse=T, penult.drop = F){
  ## 
  if(missing(cnm)) stop("missing cnm")
  ##
  lab <- NULL
  ##
  if( length( nm <- names(cnm) ) ){
    lab <- cnm
    cnm <- nm
  }
  ## ¿¿ utile ?? ncp=1L donne le même résultat en fait…
  if( ncp==0L ){
    ##
    txt <- ucd.getVal(x)
    ##
    rv <- ucdCommParse(txt)
    ##
    cp <- strToCp(rv[,1], drop=cp.drop)
    ##
    rv <- structure(
        setColnames(
          data.frame(cp, rv[,2], stringsAsFactors = F)
          , cnm
        )
        , htxt=htxt(txt)
      )
    ##
    return(
      if( length( lab ) ){
        attr(rv, "variable.labels") <- lab
        rv
      }else rv
    )
  }
  ## 
  ml <- ifelse(cp.drop, 1L, 2L) 
  ##
  rv <- ucdCsvParse(
    x
    , sep = sep
    , comm.parse = comm.parse
    , penult.drop = penult.drop
  )
  ##
  cp <- data.frame(strToCp(
    rv[,1]
    , cnm = cnm[1:(ml)]
    , drop=cp.drop
  ))
  ##
  cp <- if( ncp==1L ) cp else if(ncp==2L){
    ##
    setColnames(
      cbind(cp, strToCp(rv[,2], drop=cp.drop))
      , cnm[1:(2*ml)] 
    )
  }else if( ncp==3L ){
    ##
    setColnames(
      cbind(cp, strToCp(rv[,2], drop=cp.drop), strToCp(rv[,3], drop=cp.drop))
      , cnm[1:(3*ml)] 
    )
  }else stop("ncp>3: ", ncp)
  ##
  ##
  rv <- if( comm.parse ){
    ##
    i <- ( !penult.drop ):0
    ## ¡¡¡ ctrl !!!
    # print(i)
    xnc <- (ncol(rv) - (ncp + length(i)) ) 
    # print(xnc)
    i <- (xnc+i[1]):0
    ##
    structure(
      data.frame(
        cp
        ##, setColnames(rv[,1:nc], prefix="hexchar")
        , setColnames(
          rv[, ncol(rv) - ( i ) , drop = F]
          , cnm[ length(cnm) - i ]
        )
        , stringsAsFactors = F
      )
      , htxt=htxt(rv)
    ) 
  }else{
    ##
    i <- (ncol(rv)-ncp -1):0
    ## ¡¡¡ ctrl !!!
    xnc <- (ncol(rv) - (ncp + length(i)) ) 
    ##
    i <- (xnc+i[1]):0
    ##
    structure( ##  ¡¡¡ todo:trim, cf. ucd.blk !!!
      data.frame(
        cp
        ##, setColnames(rv[,1:nc], prefix="hexchar")
        , setColnames(
          rv[
            , ncol(rv) - ( i ) ##   ## ¡¡¡ ctrl !!!
            , drop=F
          ]
          , cnm[ length(cnm) - i ]
        )
        , stringsAsFactors = F
      )
      , htxt=htxt(rv)
    ) 
  }
  ##
  if( length( lab ) ){
    attr(rv, "variable.labels") <- lab
    rv
  }else rv
}

##
##
##
read.ucdTbl <- function(rpath, ucd.path,...){
  ##
  structure(
    ucdTblParse(x= readLines( paste(ucd.path, rpath, sep = "/") ), ... )
    , rpath = rpath
  )
}


##
##
##
##
##
##
##
read.udata <- function(rpath="ucd/UnicodeData.txt", ucd.path){
  ##
  udata.lab <- c(
    "cp" = cp_label ## "Code point"                       
    , "Name" = "Code point Name (na)"                    
    , "General_Category" = "Code point General_Category (gc)"         
    , "Canonical_Combining_Class" = "Code point Combining_Class (ccc)"
    , "Bidi_Class"=  "Code point Bidirectional Class (bc)"  
    ##
    , "Decomposition_Type" = "Code point Decomposition Type (dt)"  
    , "Decomposition_Mapping" = "Code point Decomposition Mapping"
    ##
    , "Numeric_Type_decimal"  = "Numeric_Type=Decimal"            
    , "Numeric_Type_digit"  = "Numeric_Type=Digit"            
    , "Numeric_Value"  = "Numeric_Value for Numeric_Type=Numeric"    
    ##
    , "Bidi_Mirrored" = "Code point Bidirectional Mirrored"
    , "Unicode_1_Name" = "Code point v1 Name (obsolete)"
    , "ISO_Comment" = "Iso Comment (obsolete)"
    , "Simple_Uppercase_Mapping" = "Simple_Uppercase_Mapping Code point (suc)"
    , "Simple_Lowercase_Mapping" = "Simple_Lowercase_Mapping Code point (slc)"
    , "Simple_Titlecase_Mapping" = "Simple_Titlecase_Mapping Code point (stc)"
  )
  ##
  ## parse Decomposition_Type
  ##
  decompTyParse <- function(x, j){
    ##
    m <- stringi::stri_match(
      x[,j]
      , regex = paste0(
        "(<([A-Za-z]+)>)?"
        , "((\\s?[0-9A-F]+)*)"
      )
    )
    ##
    if( any( (nchar(x[,j])>0)!= (nchar(m[,1])>0) ) ) stop("Decomposition_Type")
    ##
    m[,c(3,4)]
  }
  ##
  ##
  ##
  ucd.cnames <- names(udata.lab)
  ##
  rv <- ucdTblParse( ## 
    readLines(paste(ucd.path, rpath, sep = "/"))
    , cnm = paste0("x", 1:15)
    , comm.parse = F
    , cp.drop = T
  )
  ##
  decompTy0 <- decompTyParse(rv, j =  j <- 6)
  ## insert Decomposition Type and Mapping
  udata0 <- within(
    setColnames(
      data.frame(
        rv[,1:(j-1)]
        ##
        , decomp = decompTy0[,1]
        , decompTy = decompTy0[,2]
        ##
        , rv[,(j+1):ncol(rv)]
        , stringsAsFactors = F
      )
      , ucd.cnames
    )
    , {
      Decomposition_Mapping <- strToCpSeq(Decomposition_Mapping)
      ##
      Simple_Uppercase_Mapping <- as.vector(strToCp(
        Simple_Uppercase_Mapping
        , drop = T
      ))
      ##
      Simple_Lowercase_Mapping <- as.vector(strToCp(
        Simple_Lowercase_Mapping
        , drop = T
      ))
      ##
      Simple_Titlecase_Mapping <-as.vector(strToCp(
        Simple_Titlecase_Mapping
        , drop = T
      ))
    }
  )
  ##
  clabels(udata0) <- udata.lab
  ##
  structure(
    udata0
    , rpath = rpath
  )
}
##
##
##
udataTowide <- function(udata){
  ##
  ## Ranges
  ##
  i <- matrix(
    grep('((First)|(last))>', udata$Name, ignore.case = T, perl=T)
    , ncol=2,byrow=T
  )
  ##
  rv <- subset(
    within(
      cbind(
        cp_lo = cp <- udata$cp##
        , cp_hi = cp
        , udata[, 2:ncol(udata)]
      )
      ,{
        cp_hi[i[,1]] <- cp_hi[i[,2]]
        Name[i[,1]] <-  apply(
          matrix(udata$Name[i],ncol=2,byrow=F)
          , 1
          , paste, collapse=".."
        )
      })
    , !( 1:nrow(udata) %in% i[,2])
  )
  ##
  structure(
    rv
    , variable.labels = c( cp_lo_label, cp_hi_label, ( v <- attr(udata, "variable.labels") )[-1] )
    , rpath = attr(udata, "rpath")
  )
}
##
## 
##
read.propAl <- function(
  ##
  rpath="ucd/PropertyAliases.txt", ucd.path
){
  ##
  fnm <- paste(ucd.path, rpath, sep = "/")
  ##
  propal0.0 <- ucd.getVal(readLines( fnm ))
  ##\\w*([\\p{L}_\\d]+)
  m0 <- stringi::stri_match(
    propal0.0
    , case_insensitive=T
    , regex = paste0(
      "([\\p{L}_\\d]+?\\s*)"
      , ";"
      , "\\W*(\\w+)\\s*"
      , "(;(.+))*"
    )
  )
  if( any( (nchar(propal0.0)>0)!= (nchar(m0[,1])>0) ) ) stop("propal")
  ##
  propal0.1 <- setColnames( m0[,c(2,3,5)], c( "propabbr" , "propname", "comments")  )
  ##
  propal.htxt <- htxt(propal0.0)
  ##
  hidx <- attr(propal.htxt, "hidx")
  ##
  hlen <- attr(propal.htxt, "hlen")
  ##
  lab <- propal.htxt[cumsum(hlen)[-length(hlen)]-1] 
  ##
  propal1 <- data.frame( propcat=rep(lab, times=diff(hidx)), propal0.1, stringsAsFactors = F)
  ##
  attr(propal1, "htxt") <- htxt(propal0.0)
  ##
  clabels(propal1) <- c(
    "propcat" = "Property category"
    , "propabbr" = "Property abbreviation"
    , "propname" = "Property name"
    , comments = comments_label
  )
  ##
  structure(
    propal1
    , rpath = rpath
  )
}
##
## propValal
##
read.propValal <- function(rpath="ucd/PropertyValueAliases.txt", ucd.path){
  ##
  propal0.0 <-  ucd.getVal(readLines( paste(ucd.path, rpath, sep = "/") ))
  ##
  propal.htxt <- htxt(propal0.0)
  ##
  hidx <- attr(propal.htxt, "hidx")
  ##
  hlen <- attr(propal.htxt, "hlen")
  ##
  propal0.m0 <- stringi::stri_match(
    propal0.0
    , regex=paste(
      "^\\s*(.+?)\\s*;\\s*(.+?)\\s*;([^#;]+)(.*)" ## $  ([^#]*)([;#](.+))* (.+?)
      , sep =""
    )
  )[,2:5]
  ## ???
  ccc <- strsplit(propal0.m0[,4], ';')
  ##
  ccc <- lapply(
    ccc
    , function(x) {
      if( (l<-length(x))>1){
        if( nchar(x[1]==0) ) x <- x[-1] 
        x <- gsub("(^\\s+)|(\\s+$)", "", x, perl=T)
      }
      x
    }
  )
  ##
  colnames(propal0.m0) <- c("prop", "val", "valname", "comments")
  ##
  ## on prend le __dernier__ item -cf. @missing
  ##
  ##
  lab <- propal.htxt[cumsum(hlen)] ##
  ##
  propal.lab0 <- rep(lab[-length(lab)], times=hidx[c(-1)] - hidx[c(-length(hidx))])
  ##
  rv <- structure(
    data.frame( propname=propal.lab0, propal0.m0, stringsAsFactors = F)
    , htxt=propal.htxt
    , rpath = rpath
  )
  ##
  clabels (rv) <- c(
    "propname" = "Property name"
    , "prop"  = "Property"
    , "val"  = "Property values"
    , "valname" ="Property value name"
    , comments = comments_label
  )
  ##
  rv
}

##
## Unicode Named Character Sequences
##
# Format:
# Name of Sequence; Code Point Sequence for USI
##
##
##
read.namedSeq <- function(rpath="ucd/NamedSequences.txt", ucd.path){
  ##
  txt <- ucd.getVal(readLines( paste(ucd.path, rpath, sep = "/") ))
  ##
  rv <- structure(
    within(
      read.table(textConnection(txt), sep=";", stringsAsFactors = F,comment.char ="", col.names = c("Name", "cp"))
      , {
        cp = strToCpSeq(cp)
      }
    )
    , htxt=htxt(txt)
    , variable.labels = c("Name", cp_seq_label)
    , rpath = rpath
  )
  ##
}


##
##
## 
# <code>; <status>; <mapping>; # <name>
# 0041; C; 0061; # LATIN CAPITAL LETTER A
## 
read.casefold <- function(rpath="ucd/CaseFolding.txt", ucd.path){
  ##
  within(
    read.ucdTbl(
      rpath
      , ucd.path
      , cnm = c("cp", "status", "Case_Folding", "Name") ##
      , nc = 1L
      , cp.drop=T
      , comm.parse=F
    )
    , {
      Case_Folding <- as.vector( strToCp(Case_Folding, drop=T))
  })
  # ##
  # rv <- ucdCsvParse(
  #   readLines(paste(ucd.path, rpath, sep = "/") )
  #   ## 
  #   , cnm=c(
  #     "cp", "status"
  #     , "Case_Folding" ## "mapping" ## 
  #     , "Name"
  #   )
  #   , penult.drop = T
  # )
  # ##
  # structure(
  #   rv
  #   , variable.labels = c(
  #     cp_label
  #     , "status"
  #     , "Case_Folding" ## "mapping" ## 
  #     , "Name"
  #   )
  #   , rpath= rpath
  # )
}
##
##
# The <condition_list> is optional. Where present, it consists of one or more language IDs               
# or casing contexts, separated by spaces. In these conditions:                                    
# - A condition list overrides the normal behavior if all of the listed conditions are true.             
# - The casing context is always the context of the characters in the original string,                   
# NOT in the resulting string.                                                                           
# - Case distinctions in the condition list are not significant.- Conditions preceded by \"Not_\" represent the negation of the condition.
# The condition list is not represented in the UCD as a formal property.
##
# Parsers of this file must be prepared to deal with future additions to this format:"                     
# * Additional contexts                                                                                 
# * Additional fields   
##
# <code>; <lower>; <title>; <upper>; (<condition_list>;)? # <comment>
# 00DF; 00DF; 0053 0073; 0053 0053; # LATIN SMALL LETTER SHARP S
##
read.specialCasing <- function(rpath="ucd/SpecialCasing.txt", ucd.path){
  ##
  sc0 <- ucd.getVal(readLines( paste(ucd.path, rpath, sep = "/") ))
  ##
  m0 <- stringi::stri_match(
    sc0
    , case_insensitive=T
    , regex = paste0(
      paste(rep( "\\s*([0-9A-F\\s]+);", 4), collapse="")
      , "(([0-9A-Z\\s_]+);)*"
      , "(\\s*#\\s*(.+))*"
    )
  )
  ##
  rv <- apply( m0[,c(3:5)], 2 , function(x){
    lapply(strsplit(x, " "), charToCp) ##x
  })
  ##
  sc1 <- structure(
    data.frame(
      cp = charToCp(m0[,c(2)])
      , I(rv[[1]])
      , I(rv[[2]])
      , I(rv[[3]])
      , m0[,c(7,9)]
      , stringsAsFactors = F
    )
    , htxt = htxt(sc0)
  )
  ##
  colnames(sc1) <- c(
    "cp"
    , "Lowercase_Mapping" ## "cp_lowcase" ## 
    , "Titlecase_Mapping" ## "cp_titlecase" ## 
    , "Uppercase_Mapping" ## "cp_uppercase" ## 
    , "condition_list", "comments"
  )
  ##
  structure(
    sc1
    , variable.labels = c(
      cp_label
      , "Lowercase Mapping", "Titlecase Mapping", "Uppercase Mapping"
      , "One or more language IDs or casing contexts"
      , comments_label
    )
    , rpath = rpath
  )
}

##
##
##
read.dervNormProp <- function(rpath="ucd/DerivedNormalizationProps.txt", ucd.path,...){
  ##
  txt <- ucd.getVal(readLines(paste(ucd.path, rpath, sep = "/")))
  ##
  ## # champs variable
  ##
  m <- stringi::stri_match(
    txt
    , regex = paste0(
      "(.+?);"
      , "(?<m1>(.+?);)?" ## 
      , "(.+?)#(.+)"
    )
  )
  ##
  rv <- within(
    as.data.frame(
      m[,c(2,4,5,6)]
      , stringsAsFactors = F
    )
    ,{
      V1 <- as.data.frame(strToCp(V1, drop =F), stringsAsFactors=F)
      lo <- V1[,1]
      hi <- V1[,2]
      V1 <- NULL
      na2 = is.na(V2)
      V2[na2] <- V3[na2]
      V3[na2] <- NA_character_
      rm(na2)
      ## 
      Quick_Check <- ifelse(
        is.na(V3)
        , NA_character_ ## 'N'
        , ifelse( grepl("[MN]", V3), gsub("(^\\s+)|(\\s+$)", "", V3), "Y")
      )
      ##
      nfSeq <- strToCpSeq(V3)
    })
  ##, "V3" "V1"
  rv <- rv[,c( "lo", "hi", "V2", "Quick_Check", "nfSeq", "V4")]
  ##
  rv <- setColnames(
    rv, c("cp_lo", "cp_hi", "normprop", "Quick_Check", "nfSeq", "comments")
  )
  ##
  structure(
    rv
    , htxt=htxt(txt)
    , variable.labels = c(
      cp_lo_label, cp_hi_label
      , "Normalization property"
      , "Quick Check"
      , "Normalization sequence"
      , comments_label
    )
    , rpath = rpath
  )
}


##
##
##
##
##
read.arabicShapings <- function(rpath="ucd/ArabicShaping.txt", ucd.path){
  ##
  txt <- ucd.getVal(readLines( paste(ucd.path, rpath, sep = "/") ))
  ##
  rv <- within(
    read.table(
    textConnection(txt)
    , col.names = c( "cp", "Name", "Joining_Type", "Joining_Group")
    , sep=";", stringsAsFactors = F,comment.char =""
  )
  , {
    cp = as.vector(strToCp(cp, drop=T))
  })
  ##
  structure(
    rv
    , htxt=htxt(txt)
    , variable.labels = c(cp_label, "Name", "Joining Type", "Joining Group")
    , rpath = rpath
  )
  ##
}

##
# The first field is the CJK radical number. 
# The second field is the CJK radical character
# The third field is the CJK unified ideograph.
##
#  ¡¡¡ WUT : cf. "'" CJKid se répètent !!!
##
##
read.CJKRadicals <- function(rpath="ucd/CJKRadicals.txt", ucd.path){
  ##
  txt <-  ucd.getVal(readLines( paste(ucd.path, rpath, sep = "/") ))
  ##
  rv <- within(
    read.table(
      textConnection(sub("'", "", txt))
      , col.names = c( "CJKradicalNum", "cp_CJKradical", "cp_unified_ideograph")
      , sep=";", stringsAsFactors = F,comment.char =""
    )
    , {
      cp_CJKradical = strToCp(cp_CJKradical, drop=T)[,1, drop=T]
      cp_unified_ideograph = strToCp(cp_unified_ideograph, drop=T)[,1, drop=T]
    })
  ##
  structure(
    rv
    , htxt=htxt(txt)
    , variable.labels = c( "CJK radical number", "CJK radical character", "CJK unified ideograph")
    , rpath = rpath
  )
  ##
}

##
##
##
read.brkTest <-  function(
  rpath, ucd.path
  , cnm = c("string", "comment")
){
  # )
  txt <- ucd.getVal(
    x <- readLines(paste(ucd.path, rpath, sep = "/"))
  )
  ##
  rv <- structure(
    setColnames(
      as.data.frame(ucdCommParse(txt), stringsAsFactors = F)
      , cnm
    )
    , htxt = htxt(txt)
    , variable.labels = c("string", comments_label)
    , rpath = rpath
  )
}

##
## UCA 
##
read.allkeys <- function(rpath="uca/allkeys.txt" , uca.path,...){
  ##
  txt <- ucd.getVal(
    readLines( paste(ucd.path, rpath, sep = "/"))
  )
  ##
  m <- grepl("^@", txt)
  ##
  rv <- read.table(textConnection(txt[!m]), sep=sep, stringsAsFactors = F, comment.char ="")
  ##
  comm <- ucdCommParse(rv[,2])
  ##
  allkeys <- data.frame(
    cp = I(strToCpSeq(rv[,1]))
    , collationElement = comm[,1]
    , comments =  comm[,2]
    , stringsAsFactors = F
  )
  ##
  structure(
    allkeys
    , htxt=htxt(txt)
    , meta = txt[m]
    , variable.labels = c(
      "Code point sequence"
      , "collation element"
      , comments_label
    )
    , rpath = rpath
  )
}
##
##
##
read.decomps <- function(rpath="uca/decomps.txt" , uca.path,...){
  structure(
    within(
    ucdTblParse( ##
      readLines(paste(uca.path, rpath, sep="/"))
      , cnm = c("cp", "decomposition_tag", "decomposition", "comments")
    )
    ,{
      decomposition <- strToCpSeq(decomposition)
    })
    , rpath = rpath
  )
}

##
## IVD
##
##
##
read.ivdSeq <- function(rpath="ivd/IVD_Sequences.txt" , ivd.path, fnm="IVD_Sequences.txt",...){
  ##
  fpath <- if( !missing( rpath) ) paste(ivd.path, rpath, sep="/") else{
    fpath <- paste(ivd.path, "ivd", sep="/")
    dnm <- list.dirs(fpath)
    dnm <- stringi::stri_match(dnm, regex= pat <- "(\\d{4}-\\d{2}-\\d{2})")
    if( all(is.na(dnm[,1])) ) stop("no directory matching ", pat, " directory in ", fpath)
    # dnm <- sort(as.Date(dnm[,2]), decreasing = T)[1]
    ##, dnm
    # rpath <- paste( "ivd", fnm, sep = "/")
    ##
    paste( fpath, sort(as.Date(dnm[,2]), decreasing = T)[1] , fnm, sep = "/")
  }
  ##
  structure(
    within(
      ucdCsvParse(
        readLines(fpath)
        , cnm = c("cp", "collection", "ivd_oid")
        , comm.parse = F
        , 
      )
      , {
        cp <- strToCp(cp, drop=T)
      }
    )
    , rpath = formals(read.ivdSeq)$rpath ##rpath 
  )
}

##
##
## 3 semi-colon separated columns; last columns uses \t as separator
##
read.confusable <- function(rpath="security/confusables.txt", ucd.path){
  ##
  structure(
    within(
      ucdTblParse( # ucdCsvParse(
        readLines( paste(ucd.path, rpath, sep = "/"))
        , cnm = , c("cp", "target", "obsolete", "comments")
        , cp.drop = T
      )
      , {
        ##
        target = strToCpSeq(
          sub( "^\\t", "", target)
        )
        ## #1 field of last column
        obsolete= sub( "^\\t", "", obsolete)
      }
    )
    , rpath = rpath
  )
}
##
##
##
read.confusableSummary <- function(rpath="security/confusablesSummary.txt", ucd.path){
  ##
  txt <- ucd.getVal(
    readLines( paste(ucd.path, rpath, sep = "/"))
  )
  ##
  m <- stringi::stri_match(
    txt
    ## ou \\t(.+?)\\t
    , regex="\\)\\s+(([A-F\\d]{4,5}\\s)+)"
  )
  ##
  cp <- m[,2]
  ##
  h <- htxt(txt)
  ##
  hidx <- attr(h, "hidx")
  ##
  hidx <- hidx[-length(hidx)]
  ##
  confus <- rep(
    cp[hidx]
    , nrep <- diff(attr(h, "hidx")) - 1
  )
  ##
  rv <- data.frame(
    cp = I(
      lapply(strsplit(cp[-hidx], "\\s"), charToCp)
    )
    , confusable =  I(
      lapply(strsplit(confus, "\\s"), charToCp)
    )
    , comments = paste(
      rep(txt[hidx], nrep)
      , txt[-hidx]
    )
    , stringsAsFactors = F
  )
  ##
  htxt(rv) <- h
  ##
  structure( rv, rpath = rpath )
}