##
##
##
ucdCommParse <- function(x, j = c(2,4)){
  ##
  m <- stringi::stri_match(x,regex="([^#]*)(#\\s*(.+))?")[,j]
  ##
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
    comm <- rv[,2]
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
# ucdCsvParse <- function(x, cnm, sep=";", comm.parse = T, penult.drop = F){
#   ##
#   txt <- ucd.getVal(x)
#   ##
#   rv <- read.table(textConnection(txt), sep=sep, stringsAsFactors = F,comment.char ="")
#   ##
#   ##
#   rv <- structure(
#     ##
#     if( comm.parse ){
#       ##
#       m <- as.data.frame(ucdCommParse(rv[, ncol(rv)]), stringsAsFactors = F) ##
#       ##
#       if(penult.drop) m <- m[, -ncol(m)+1, drop=F]
#       ##
#       cbind(rv[,-ncol(rv), drop=F], m) ##
#     } else rv ##
#     ##
#     , htxt = htxt(txt)
#   )
#   ## ASSERT comm.parse==T
#   # if(penult.drop) rv <- rv[, -ncol(rv)+1]
#   ##
#   ##
#   if(!missing(cnm)) setColnames(rv, cnm) else rv
# }
##
##
##
ucdTblParse <- function(x, cnm = NULL, ncp = 1L, cp.drop=F, hex.drop = T, sep=";", comm.parse=T, penult.drop = F){
  ## 
  if(missing(cnm)) stop("missing cnm")
  ##
  if( ncp==0L ){
    ##
    txt <- ucd.getVal(x)
    ##
    rv <- ucdCommParse(txt)
    ##
    cp <- strToCp(rv[,1], drop=cp.drop)
    ##
    return(
      structure(
        setColnames(
          data.frame(cp, rv[,2], stringsAsFactors = F)
          , cnm
        )
        , htxt=htxt(txt)
      )
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
  if( comm.parse ){
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
read.udata <- function(rpath="ucd/UnicodeData.txt", ucd.path){
  ##
  udata.lab <- c(
    "cp" = "Code point"                       
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
  ##
  udata0 <- within(
    setColnames(
      data.frame(
        rv[,1:(j-1)]
        , decomp = decompTy0[,1]
        , decompTy = decompTy0[,2]
        , rv[,(j+1):ncol(rv)]
        , stringsAsFactors = F
      )
      , ucd.cnames
    )
    , {
      Decomposition_Mapping <- strToCpSeq(Decomposition_Mapping)
    }
  )
  ##
  clabels(udata0) <- udata.lab
  ##
  ## Ranges
  ##
  i <- matrix(
    grep('((First)|(last))>', udata0$Name, ignore.case = T, perl=T)
    , ncol=2,byrow=T
  )
  ##
  udata1 <- subset(
    within(
      cbind(
        cp_lo = cp <- udata0$cp##
        , cp_hi = cp
        , udata0[, 2:ncol(udata0)]
      )
      ,{
        cp_hi[i[,1]] <- cp_hi[i[,2]]
        Name[i[,1]] <-  apply(
          matrix(udata0$Name[i],ncol=2,byrow=F)
          , 1
          , paste, collapse=".."
        )
      })
    , !( 1:nrow(udata0) %in% i[,2])
  )
  ##
  structure(
    udata1
    , rpath = rpath
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
    , rpath = rpath
  )
  ##
}


##
##
## 
# 0041; C; 0061; # LATIN CAPITAL LETTER A
## 
read.casefold <- function(rpath="ucd/CaseFolding.txt", ucd.path){
  ##
  rv <- ucdCsvParse(
    readLines(paste(ucd.path, rpath, sep = "/") )
    ## 
    , cnm=c(
      "cp", "status"
      , "Case_Folding" ## "mapping" ## 
      , "Name"
    )
    , penult.drop = T
  )
  ##
  structure(rv, rpath= rpath)
}
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
    , "cond", "comments"
  )
  ##
  structure(
    sc1
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
    rv, c("cp_lo", "cp_hi", "nf", "Quick_Check", "nfSeq", "comments")
  )
  ##
  structure(
    rv
    , htxt=htxt(txt)
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
  rv <- read.table(
    textConnection(txt)
    , col.names = c( "cp", "Name", "Joining_Type", "Joining_Group")
    , sep=";", stringsAsFactors = F,comment.char =""
  )
  ##
  structure(
    rv
    , htxt=htxt(txt)
    , rpath = rpath
  )
  ##
}

##
##
#  ¡¡¡ WUT : cf. "'" CJKid se répètent 
##
##
read.CJKRadicals <- function(rpath="ucd/CJKRadicals.txt", ucd.path){
  ##
  txt <-  ucd.getVal(readLines( paste(ucd.path, rpath, sep = "/") ))
  ##
  rv <- within(
    read.table(
      textConnection(sub("'", "", txt))
      , col.names = c( "CJKid", "Radicals", "Unified_Ideograph")
      , sep=";", stringsAsFactors = F,comment.char =""
    )
    , {
      Radicals = strToCp(Radicals, drop=T)
      Unified_Ideograph = strToCp(Unified_Ideograph, drop=T)
    })
  ##
  structure(
    rv
    , htxt=htxt(txt)
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
    , rpath = rpath
  )
}

##
##
##
read.decomps <- function(rpath="uca/decomps.txt" , uca.path,...){
  within(
    ucdTblParse( ##
      readLines(paste(uca.path, rpath, sep="/"))
      , cnm = c("cp", "decomposition_tag", "decomposition", "comments")
    )
    ,{
      decomposition <- strToCpSeq(decomposition)
    })
}

##
##
##
read.ivdSeq <- function(rpath="ivd/IVD_Sequences.txt" , ivd.path,...){
  structure(
    within(
      ucdCsvParse(
        readLines(paste(ivd.path, rpath, sep="/"))
        , cnm = c("cp", "collection", "ivd_oid")
        , comm.parse = F
        , 
      )
      , {
        cp <- strToCp(cp, drop=T)
      }
    )
    , rpath = rpath 
  )
}
