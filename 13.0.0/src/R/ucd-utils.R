##
##
##
setColnames <- function(x, cnm = paste(prefix,1:ncol(x),sep=''), prefix='X' ){
  nm <- names(cnm)
  if(length(nm)==0L){
    colnames(x) <- cnm
    x
  }else{
    stopifnot(is.character(cnm))
    i <- match((colnames(x)), nm, nomatch = 0L)
    if(any(
      na <- !( 1:length(nm) %in% i[i!=0L] )
    )) warning("missing colname(s): ", nm[na])
    colnames(x)[i!=0L] <- (cnm[i[i!=0L]])
    x
  }
}

##
##
##
`clabels<-` <- function(x, value=NULL){
  ##
  j <- match(names(value), colnames(x), nomatch = 0)
  ##
  if( any( i <- (j==0)) ){
    stop("not found: ", paste( names(value)[i], collapse = ", " ))
  }
  ##
  j <- match(colnames(x), names(value),nomatch = 0)
  ##
  nc <- ncol(x)
  ##
  if( is.null(attributes(x)$variable.labels ) ) attributes(x)$variable.labels <- rep("", nc)
  ##
  if( length(attributes(x)$variable.labels )!=nc ) stop("wrong number of labels", nc )
  ##
  attributes(x)$variable.labels[j] <- value
  ##
  x
}
##
clabels <- function(x) attributes(x)$variable.labels

##
##
##
htxt <- function(x){
  attr(x,"htxt")
}
##
`htxt<-` <- function(x,value){
  ##
  nm <- c('htxt', 'hidx', 'hlen', 'src.hidx')
  ## WUT : is.vector(x) renvoie F avec attr ? 
  if( 
    all( names( attributes(value))  %in% nm[-1] )
  ){
    ##
    cat("htxt\n");print(str(value))
    ##
    structure(
      x
      , htxt = value
    )
    return(x)
  }
  ##
  if(
    !all(
      (idx <- ( names(value) %in% nm))
    )
  )stop("unknown args: " , paste( names(value)[!idx] , collapse = ","))
  ##
  if(
    any( null <- sapply(value,is.null) )
  ){
    stop("NULL args: ", paste( names(value)[null] , collapse = ",") )
  }
  ##
  h <- structure(
    value[['htxt']], hidx=value[['hidx']], hlen=value[['hlen']]
    , src.hidx=value[['src.hidx']]
  )
  ##
  x <- structure(x, htxt=h)
  ##
  x
}
##
ucd.hdr <- function(x){
  if(
    is.null(h <-  htxt(x))
  ) return(NA_character_)
  ##
  h[1:attr(h,"hlen")[1]]
}
##
##
##
ucd.comments <- function(x, xpd=F){
  ##
  h <- attr(x, "htxt")
  ##
  hidx <- attr(h, "hidx")
  ##
  hlen <- attr(h, "hlen")
  ##
  if(!xpd) return(
    ##
    lapply(
      1:(length(hlen))
      , function(i, idx){
        ##h[idx[i]:(idx[i+1]-1L)]
        ##
        h[
          idx[i] + (0:(hlen[i]-1L))
        ]
        ##
        # idx[i] + (0:(hlen[i]-1L))
      }
      , idx = cumsum(c(1,hlen))
    )
  )
  ## ¡¡¡ rm comments beyond last line !!!
  nnib <- sum( hidx>nrow(x) ) 
  ##
  print(nnib)
  ##
  comments <- list() ## 
  ##
  comments[hidx[i]] <- 
    sapply(
      i <- 1:(length(hidx) - nnib)
      , function(i, pos){
        idx <- pos[i]+ (0:(hlen[i]-1L))
        h[idx]
      }
      , pos=cumsum(c(1, hlen[-( length(hlen)-(nnib:0) ) ]) )
    )
  ##
  if( (nr <-nrow(x)) > (ncomm <- length(comments)) ) comments[
    ncomm + (1:(nr - ncomm))
  ] <- ""
  ##
  comments
}

##
## vectorized parsing of commented lines
##
## the file format is meant to be easily parsed using data streams and looping over lines
## but since this would be slow in R and since R use a vector based data model, we need to vectorize everything
## 
ucd.getVal <- function(txt,keep=c( "comments")){
  ## rm empty lines
  txt <- txt[nchar(txt)>0]
  ##
  htag <- grepl("^#",txt)
  ## !!! hlo %% 2 : début du bloc suivant
  hlo.idx <- ( htag!=c("#",htag[-length(htag)]) ) ## 
  ##
  hhi.idx <- ( htag!=c(htag[-1],"#") ) & htag
  ## # commented lines
  nh <- sum(hhi.idx)
  ##
  hnum   <- cumsum(hlo.idx)
  ## !!! rm next block idx
  hlo.idx <- hlo.idx & htag
  ## get rid of leading #
  htxt <- sub("^#\\s+" , "", txt[htag])
  ##
  val <- txt[!htag]
  ##
  idx <- (1:length(txt))
  ## position of the corresponding values in the output vector
  ## by subtracting # of previous lines to the original position
  h.vidx <- idx[hlo.idx]  - cumsum(c(
    0
    , ( hlen <- idx[hhi.idx] - idx[hlo.idx] +1)[-nh]
  ))
  ## ¡¡¡ sanity checks !!!
  # length(val)==(length(txt)-sum(hlen)-2L)
  # txt[which(hhi.idx)+1]==val[h.vidx]
  ##
  htxt(val) <- list(htxt=htxt,hidx=h.vidx,hlen=hlen, src.hidx=which(hlo.idx))
  ##
  val
}

##
##
##
charToCp <- function(x){
  cp <- strtoi(
    ifelse(is.na(x),x, sprintf("0x%s", x))
  )
  ##
  cp
}
##
##
##
strToCp <- function(x, cnm=c("cp_lo", "cp_hi"), drop=F){
  ##
  m0 <- stringi::stri_match(x,regex="([A-Z0-9]+)(\\.\\.([A-Z0-9]+))*")[,c(2,4)]
  ##
  cpr <- apply(m0,2,charToCp)
  ##
  na <- is.na(cpr[,2])
  ## drop 2nd column if empty
  if( drop ){
    if( !all(na) ) warning("drop:", paste(head(cpr[!na,2]), collapse=" ") )
    cpr <- cpr[,1,drop=F]
    colnames(cpr)<- c("cp")
  }
  else{
    ##
    cpr[na,2] <- cpr[na,1]
    ##
    colnames(cpr) <- cnm
  }
  ## 
  cpr
}
##
## 
##
 strToCpSeq <- function(x){
  ##
  lapply(
    strsplit(
      gsub("(^\\s+)|(\\s+$)", "", x)
      , "\\s+"
    )
    , function(x) {
      if( length(rv <- charToCp(x)) ) rv else NA_integer_
    }
  )
}