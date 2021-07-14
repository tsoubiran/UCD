# Unicode Character Database (UCD)

This repo contains a subset of the [Unicode Character Database](https://unicode.org/ucd/) (UCD) version 13.0.0 files converted to R `data.frames`. It is intended to serve as a companion site to a [blog series](https://numa.hypotheses.org/2542) on the Unicode standard.

The UCD provide machine-readable data files about Unicode [characters properties](https://numa.hypotheses.org/3263) and is documented in [Technical Report #44](http://www.unicode.org/reports/tr44/tr44-26.html).

## Layout

Most files consists of character code points &mdash;`cp`&mdash; or code points range &mdash;`cp.lo`&ndash;`cp.hi`&mdash; and corresponding property value or, sometimes, mappings to other code points. Most files also contains one or more additional informative fields. Since they may vary in length and meaning, they're usually stored in single column named `comments` of the `data.frame`.

Files names are abbreviated in order to avoid too much typing because of the added prefix

|File Name | `data.frame` Name |
|----------|-------------------|
|[PropertyAliases](https://unicode.org/reports/tr44/#PropertyAliases.txt)|ucd.propal|
|[PropertyValueAliases](https://unicode.org/reports/tr44/#PropertyValueAliases.txt)|ucd.propvalal|
|[UnicodeData](https://unicode.org/reports/tr44/#UnicodeData.txt)|ucd.udata|
|[DerivedAge](https://unicode.org/reports/tr44/#DerivedAge.txt)|ucd.age|
|[Blocks](https://unicode.org/reports/tr44/#Blocks.txt)|ucd.blk|
|[PropList](https://unicode.org/reports/tr44/#PropList.txt)|ucd.prop|
|[DerivedCoreProperties](https://unicode.org/reports/tr44/#DerivedCoreProperties.txt)|ucd.dervprop|
|[Scripts](https://unicode.org/reports/tr44/#Scripts.txt)|ucd.scripts|
|[ScriptExtensions](https://unicode.org/reports/tr44/#ScriptExtensions.txt)|ucd.scriptsExt|
|[LineBreak](https://unicode.org/reports/tr44/#LineBreak.txt)|ucd.lbrk|
|[CompositionExclusions](https://unicode.org/reports/tr44/#CompositionExclusions.txt)|ucd.compex|
|[CaseFolding](https://unicode.org/reports/tr44/#CaseFolding.txt)|ucd.casefold|
|[auxiliary/GraphemeBreakProperty](https://unicode.org/reports/tr44/#GraphemeBreakProperty.txt)|ucd.grBrkProp|
|[auxiliary/WordBreakProperty](https://unicode.org/reports/tr44/#WordBreakProperty.txt)|ucd.wordBrkProp|
|[emoji/emoji-data](https://unicode.org/reports/tr44/#emoji-data.txt)|ucd.emoji|

The standard also provides data files for Unicode Security Mechanisms which are located in the security directory of the repository

|File Name | `data.frame` Name |
|----------|-------------------|
|[confusables](http://www.unicode.org/reports/tr39/#Data_File_List)|ucd.confus|
|[IdentifierStatus](http://www.unicode.org/reports/tr39/#Data_File_List)|ucd.idStat|
|[IdentifierType](http://www.unicode.org/reports/tr39/#Data_File_List)|ucd.idType|
|[intentional](http://www.unicode.org/reports/tr39/#Data_File_List)|ucd.intent|

Please refer to [Tr#39: Unicode Security Mechanisms](http://www.unicode.org/reports/tr39/) for further details.

If you don't want to download the entire database, you can download individual files from R using a modified url :

``` r
co <- url('https://raw.githubusercontent.com/tsoubiran/UCD/master/13.0.0/security/Rdata/confusables.Rdata')
load(co)
close(co)
``` 

Using [https://github.com/tsoubiran/UCD/blob/master/13.0.0/security/Rdata/confusables.Rdata](https://github.com/tsoubiran/UCD/blob/master/13.0.0/security/Rdata/confusables.Rdata) won't work because bacause in that case github.com  will redirect to raw.githubusercontent.com and `url()` does not handle redirection so it seems.

## Metadata

The `data.frames` retain the original commented lines &mdash;if any&mdash; in an `attribute` named "htxt". For example, you can retrieve the header of the original UCD files which describes the file's format and points to the relevant TR like this 

``` r
ucd.hdr <- function(x){
  h <- attr(x,"htxt")
  h[1:attr(h,"hlen")[1]]
}
ucd.hdr(ucd.propvalal)
``` 

or extract every comment blocks

``` r
ucd.comments <- function(x){
  h <- attr(x,"htxt")
  lapply(
    1:(length(attr(h, "hlen")))
    , function(i, idx){
      cat(i)
      h[idx[i]:idx[i+1]]
    }
    , idx = c(1,cumsum(attr(h, "hlen")))
  )
}
commments <- ucd.comments(ucd.propvalal)
``` 

## License

Please refer to the Unicode® [Copyright and Terms of Use](https://www.unicode.org/copyright.html) for more information.
