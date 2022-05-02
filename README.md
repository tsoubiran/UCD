# Unicode Character Database (UCD)

This repo contains most of the [Unicode Character Database](https://unicode.org/ucd/) (UCD) version 13.0.0 files converted to R `data.frames` as well as other contributory data files. It is intended to serve as a companion site to this [blog series](https://numa.hypotheses.org/2542) on the Unicode standard.

The UCD provides machine-readable data files related to the Unicode Standard implementation [characters properties](https://numa.hypotheses.org/3263) and is documented in [Technical Report #44](http://www.unicode.org/reports/tr44/tr44-26.html).

## Data Layout

Most files consists of Unicode character code points &mdash;`cp`&mdash; or code points range &mdash;`cp_lo`&ndash;`cp_hi`&mdash; or sequences and either one or more corresponding property value or mappings to other code points. Most files also contains one or more additional informative fields. Since they may vary in length, meaning and format, they're usually stored in single column named `comments` located at the end of the `data.frame`. Note that this is different from the line comments which contain information about code points or code point ranges range —see Metadata section below—.

In addition, some `data.frame` also have the `variable.labels` attribute set with a short column description. Use `attr(<data.frame/>, "variable.labels")` to see them.

Files R names are abbreviated in order to avoid too much typing because of the added prefix. The mapping between the original files names and the corresponding `data.frame` is documented in the README file of the ucd sub–directory in addition to a link that points to the file's description. A [13.0.0/ucd/ucd-hdr.html](listing) of each file's header and first six lines is also included.

Contributory data files for the [Unicode Collation Algorithm](http://www.unicode.org/reports/tr10/) are located in the uca sub–directory and [Ideographic Variation Sequences](http://www.unicode.org/reports/tr37/tr37-14.html) files are located in the ivs sub–directory.

The Unicode Consortium also provides data files for Unicode Security Mechanisms which are located in the security sub–directory of the repository&nbsp;:

|File Name | `data.frame` Name |
|----------|-------------------|
|[confusables](http://www.unicode.org/reports/tr39/#Data_File_List)|ucd.confus|
|[IdentifierStatus](http://www.unicode.org/reports/tr39/#Data_File_List)|ucd.idStat|
|[IdentifierType](http://www.unicode.org/reports/tr39/#Data_File_List)|ucd.idType|
|[intentional](http://www.unicode.org/reports/tr39/#Data_File_List)|ucd.intent|
|[confusablesSummary](http://www.unicode.org/reports/tr39/#Data_File_List)|ucd.confusSummary|

Please refer to [Tr#39: Unicode Security Mechanisms](http://www.unicode.org/reports/tr39/) for further details.

If you don't want to download the entire database, you can download individual files from R like this&nbsp;:

``` r
load(co <- url('https://github.com/tsoubiran/UCD/blob/master/13.0.0/security/Rdata/ucd.udata?raw=true'));close(co)
``` 

Note that in order to d this, you need to change the domain name. 
Otherwise, using  [https://github.com/tsoubiran/UCD/blob/master/13.0.0/security/Rdata/confusables.Rdata](https://github.com/tsoubiran/UCD/blob/master/13.0.0/security/Rdata/confusables.Rdata) won't work because because in that case github.com  will redirect to raw.githubusercontent.com and `url()` does not handle redirection so it seems.

## Code 

The src sub–directory contains R code used to convert the original files to R `data.frame` in addition to some utils for dealing with the file metadata. In order to use this code, you'll need to have the [stringi](https://cran.r-project.org/web/packages/stringi/index.html) library installed.

## Metadata

Each `data.frame` also stores the original commented lines &mdash;if any&mdash; in an `attribute` named "htxt". For example, you can retrieve the header of the original UCD files which describes the file's format and points to the relevant Technical Report like this&nbsp;:

``` r
ucd.hdr(ucd.propvalal)
``` 

or extract every comment blocks

``` r
## w/o expansion
comments <- ucd.comments(ucd.propvalal)
## with expansion
comments <- ucd.comments(ucd.propvalal, xpd=T)
``` 

`ucd.comments` returns a `list` of all comments. Using `xpd=T` returns a list of the same length as the original `data.frame` with comments located at their original position in the file. This can be useful for extracting relevant information and merging them with the `data.frame`.

## License

Please refer to the Unicode® [Copyright and Terms of Use](https://www.unicode.org/copyright.html) for more information.
