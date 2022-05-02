##
## Directory structure
##
# <ucd.path/>/ucd/
#   ArabicShaping.txt      
#   Blocks.txt                 
#   DerivedNormalizationProps.txt   
#   HangulSyllableType.txt
#   …
# 
# <uca.path/>/uca/
#   allkeys.txt  
#   decomps.txt
# 
# <ivd.path/>/ivd/
#   IVD_Collections.txt  
#   IVD_Sequences.txt
#   …
##
##
## if the root paths are all the sames :
# ivd.path <- uca.path <- ucd.path <- <path/>

##
## ucd/UnicodeData.txt
##
##
ucd.udata <- read.udata(
  ucd.path = ucd.path ## 
)

##
## 
##
##
ucd.nameAl <- read.ucdTbl(
  "ucd/NameAliases.txt"
  , ucd.path ## 
  , cnm = c("cp", "Name_Alias", "Name_Alias_Type")
  , comm.parse=F
  , cp.drop=T
)
##
## ucd/PropertyAliases.txt
##
##
ucd.propAl <- read.propAl( 
  ucd.path = ucd.path ## 
)
##
## ucd/PropertyValueAliases.txt
##
##
ucd.propValal <- read.propValal(
  ucd.path = ucd.path ##
)
##
## 
# Contributory properties are typically defined in PropList.txt and the corresponding derived property is then listed in DerivedCoreProperties.txt.
##
ucd.prop <- read.ucdTbl(
  "ucd/PropList.txt"
  , ucd.path ## 
  , cnm = c("cp_lo", "cp_hi", "propname", "comments")
  , ncp = 1L
  , cp.drop=F
)
##
## 
##
ucd.dervProp <- read.ucdTbl(
  "ucd/DerivedCoreProperties.txt"
  , ucd.path ## 
  , cnm = c("cp_lo", "cp_hi", "dpropname", "comments")
  , ncp = 1L
  , cp.drop=F
)

##
## 
##
ucd.age <- read.ucdTbl(
  "ucd/DerivedAge.txt"
  , ucd.path ## 
  , cnm = c("cp_lo", "cp_hi", "Age", "comments")
  , ncp = 1L
  , cp.drop=F
)

##
##
##
ucd.blk <- within(
  read.ucdTbl(
    "ucd/Blocks.txt"
    , ucd.path ## 
    ##
    , cnm = c("cp_lo", "cp_hi", "Block")
    , ncp = 1L
    , cp.drop=F
    , comm.parse = F
  )
  , Block <- gsub("(^\\s+)|(\\s+$)", "", Block)
)


##
## ucd/CaseFolding.txt
##
##
ucd.casefold <- read.casefold(
  ucd.path =  ucd.path ## 
)

##
## ucd/SpecialCasing.txt
##
##
##
ucd.SpecialCasing <- read.specialCasing(
  ucd.path =  ucd.path ## 
)

##
## ucd/DerivedNormalizationProps.txt
##
##
ucd.dervNormProp <- read.dervNormProp(
  ucd.path =  ucd.path ## 
)
##
##
##
ucd.NormalizationCorrections  <- read.ucdTbl(
  "ucd/NormalizationCorrections.txt"
  , ucd.path ## 
  , cnm = c("cp", "cp_err", "cp_decomp", "version" , "comments")
  , ncp = 3L
  , cp.drop=T
)
##
##
##
ucd.compex <- read.ucdTbl(
  "ucd/CompositionExclusions.txt"
  , ucd.path ## 
  , cnm = c("cp", "comments")
  , nc = 0L
  , cp.drop=T
)

##
##
##
ucd.BidiBrackets <- read.ucdTbl(
  "ucd/BidiBrackets.txt"
  , ucd.path ## 
  , cnm = c("cp", "Bidi_Paired_Bracket", "Bidi_Paired_Bracket_Type", "comments")
  , ncp = 2L
  , cp.drop=T
)
##
## ucd/BidiMirroring.txt
##
##
# ucd.udata$Bidi_Mirrored==Y
##
##
ucd.BidiMirroring <- within(
  read.ucdTbl(
    "ucd/BidiMirroring.txt"
    , ucd.path ## 
    , cnm = c("cp", "Bidi_Mirroring_Glyph", "comments")
    , ncp = 1L
    , cp.drop=T
  )
  , {
    Bidi_Mirroring_Glyph <- as.vector(strToCp(Bidi_Mirroring_Glyph, drop=T))
  }
)

##
##
## 
ucd.vertOrient <- read.ucdTbl(
  "ucd/VerticalOrientation.txt"
  , ucd.path ## 
  , cnm = c("cp_lo", "cp_hi", "Vertical_Orientation", "comments")
  , nc = 1L
  , cp.drop=F
)

##
## ucd/NamedSequences.txf
##
##
ucd.namedSeq <- read.namedSeq( 
  ucd.path =  ucd.path ## 
)
##
ucd.namedSeqProv <- read.namedSeq( 
  "ucd/NamedSequencesProv.txt"
  , ucd.path = ucd.path ## 
)

##
## 
##
ucd.scripts <- read.ucdTbl(
  "ucd/Scripts.txt"
  , ucd.path ## 
  ##
  , cnm = c("cp_lo", "cp_hi", "Script", "comments")
  , ncp = 1L
  , cp.drop=F
)

##
##
##
ucd.scriptsExt <- read.ucdTbl(
  "ucd/ScriptExtensions.txt"
  , ucd.path ## 
  ##
  , cnm = c("cp_lo", "cp_hi", "Script_Extensions", "comments")
  , ncp = 1L
  , cp.drop=F
)
##
## ucd/ArabicShaping.txt
##
##
ucd.ArabicShaping <- read.arabicShapings(
  ucd.path =  ucd.path ## 
)
##
## ucd/CJKRadicals.txt
##
##
ucd.CJKRadicals <- read.CJKRadicals(
  ucd.path =  ucd.path ##
)
##
## ucd/EastAsianWidth.txt
##
##
##
ucd.EastAsianWidth <- read.ucdTbl(
  "ucd/EastAsianWidth.txt"
  , ucd.path ## 
  , cnm = c("cp_lo", "cp_hi", "East_Asian_Width",  "comments")
  , ncp = 1L
  , cp.drop=F
)
##
## ucd/EquivalentUnifiedIdeograph.txt
##
# This data file lists CJK radicals and CJK strokes (aka characters
# that have the Radical=Yes property or are in the CJK Strokes block)
# for which there are reasonably equivalent CJK unified ideographs in
# that they are visually identical or near-identical.
##
# The first field contains a code point or code point range represented
# as a variable-length hexadecimal value with 4 to 6 digits. The second
# field contains a code point represented as a variable-length
# hexadecimal value with 4 to 6 digits.
##
ucd.EquivalentUnifiedIdeograph <- within(
  read.ucdTbl(
    "ucd/EquivalentUnifiedIdeograph.txt"
    , ucd.path ## 
    , cnm = c("cp_lo", "cp_hi", "Equivalent_Unified_Ideograph",  "comments")
    , ncp = 1L
    , cp.drop=F
  )
  ,{
    Equivalent_Unified_Ideograph <- as.vector(strToCp(Equivalent_Unified_Ideograph, drop=T) );
  }
)
##
##
##
# The first field gives the code point, in 4-digit hexadecimal
# form, of a conjoining jamo character that participates in the
# algorithmic determination of Hangul syllable character names.
# The second field gives the Jamo_Short_Name as a one-, two-,
# or three-character ASCII string (or in one case, for U+110B,
# the null string).
##
# Jamo.txt
##
ucd.jamo<- read.ucdTbl(
  "ucd/Jamo.txt"
  , ucd.path ## 
  , cnm = c("cp", "Jamo_Short_Name",  "comments")
  , ncp = 1L
  , cp.drop=T
)
##
## ucd/HangulSyllableType.txt 
##
ucd.HangulSyllableType <- read.ucdTbl(
  "ucd/HangulSyllableType.txt"
  , ucd.path ## 
  , cnm = c("cp_lo", "cp_hi", "Hangul_Syllable_Type",  "comments")
  , ncp = 1L
  , cp.drop=F
)
##
## ucd/IndicPositionalCategory.txt
##
##
ucd.IndicPositionalCategory<- read.ucdTbl(
  "ucd/IndicPositionalCategory.txt"
  , ucd.path ## 
  , cnm = c("cp_lo", "cp_hi", "Indic_Positional_Category",  "comments")
  , nc = 1L
  , cp.drop=F
)
##
## ucd/IndicSyllabicCategory.txt 
##
#    Field 0  Unicode code point value or range of code point values
#    Field 1  Indic_Syllabic_Category property value
##
##
ucd.IndicSyllabicCategory <- read.ucdTbl(
  "ucd/IndicSyllabicCategory.txt"
  , ucd.path ## 
  , cnm = c("cp_lo", "cp_hi", "Indic_Syllabic_Category",  "comments")
  , ncp = 1L
  , cp.drop=F
)

##
##
##
ucd.emoji <- read.ucdTbl(
  "ucd/emoji/emoji-data.txt"
  , ucd.path ## 
  , cnm = c("cp_lo", "cp_hi", "Emoji", "comments")
  , nc = 1L
  , cp.drop=F
)
## 
## ucd/emoji-variation-sequences.txt, cf. infra
##

##
##
##
ucd.grBrk <- read.ucdTbl(
  "ucd/auxiliary/GraphemeBreakProperty.txt"
  , ucd.path ## 
  , cnm = c("cp_lo", "cp_hi", "Grapheme_Cluster_Break", "comments")
  , nc = 1L
  , cp.drop=F
)
##
##
##
ucd.wordBrk <- read.ucdTbl(
  "ucd/auxiliary/WordBreakProperty.txt"
  , ucd.path ## 
  , cnm = c("cp_lo", "cp_hi", "Word_Break", "comments")
  , nc = 1L
  , cp.drop=F
)
##
##
##
ucd.sentBrk <- read.ucdTbl(
  "ucd/auxiliary/SentenceBreakProperty.txt"
  , ucd.path ## 
  , cnm = c("cp_lo", "cp_hi", "Sentence_Break", "comments")
  , nc = 1L
  , cp.drop=F
)
## 
##
##
ucd.lnBrk <- read.ucdTbl(
  "ucd/LineBreak.txt"
  , ucd.path ## 
  , cnm = c("cp_lo", "cp_hi", "Line_Break", "comments")
  , nc = 1L
  , cp.drop=F
)

##
## ucd/DerivedName.txt  
##
# This property is specified by the derivation documented in Section 4.8 of UnicodeData.txt,
# the Jamo_Short_Name property in Jamo.txt, and a specified list of pattern strings used to derive names
# for certain ranges of characters from their code points.
##
ucd.dervName <- read.ucdTbl( ##
  "ucd/extracted/DerivedName.txt"
  , ucd.path ## 
  , cnm = c("cp_lo","cp_hi","name")
  , comm.parse =F
)
##
## ucd/DerivedNumericValues.txt
##
# Field 1:
#    The values are based on field 8 of UnicodeData.txt, plus the fields
#    kAccountingNumeric, kOtherNumeric, kPrimaryNumeric in the Unicode Han Database (Unihan).
#    The derivations for these values are as follows.
#      Numeric_Value = the value of kAccountingNumeric, kOtherNumeric, or kPrimaryNumeric, if they exist; otherwise
#      Numeric_Value = the value of field 8, if it exists; otherwise
#      Numeric_Value = NaN
# Field 2:
#   This field is empty; it used to be a copy of the numeric 
# Field 3:
#   This field was added to this extracted data as of Unicode 5.1.0,
# expressing the same numeric value either as a whole integer
# where possible, or as a rational fraction such as "1/6".
# 
# WARNING: Certain values, such as 0.16666667, are repeating fractions.
# Although they are only printed with a limited number of decimal places
# in this file, they should be expressed to the limits of the precision
# available when used.
## 
##
ucd.dervNumVal <-  read.ucdTbl( ##
  "ucd/extracted/DerivedNumericValues.txt"
  , ucd.path ## 
  , cnm = c("cp_lo","cp_hi","Numeric_Value", "empty" , "Symbolic_Value", "comments")
  , comm.parse = T
)


##
##
##
ucd.StandardizedVariants <- structure(
  within(
    ucdCsvParse(readLines(
      paste(
        ucd.path ## 
        , "ucd/StandardizedVariants.txt"
        , sep = "/"
        ) 
      )
      , cnm=c("cp", "description", "position", "comments")
    )
    ##
    , cp <- strToCpSeq(cp)
  )
  , rpath=  "ucd/StandardizedVariants.txt"
)
##
##
##
ucd.emojiVariation <- structure(
  within(
    ucdCsvParse(
      readLines(paste(
        ucd.path ## 
        , "ucd/emoji/emoji-variation-sequences.txt"
        , sep = "/") 
      )
      , cnm=c("cp", "description", "comments")
      , penult.drop = T
    )
    ##
    , cp <- strToCpSeq(cp)
  )
  , rpath=  "ucd/emoji/emoji-variation-sequences.txt"
)

##
##
##
##
ucd.grBrkTest <- read.brkTest(
  "ucd/auxiliary/GraphemeBreakTest.txt", ucd.path 
)
##
ucd.wordBrkTest <- read.brkTest(
  "ucd/auxiliary/WordBreakTest.txt", ucd.path 
)
##
ucd.sentBrkTest <- read.brkTest(
  "ucd/auxiliary/SentenceBreakTest.txt", ucd.path 
)
##
ucd.lnBrkTest <- read.brkTest(
  "ucd/auxiliary/LineBreakTest.txt", ucd.path 
)

##
## uca/decomps.txt
##
## Unicode Collation Algorithm
##
##
# decompositions used in generating the Default Unicode Collation Element Table
# (DUCET) for the Unicode Collation Algorithm
##
uca.decomps <-read.decomps(
  uca.path=uca.path
)


##
## ivd/IVD_Sequences.txt
##
ivd.seq <- read.ivdSeq(ivd.path = ivd.path)
## 
## ivd/IVD_Stats.txt
##