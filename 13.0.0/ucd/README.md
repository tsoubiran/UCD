# List of UCD files

This a list of all UCD files included in this repo with a link to their description in [UAX#44](https://unicode.org/reports/tr44/). 

Whenever possible, `data.frame` column names match those listed in the PropertyAliases file.

Some `data.frame` are a little different form the original txt files&nbsp;:

UnicodeData&nbsp;: for backward compatibility, ranges in the original UnicodeData.txt file are specified by entries for the start and end characters of the range while ucd.udata has a single range per row. This means we have 

|cp_lo |cp_hi |Name |
|------|------|-----|
|13312|19903|<CJK Ideograph Extension A, First>..<CJK Ideograph Extension A, Last>|

instead of 

|cp |Name |
|---|-----|
|13312|<CJK Ideograph Extension A, First>|
|19903|<CJK Ideograph Extension A, Last>|

PropertyValueAliases&nbsp;: the ucd.propValal `data.frame` has some extra columns extracted form the commented lines

|File Name | `data.frame` Name |
|----------|-------------------|
|[ArabicShaping](https://unicode.org/reports/tr44/#ArabicShaping.txt)|ucd.ArabicShaping|
|[BidiBrackets](https://unicode.org/reports/tr44/#BidiBrackets.txt)|ucd.BidiBrackets|
|[BidiMirroring](https://unicode.org/reports/tr44/#BidiMirroring.txt)|ucd.BidiMirroring|
|[Blocks](https://unicode.org/reports/tr44/#Blocks.txt)|ucd.blk|
|[CaseFolding](https://unicode.org/reports/tr44/#CaseFolding.txt)|ucd.casefold|
|[CJKRadicals](https://unicode.org/reports/tr44/#UCD_Files_Table)|ucd.CJKRadicals|
|[CompositionExclusions](https://unicode.org/reports/tr44/#CompositionExclusions.txt)|ucd.compex|
|[DerivedAge](https://unicode.org/reports/tr44/#DerivedAge.txt)|ucd.age|
|[DerivedCoreProperties](https://unicode.org/reports/tr44/#DerivedCoreProperties.txt)|ucd.dervProp|
|[DerivedNormalizationProps](https://unicode.org/reports/tr44/#DerivedNormalizationProps.txt)|ucd.dervNormProp|
|[EastAsianWidth](https://unicode.org/reports/tr44/#EastAsianWidth.txt)|ucd.EastAsianWidth|
|[EquivalentUnifiedIdeograph](https://unicode.org/reports/tr44/#EquivalentUnifiedIdeograph.txt)|ucd.EquivalentUnifiedIdeograph|
|[HangulSyllableType](https://unicode.org/reports/tr44/#HangulSyllableType.txt)|ucd.HangulSyllableType|
|[IndicPositionalCategory](https://unicode.org/reports/tr44/#IndicPositionalCategory.txt)|ucd.IndicPositionalCategory|
|[IndicSyllabicCategory](https://unicode.org/reports/tr44/#IndicSyllabicCategory.txt)|ucd.IndicSyllabicCategory|
|[Jamo](https://unicode.org/reports/tr44/#Jamo.txt)|ucd.jamo|
|[LineBreak](https://unicode.org/reports/tr44/#LineBreak.txt)|ucd.lnBrk|
|[NameAliases](https://unicode.org/reports/tr44/#NameAliases.txt)|ucd.nameAl|
|[NamedSequences](https://unicode.org/reports/tr44/#UCD_Files_Table)|ucd.namedSeq|
|[NamedSequencesProv](https://unicode.org/reports/tr44/#UCD_Files_Table)|ucd.namedSeqProv|
|[NormalizationCorrections](https://unicode.org/reports/tr44/#NormalizationCorrections.txt)|ucd.NormalizationCorrections|
|[PropertyAliases](https://unicode.org/reports/tr44/#PropertyAliases.txt)|ucd.propAl|
|[PropertyValueAliases](https://unicode.org/reports/tr44/#PropertyValueAliases.txt)|ucd.propValal|
|[PropList](https://unicode.org/reports/tr44/#PropList.txt)|ucd.prop|
|[ScriptExtensions](https://unicode.org/reports/tr44/#ScriptExtensions.txt)|ucd.scriptsExt|
|[Scripts](https://unicode.org/reports/tr44/#Scripts.txt)|ucd.scripts|
|[SpecialCasing](https://unicode.org/reports/tr44/#SpecialCasing.txt)|ucd.SpecialCasing|
|[StandardizedVariants](https://unicode.org/reports/tr44/#UCD_Files_Table)|ucd.StandardizedVariants|
|[UnicodeData](https://unicode.org/reports/tr44/#UnicodeData.txt)|ucd.udata|
|[VerticalOrientation](https://unicode.org/reports/tr44/#VerticalOrientation.txt)|ucd.vertOrient|
|[auxiliary/GraphemeBreakProperty](https://unicode.org/reports/tr44/#GraphemeBreakProperty.txt)|ucd.grBrk|
|[auxiliary/GraphemeBreakTest](https://unicode.org/reports/tr44/#Algorithm_Test_Table)|ucd.grBrkTest|
|[auxiliary/LineBreakTest](https://unicode.org/reports/tr44/#Algorithm_Test_Table)|ucd.lnBrkTest|
|[auxiliary/SentenceBreakProperty](https://unicode.org/reports/tr44/#SentenceBreakProperty.txt)|ucd.sentBrk|
|[auxiliary/SentenceBreakTest](https://unicode.org/reports/tr44/#Algorithm_Test_Table)|ucd.sentBrkTest|
|[auxiliary/WordBreakProperty](https://unicode.org/reports/tr44/#WordBreakProperty.txt)|ucd.wordBrk|
|[auxiliary/WordBreakTest](https://unicode.org/reports/tr44/#Algorithm_Test_Table)|ucd.wordBrkTest|
|[emoji/emoji-data](https://unicode.org/reports/tr44/#emoji-data.txt)|ucd.emoji|
|[emoji/emoji-variation-sequences](https://unicode.org/reports/tr44/#UCD_Files_Table)|ucd.emojiVariation|
|[extracted/DerivedName](https://unicode.org/reports/tr44/#Extracted_Properties_Table)|ucd.dervName|
|[extracted/DerivedNumericValues](https://unicode.org/reports/tr44/#Extracted_Properties_Table)|ucd.dervNumVal|




