# plantMS2

## Introduction

PlantMS2 is a program written and developed independently in our laboratory for large-scale annotation of glycosyl and acyl-glycosyl compounds automatically and systematically from non-targeted metabolomics data obtained based on the UPLC-HRMS platform. The source code is written in R language, and the developing environment is R 3.6.3.

## Development

The operation of the PlantMS2 is mainly divided into the following steps:

1. The generation of built-in glycosyl/acyl-glycosyl MS/MS fragmentation library based on user-defined sugars and acids.
2. The large-scale annotation of glycosyl and acyl-glycosyl moieties automatically and systematically from non-targeted metabolomics data obtained based on the UPLC-HRMS platform.
3. Integration of the MS/MS data of the annotated glycosides and [peak table-based](https://www.sciencedirect.com/science/article/pii/S0021967315003684) MS1 and tR information.
4. Aglycone assignments of the aglycone ions by searching the **in silico** aglycone library.
5. The output of the annotation result of the candidate glycosides list.
6. Extraction of the glycosides-related MS/MS data from the original non-targeted metabolomics data by the unique identifier “ScanNum” in MS/MS data.
7. MS/MS-based chemical classification of glycosides by Canopus (Operate separately by built-in software Canopus in Sirius 4)
8. The calculation of the MS/MS similarity for glycosides and corresponding aglycones.

## How to use

To install plantMS2, you need to install devtools first, and use the following code in R language:

```R
install.packages("devtools")
```

After installing "devtools", install "plantMS2" package using the code below:

```R
devtools::install_github("zhengfj1994/plantMS2")
```

After successful installation, you can use "plantMS2" function to make the large-scale annotation.

 

The packages of “stringr”, “gtools”, “xlsx”, “tcltk” need to be installed in advance. (In the “Tools>>Install packages” menu, enter the names of the four dependent packages to install.

 

The running instructions of plantMS2 are as follows:

```R
library(plantMS2)

NLmatrix <- createNLmatrix(sugarFile="F:\\Sugar and Acid used for neutral loss-PlantMS2.xlsx", numOfSugar=4)

NLmatrix <- NLmatrix[which( 1 <= (as.data.frame(NLmatrix)$Hex + as.data.frame(NLmatrix)$dHex + as.data.frame(NLmatrix)$Pen+ as.data.frame(NLmatrix)$HexA) & as.data.frame(NLmatrix)$Mal <= 1 & as.data.frame(NLmatrix)$Caf <= 1 & as.data.frame(NLmatrix)$DDMP <= 1 & as.data.frame(NLmatrix)$Cou <= 1 & as.data.frame(NLmatrix)$Fur <= 1 & as.data.frame(NLmatrix)$Sin <= 1 & (as.data.frame(NLmatrix)$Mal + as.data.frame(NLmatrix)$Cou + as.data.frame(NLmatrix)$Fur + as.data.frame(NLmatrix)$Caf + as.data.frame(NLmatrix)$Sin + as.data.frame(NLmatrix)$DDMP) <= 1), ]

precusorAndProductPair <- createPrecusorProductPair(mgfFile="F:\\pos-standards-GNPS-583.mgf",intThreshold = 0.01)

selectNLresult <- selectNL(sugarFile = "F:\\Sugar and Acid used for neutral loss-PlantMS2.xlsx", precusorAndProductPair,NLmatrix,deltaMZppm=10,ionMode = "P")

selectNLfliterResult <- selectNLfliter(selectNLresult, ratioThreshold = 0.001)

write.csv(selectNLfliterResult,file = "F:\\selectNLfliterResult-pos-standards-GNPS-583.csv")

ms1ms2ConbRes <- ms1ms2Conbination(ms1File="F:\\peak table-pos-GNPS-583.xlsx", selectNLfliterResult, deltaMZppm=10, deltaTR=0.2)

write.csv(ms1ms2ConbRes,file = "F:\\ms1ms2ConbinationResult-pos-GNPS-583.csv")

ms1IdentificationRes <- ms1Identification(ms1ms2ConbRes, aglyconeFile="F:\\aglyconeDB-pos-standards-GNPS-583.xlsx", ionMode='P', deltaMZppm=10)

write.csv(ms1IdentificationRes, file = "F:\\ms1IdentificationRes-pos-GNPS-583.csv")
```

 

The meaning of the parameters is shown below:

| Parameters     | Meaning                                                      |
| -------------- | ------------------------------------------------------------ |
| sugarFile      | User-defined sugars and acids file, including “class”, “name”, “formula” and “molecular weight” of sugars and acids (.xlsx) |
| numOfSugar     | The maximum number of sugar in glycosyl involved in computation, the default value is 6 (.xlsx) |
| mgfFile        | MS2 file (.mgf)                                              |
| intThreshold   | The smallest relative intensity of product ion (have been normalized by the biggest fragment, the default value is 0.01) |
| ms1File        | Peaktable file (.xlsx)                                       |
| deltaMZppm(1)  | m/z tolerance of built-in glycosyl/acyl-glycosyl MS/MS fragmentation library searching (the default value is 10 ppm) |
| deltaTR        | Retention time tolerance between MS1 and corresponding MS1 in MS2 data(the default value is 0.2 min) |
| aglyconeFile   | **in silico** aglycone library file (.xlsx)                  |
| ionMode        | Positive ion mode = “P”, Negative ion mode = “N”             |
| deltaMZppm(2） | m/z tolerance for **in silico** aglycone databases searching (the default value is 10 ppm) |
| file-1         | Output file of the result of annotation of sugar/acid moiety assignments |
| file-2         | Output file of the matching result of the annotated MS/MS spectra and peak table |
| file-3         | Output file of the annotated result of glycosides            |

**Notice!**

All user-defined files should follow the format as described in the sample data:

1. The generation of built-in glycosyl/acyl-glycosyl MS/MS fragmentation library based on user-defined sugars and acids (as provided in “Sugar used for neutral loss-PlantMS2.xlsx” in sample data)
2. The large-scale annotation of glycosyl and acyl-glycosyl moieties automatically and systematically from non-targeted metabolomics data obtained based on the UPLC-HRMS platform (as provided in “neg-1-398-35CE.mgf” in sample data converted by ProteoWizard ((filter: zeroSamples removeExtra 1–2)
3. The output of the annotation result of glycosyl and acyl-glycosyl moieties (file1, “selectNLfliterResult-neg-1-398-35CE.csv”)
4. Integration of the MS/MS data of the annotated glycosides with [peak table-based](https://www.sciencedirect.com/science/article/pii/S0021967315003684) MS1 and tR information (as provided in “Peak table-neg-1-398-35CE.xlsx” in sample data)
5. Output the result of the integration of the MS/MS data of the annotated glycosides and [peak table-based](https://www.sciencedirect.com/science/article/pii/S0021967315003684) MS1 and tR information (file2, “ms1ms2ConbinationResult-neg-1-398-35CE.csv”)
6. Aglycone assignments of the aglycone ions by searching the **in****-silico** aglycone library (as provided in **in****-silico** aglyconeDB.xlsx in sample data)
7. The output of the result of the candidate glycosides list (file3, “ms1IdentificationRes-neg-1-398-35CE.csv”)
8. Extraction of the glycosides-related MS/MS data from the original non-targeted metabolomics data by the unique identifier “scanNum” from file3(Based on the script of “Exact_scan_in_mgf.R” and “Scan.txt”)
9. MS/MS-based chemical classification of glycosides by Canopus (Operate separately by built-in software Canopus in Sirius 4)
10. The calculation of the MS/MS similarity of glycosides and corresponding aglycones (Based on the script of “MSMS Similarity of glycosides and aglycones.R”, the format of the input file should follow the format as in the sample data of “Sim.xlsx” and “neg1-509.mgf”)

## Codes for MS/MS extraction

We provide the codes for extraction of the glycosides-related MS/MS data from the original non-targeted MS/MS by the unique identifier “Scan” in MS/MS. (Based on the script of Exact_scan_in_mgf.R”, the format of the input file follows the format as the sample data of “Scan.txt”, the scan number of all glycosides came from the result of the “scanNum” column in file 3.

## Codes for computation of MS/MS similarity

We provide the codes for the script to calculate the MS/MS similarity of glycosides and their corresponding aglycones (Based on the script of “MSMS Similarity of glycosides and aglycones.R”, the format of the input file follows the format followed the sample data of “Sim.xlsx” and “neg1-509.mgf”)

| Parameters        | Meaning                                                      |
| ----------------- | ------------------------------------------------------------ |
| mgfFile           | In the mgf file of the glycosides, the MS/MS of each glycoside is identified by the unique identifier “ScanNum” |
| xlsxFile          | Sheet 1:The corresponding relation of the glycosides and aglycones(column B is the scan number of each glycoside) |
| ionMode           | Positive ion mode = “P”, Negative ion mode = “N”             |
| intThreshold1_Exp | The smallest relative intensity of product ion of the glycoside in mgfFile (has been normalized by the biggest fragment, the default value is 0.005) |
| intThreshold2_DB  | The smallest relative intensity of product ion of the aglycone in Sheet 2 (has been normalized by the biggest fragment, the default value in ESI- is 0.01, in ESI+ is 0.03) |

## Contact us

If you have any problems when you use plantMS2. Please contact us, you can send an email to zhangxiuqiong@dicp.ac.cn