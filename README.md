
<!-- README.md is generated from README.Rmd. Please edit that file -->

``` r
library(newprideR)
#> Loading required package: rjson
```

# Introduction

NewprideR allows for exploring the Pride archive through R. NewprideR
has classes and functions for Pride projects, files, peptides, and
proteins. It allows for most search functionality featured on the Pride
Archive website, and a bit more. This makes systematically searching,
and downloading possible solely in R.

All metadata received is obtained through the Pride Swagger UI API,
which can be found here:
<https://www.ebi.ac.uk/pride/ws/archive/v2/swagger-ui.html>

# Installation

First install the package devtools if not installed already, then use
`install_github()`.

    install.package("devtools")
    library(devtools)
    install_github("booker-tremayne/new-prideR")

# Basic Use

## Project Summaries

Project Summaries are how meta data for projects are stored.

`search.ProjectSummary()` is the function to search for projects across
Pride. It can accept:

-   **keywords**: will return projects that feature the given word
    within the entire metadata
-   **filters**: which will only return projects that have a value in a
    given field
-   **page sorting**: specifies how many projects per page, which page
    accessed, and whether it will sort by ascending or descending order.

For example:

``` r
project.list <- search.ProjectSummary(keywords = "imaging", page.size = 2, page.number = 4, instrument = "LTQ Orbitrap")
project.list
#> [[1]]
#> An object of class compactProjectSummary made public in 2020-01-07
#>     Accession: PXD016570
#>     Title: Plasma proteome profiling of freshwater and seawater life stages of rainbow trout (Oncorhynchus mykiss)
#>     Description: The sea-run phenotype of rainbow trout (Oncorhynchus mykiss), like other anadromous salmonids, present a juvenile stage fully adapted to life in freshwater known as parr. Development in freshwater is followed by the smolt stage, where preadaptations needed for seawater life are developed making fish ready to migrate to the ocean, after which event they become post-smolts. While these three life stages have been studied using a variety of approaches, proteomics has never been used for such purpose. The present study characterised the blood plasma proteome of parr, smolt and post-smolt rainbow trout using a gel electrophoresis liquid chromatography tandem mass spectrometry approach alone or in combination with low-abundant protein enrichment technology (combinatorial peptide ligand library). In total, 1,822 proteins were quantified, 17.95% of them being detected only in plasma post enrichment. Across all life stages, the most abundant proteins were ankyrin-2, DNA primase large subunit, actin, serum albumin, apolipoproteins, hemoglobin subunits, hemopexin-like proteins and complement C3. When comparing the different life stages, 17 proteins involved in mechanisms to cope with hyperosmotic stress and retinal changes, as well as the downregulation of nonessential processes in smolts, were significantly different between parr and smolt samples. On the other hand, 11 proteins related to increased growth in post-smolts, and also related to coping with hyperosmotic stress and to retinal changes, were significantly different between smolt and post-smolt samples. Overall, this study presents a series of proteins with the potential to complement current seawater-readiness assessment tests in rainbow trout, which can be measured non-lethally in an easily accessible biofluid. Furthermore, this study represents a first in-depth characterisation of the rainbow trout blood plasma proteome, having considered three life stages of the fish and used both fractionation alone or in combination with enrichment methods to increase protein detection.
#>     Organisms: Oncorhynchus mykiss (rainbow trout) (salmo gairdneri)
#>     Organism Parts: Blood plasma
#>     Instruments: Ltq orbitrap
#>     Lab PIs: Amaya Albalat
#>     Submitters: Bernat Morro
#>     Affiliations: Institute of Aquaculture, Stirling
#> 
#> [[2]]
#> An object of class compactProjectSummary made public in 2019-11-06
#>     Accession: PXD016146
#>     Title: MALDI rat liver anticancer drug spiked-in dataset (imzML)
#>     Description: For the MALDI-MSI experiment, we selected 12 different drugs. The drugs were purchased from the LC Laboratories (Woburn, MA; CAS numbers: dabrafenib: 1195765-45-7, dasatinib: 302962-49-8, erlotinib: 183321-74-6, gefitinib: 184475-35-2, imatinib: 152459-95-5, lapatinib: 388082-78-8, pazopanib: 444731-52-6, sorafenib: 284461-73-0, sunitinib: 557795-19-4, trametinib: 871700-17-3, vatalanib: 212141-54-3) and from SelleckChem (Munich, Germany; CAS numbers: ipratropium: 60205-81-4) with >99% purity and were dissolved in methanol (MeOH, (Chromasolv Plus for HPLC) (Sigma-Aldrich, Steinheim, Germany) at 10 mg/mL concentration. These stock solutions were further diluted with 50% MeOH and five mixtures were generated, each containing four different drug compounds. The spreadsheet in Supporting Information summarizes the composition of the five drug mixtures. A 5 mg/mL solution of a-cyano-4-hydroxycinnamic acid (CHCA, Sigma-Aldrich) dissolved in 50% MeOH containing 0.1% trifluoroacetic acid (TFA, Sigma-Aldrich, Steinheim, Germany) was used as matrix solution.
#>     Organisms: Rattus norvegicus (rat)
#>     Organism Parts: Liver
#>     Instruments: Ltq orbitrap
#>     Lab PIs: Peter Horvatovich
#>     Submitters: Jonatan Eriksson
#>     Affiliations: University of Groningen, Department of Analytical Biochemistry, Groningen Research Institute of Pharmacy, Antonius Deusinglaan 1, 9713 AV Groningen, The Netherlands  Lund University, Department of Biomedical Engineering, Lund, Sweden
```

`get.ProjectSummary()` retrieves more detailed information regarding a
single project. It accepts project accessions.

``` r
first.project <- get.ProjectSummary("PXD000001")
first.project
#> An object of class ProjectSummary made public in 2012-03-07
#>     Accession: PXD000001
#>     Title: TMT spikes -  Using R and Bioconductor for proteomics data analysis
#>     Description: Expected reporter ion ratios: Erwinia peptides:    1:1:1:1:1:1 Enolase spike (sp|P00924|ENO1_YEAST):  10:5:2.5:1:2.5:10 BSA spike (sp|P02769|ALBU_BOVIN):  1:2.5:5:10:5:1 PhosB spike (sp|P00489|PYGM_RABIT):  2:2:2:2:1:1 Cytochrome C spike (sp|P62894|CYC_BOVIN): 1:1:1:1:1:2
#>     Sample Processing Protocol: Not available
#>     Data Processing Protocol: Two extra files have been added post-publication:<br><a href="ftp://ftp.pride.ebi.ac.uk/pride/data/archive/2012/03/PXD000001/TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01-20141210.mzML" target="_top">TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01-20141210.mzML</a><br><a href="ftp://ftp.pride.ebi.ac.uk/pride/data/archive/2012/03/PXD000001/TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01-20141210.mzXML" target="_top">TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01-20141210.mzXML</a>
#>     Tags:  Prime-xs project 
#>     Submission type: COMPLETE
#>     Organism(s): 
#>         Erwinia carotovora  |  Accession: 554
#>     Organism Part(s):  
#>          Not available  |  Accession: Not available
#>     Disease(s):  
#>         Not available  |  Accession: Not available
#>     PTM(s):  
#>         monohydroxylated residue  |  Accession: MOD:00425
#>         TMT6plex-126 reporter+balance reagent acylated residue  |  Accession: MOD:01720
#>         methylthiolated residue  |  Accession: MOD:01153
#>     Instrument(s):  
#>         instrument model  |  Accession: MS:1000031
#>         LTQ Orbitrap Velos  |  Accession: MS:1001742
#>     Quantification Method(s):  
#>         Not available  |  Accession: Not available
#>     Lab PIs:  
#>         Name: Not available Not available  |  Email: Not available
#>         Affiliation:  Not available 
#>     Submitters:  
#>         Name: Dr Laurent Gatto  |  Email: lg390@cam.ac.uk
#>         Affiliation:  Department of Biochemistry, University of Cambridge
```

`list.ProjectSummary()` retrieves projects from newest to oldest, only
accepting page size and page number. `get.similar.projects()` accepts a
project accession and returns projects Pride deems similar.

## File Details and File Detail Lists

File Details store metadata corresponding to files which projects
contain.

File Detail Lists File Details and their associated project projects. It
is intended for lists that contain multiple Project Summaries and their
respective File Details

`get.FileDetail()` is used to get FileDetail data from a project. It
accepts project accessions.

`get.FileDetailList()` gets all the FileDetails from a list of Project
Summaries given to it.

`search.FileDetail()` is the searching function for File Details. It can
accept:

-   **ProjectSummary list/FileDetailList** Can be either object
-   **keywords** : will return projects which contain files with the
    given keywords in their file name. Can be used to check file
    extensions.
-   **a file type** : will return projects which contain files with the
    given file type. File types are categories given by Pride. They are
    as follows ; “RAW”, “SEARCH”, “RESULT”, “PEAK”, “FASTA”,
    “SPECTRUM\_LIBRARY”, “GEL”, “PARAMETERS\_FILE”, “OTHER”
-   **a file size range** : will return projects which contain a file of
    the given size in bytes.
-   **all/any logic** : specifies whether a project must contain files
    with file names of every keyword or a single keyword
-   **use.regex** : specifies whether they keyword given is to be
    interpreted as a regular expression or exactly.

Note that this returns a list of FileDetailList objects.

For example:

``` r
project.list <- search.ProjectSummary(keywords = "imaging", page.size = 2, page.number = 19, instrument = "LTQ Orbitrap")
file.list <- search.FileDetail(project.list, keywords = c(".tif", ".ibd", ".imzml"), all = TRUE)
file.list
#> [[1]]
#> An object of class FileDetailList Containing 4 files
#>    Associated Project: 
#> An object of class compactProjectSummary made public in 2014-11-19
#>     Accession: PXD001283
#>     Title: Mass spectrometry imaging of phospholipids in mouse urinary bladder (imzML dataset)
#>     Description: The spatial distribution of phospholipids in a tissue section of mouse urinary bladder was analyzed by MALDI MS imaging at 10 micrometer pixel size with high mass resolution (using an LTQ Orbitrap mass spectrometer).
#>     Organisms: Mus musculus (mouse)
#>     Organism Parts: Urinary bladder
#>     Instruments: Ltq orbitrap
#>     Tags: Biological
#>     Lab PIs: Bernhard Spengler
#>     Submitters: Andreas Roempp
#>     Affiliations: Institute of Inorganic and Analytical Chemistry, Justus Liebig University Giessen Schubertstrasse 60, D-35392 Giessen Germany
```

`download.files.from.project()` downloads all files from a project.
`download.fileDetail()` downloads a single file corresponding to a File
Detail.

Note that `get.FileDetailList()` will be slow, and `search.FileDetail()`
will be slow if given a Project Summary list and not a FileDetailList.

## Peptide Details

Peptide Details stores metadata for peptides within Pride.

`get.PeptideDetail.accession()` accepts an accession, and returns a list
of peptides assocaited with the given project.

``` r
peptide.list <- get.PeptideDetail.accession("PXD019134", page.size = 2)
peptide.list
#> [[1]]
#> An object of class PeptideDetail
#>    Accession: PXD019134
#>    Peptide Sequence: QPAYMTMKGSALSFQWIEMSSAHSLERNLAK
#>    Protein Accession: gi|215496908-DECOY
#>    Missed Cleavages: 2
#>    Properties: 
#>      distinct peptide-level q-value   |   Value: 1.01835
#>         Accession: MS:1001868
#>      distinct peptide-level FDRScore   |   Value: 0.5093
#>         Accession: MS:1002360
#>      Pass submitter threshold   |   Value: true
#>         Accession: PRIDE:0000511
#>    Quality Methods: 
#>      quality estimation with decoy database   |   Value: false
#>         Accession: MS:1001194
#>    PTMs: 
#>      Not Available   |   Value: Not Available
#>         Accession: Not Available
#> 
#> [[2]]
#> An object of class PeptideDetail
#>    Accession: PXD019134
#>    Peptide Sequence: FIHVAMTPALIQEISLLERK
#>    Protein Accession: NADA_APLKU-DECOY
#>    Missed Cleavages: 1
#>    Properties: 
#>      distinct peptide-level q-value   |   Value: 1.01835
#>         Accession: MS:1001868
#>      distinct peptide-level FDRScore   |   Value: 0.5093
#>         Accession: MS:1002360
#>      Pass submitter threshold   |   Value: true
#>         Accession: PRIDE:0000511
#>    Quality Methods: 
#>      quality estimation with decoy database   |   Value: false
#>         Accession: MS:1001194
#>    PTMs: 
#>      Not Available   |   Value: Not Available
#>         Accession: Not Available
```

`get.list.PeptideDetail()` returns a list of all peptides from Pride,
accepting page size and page number.

## Protein Details

Protein Details stores metadata for proteins within Pride, and is
similar to peptide details

`get.ProteinDetail.accession()` accepts an accession, and returns a list
of peptides assocaited with the given project.

``` r
protein.list <- get.ProteinDetail.accession("PXD019134", page.size = 2)
protein.list
#> [[1]]
#> An object of class ProteinDetail
#>    Accession: PXD019134
#>    Protein Group Members: gi|215496908-DECOY
#>    Protein Sequence: 
#>    Number of Peptides: 1
#>    Number of PSMs: 1
#>    Properties: 
#>      PIA:protein score   |   Value: 0.29352
#>         Accession: MS:1002394
#>      Pass submitter threshold   |   Value: true
#>         Accession: PRIDE:0000511
#>    Quality Methods: 
#>      quality estimation with decoy database   |   Value: false
#>         Accession: MS:1001194
#>    PTMs: 
#>      Not Available   |   Value: Not Available
#>         Accession: Not Available
#> 
#> [[2]]
#> An object of class ProteinDetail
#>    Accession: PXD019134
#>    Protein Group Members: NADA_APLKU-DECOY
#>    Protein Sequence: 
#>    Number of Peptides: 1
#>    Number of PSMs: 1
#>    Properties: 
#>      PIA:protein score   |   Value: 0.29352
#>         Accession: MS:1002394
#>      Pass submitter threshold   |   Value: true
#>         Accession: PRIDE:0000511
#>    Quality Methods: 
#>      quality estimation with decoy database   |   Value: false
#>         Accession: MS:1001194
#>    PTMs: 
#>      Not Available   |   Value: Not Available
#>         Accession: Not Available
```

`get.list.ProteinDetail()` returns a list of all peptides from Pride,
accepting page size and page number.

# Example

We wish to obtain the projects that can be analyzed with *Cardinal*. For
this, we need projects that contain “.ibd” and .“imzml” files. First, we
should obtain all projects that contain the word “imaging” or “msi”.
This is necessary since the file searching method is slow when searching
across several projects. We also wish for all the results to be focused
on breast cancer.

``` r
imaging.list <- search.ProjectSummary(c("imaging", "msi"), page.size = 100, disease = "Breast cancer")
```

Now, we must retrieve only projects containing the file extensions
“.imzml” *and* “.ibd”.

``` r
successful.list <- search.FileDetail(imaging.list, c(".imzml", ".ibd"), all = TRUE)
successful.list
#> [[1]]
#> An object of class FileDetailList Containing 15 files
#>    Associated Project: 
#> An object of class compactProjectSummary made public in 2020-09-07
#>     Accession: PXD019255
#>     Title: Uncovering tumor-stroma inter-relationships using quantitative MALDI-Mass spectrometry imaging
#>     Description: To facilitate analysis of protein expression changes in in situ tumors and stroma, we took advantage of a mouse model that permits conditional activation of the Ser-Thr kinase ROCK within mammary tumor cells. In this study, we undertook MALDI-MSI analysis of tissue samples derived from our conditional ROCK mammary tumor model, to quantify in an unbiased manner, the proteomic changes occurring during the progression of mammary cancers in their specific spatial contexts.
#>     Organisms: Mus musculus (mouse)
#>     Organism Parts: Stem cell
#>     Instruments: Ltq
#>     Diseases: Breast cancer
#>     Lab PIs: Manuela Klingler-Hoffmann
#>     Submitters: Parul Mittal
#>     Affiliations: Research Fellow   Future Industries Institute | University of South Australia ipc MLK-40 | p GPO Box 2471 Adelaide SA 5001
```

Finally, we want an optical image in our projects to analyze as well. To
do so, we may search for projects containing files with the extension
“.jpg” *or* “.jpeg” *or* “.tif” *or* “.png”. We can feed our previous
successful list into the `search.project.list()` function to do so.

``` r
final.list <- search.FileDetail(successful.list, c(".jpg", ".jpeg", ".tif", ".png"))
final.list
#> [[1]]
#> An object of class FileDetailList Containing 15 files
#>    Associated Project: 
#> An object of class compactProjectSummary made public in 2020-09-07
#>     Accession: PXD019255
#>     Title: Uncovering tumor-stroma inter-relationships using quantitative MALDI-Mass spectrometry imaging
#>     Description: To facilitate analysis of protein expression changes in in situ tumors and stroma, we took advantage of a mouse model that permits conditional activation of the Ser-Thr kinase ROCK within mammary tumor cells. In this study, we undertook MALDI-MSI analysis of tissue samples derived from our conditional ROCK mammary tumor model, to quantify in an unbiased manner, the proteomic changes occurring during the progression of mammary cancers in their specific spatial contexts.
#>     Organisms: Mus musculus (mouse)
#>     Organism Parts: Stem cell
#>     Instruments: Ltq
#>     Diseases: Breast cancer
#>     Lab PIs: Manuela Klingler-Hoffmann
#>     Submitters: Parul Mittal
#>     Affiliations: Research Fellow   Future Industries Institute | University of South Australia ipc MLK-40 | p GPO Box 2471 Adelaide SA 5001
```

Now we may choose to download this/these projects.

    download.project.list(final.list, "/users/UserName/exampleDownload")
