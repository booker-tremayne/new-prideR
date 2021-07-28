
<!-- README.md is generated from README.Rmd. Please edit that file -->

``` r
library(newprideR)
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

# Basic Use

## Project Summaries

Project Summaries is how meta data for projects are stored.

`search.ProjectSummary()` is the function to search for projects across
Pride. It can accept:

-   **keywords**: will return projects that feature the given word
    within the entire metadata
-   **filters**: which will only return projects that have a value in a
    given field
-   **Various Sorting methods**: which can specify how many elements per
    page, which page, and whether all keywords need to be matched to
    return a project or any keyword.

For example:

``` r
project.list <- search.ProjectSummary(keywords = "imaging", page.size = 2, page.number = 4, all = FALSE, filter = "instruments==LTQ Orbitrap")
project.list
#> [[1]]
#> An object of class compactProjectSummary made public in 2020-01-07
#>     Accession: PXD016570
#>     Title: Plasma proteome profiling of freshwater and seawater life stages of rainbow trout (Oncorhynchus mykiss)
#>     Description: The sea-run phenotype of rainbow trout (Oncorhynchus mykiss), like other anadromous salmonids, present a juvenile stage fully adapted to life in freshwater known as parr. Development in freshwater is followed by the smolt stage, where preadaptations needed for seawater life are developed making fish ready to migrate to the ocean, after which event they become post-smolts. While these three life stages have been studied using a variety of approaches, proteomics has never been used for such purpose. The present study characterised the blood plasma proteome of parr, smolt and post-smolt rainbow trout using a gel electrophoresis liquid chromatography tandem mass spectrometry approach alone or in combination with low-abundant protein enrichment technology (combinatorial peptide ligand library). In total, 1,822 proteins were quantified, 17.95% of them being detected only in plasma post enrichment. Across all life stages, the most abundant proteins were ankyrin-2, DNA primase large subunit, actin, serum albumin, apolipoproteins, hemoglobin subunits, hemopexin-like proteins and complement C3. When comparing the different life stages, 17 proteins involved in mechanisms to cope with hyperosmotic stress and retinal changes, as well as the downregulation of nonessential processes in smolts, were significantly different between parr and smolt samples. On the other hand, 11 proteins related to increased growth in post-smolts, and also related to coping with hyperosmotic stress and to retinal changes, were significantly different between smolt and post-smolt samples. Overall, this study presents a series of proteins with the potential to complement current seawater-readiness assessment tests in rainbow trout, which can be measured non-lethally in an easily accessible biofluid. Furthermore, this study represents a first in-depth characterisation of the rainbow trout blood plasma proteome, having considered three life stages of the fish and used both fractionation alone or in combination with enrichment methods to increase protein detection.
#>     Organisms:  Oncorhynchus mykiss (rainbow trout) (salmo gairdneri) 
#>     Organism Parts:  Blood plasma 
#>     PTMs:  Not Available 
#>     Instruments:  Ltq orbitrap 
#>     Tags:  Not Available 
#>     Submission type: Not Available
#>     Lab PIs: Amaya Albalat
#>     Submitters: Bernat Morro
#>     Affiliations: Institute of Aquaculture, Stirling
#> 
#> [[2]]
#> An object of class compactProjectSummary made public in 2019-11-06
#>     Accession: PXD016146
#>     Title: MALDI rat liver anticancer drug spiked-in dataset (imzML)
#>     Description: For the MALDI-MSI experiment, we selected 12 different drugs. The drugs were purchased from the LC Laboratories (Woburn, MA; CAS numbers: dabrafenib: 1195765-45-7, dasatinib: 302962-49-8, erlotinib: 183321-74-6, gefitinib: 184475-35-2, imatinib: 152459-95-5, lapatinib: 388082-78-8, pazopanib: 444731-52-6, sorafenib: 284461-73-0, sunitinib: 557795-19-4, trametinib: 871700-17-3, vatalanib: 212141-54-3) and from SelleckChem (Munich, Germany; CAS numbers: ipratropium: 60205-81-4) with >99% purity and were dissolved in methanol (MeOH, (Chromasolv Plus for HPLC) (Sigma-Aldrich, Steinheim, Germany) at 10 mg/mL concentration. These stock solutions were further diluted with 50% MeOH and five mixtures were generated, each containing four different drug compounds. The spreadsheet in Supporting Information summarizes the composition of the five drug mixtures. A 5 mg/mL solution of a-cyano-4-hydroxycinnamic acid (CHCA, Sigma-Aldrich) dissolved in 50% MeOH containing 0.1% trifluoroacetic acid (TFA, Sigma-Aldrich, Steinheim, Germany) was used as matrix solution.
#>     Organisms:  Rattus norvegicus (rat) 
#>     Organism Parts:  Liver 
#>     PTMs:  Not Available 
#>     Instruments:  Ltq orbitrap 
#>     Tags:  Not Available 
#>     Submission type: Not Available
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
#>          Not Available  |  Accession: Not Available
#>     Disease(s):  
#>         Not Available  |  Accession: Not Available
#>     PTM(s):  
#>         monohydroxylated residue  |  Accession: MOD:00425
#>         TMT6plex-126 reporter+balance reagent acylated residue  |  Accession: MOD:01720
#>         methylthiolated residue  |  Accession: MOD:01153
#>     Instrument(s):  
#>         instrument model  |  Accession: MS:1000031
#>         LTQ Orbitrap Velos  |  Accession: MS:1001742
#>     Quantification Method(s):  
#>         Not Available  |  Accession: Not Available
#>     Lab PIs:  
#>         Name:  Not Available  |  Email: 
#>         Affiliation:  
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

Note that this returns a list of FileDetailList objects. Also note that
this is case sensitive.

For example:

``` r
project.list <- search.ProjectSummary(keywords = "imaging", page.size = 2, page.number = 19, all = FALSE, filter = "instruments==LTQ Orbitrap")
file.list <- search.FileDetail(project.list, keywords = c(".tif", ".ibd", ".imzml"), all = TRUE)
file.list
#> [[1]]
#> An object of class FileDetailList Containing 4 files
#>    Associated Project: 
#> An object of class compactProjectSummary made public in 2014-11-19
#>     Accession: PXD001283
#>     Title: Mass spectrometry imaging of phospholipids in mouse urinary bladder (imzML dataset)
#>     Description: The spatial distribution of phospholipids in a tissue section of mouse urinary bladder was analyzed by MALDI MS imaging at 10 micrometer pixel size with high mass resolution (using an LTQ Orbitrap mass spectrometer).
#>     Organisms:  Mus musculus (mouse) 
#>     Organism Parts:  Urinary bladder 
#>     PTMs:  Not Available 
#>     Instruments:  Ltq orbitrap 
#>     Tags:  Biological 
#>     Submission type: Not Available
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
across several projects.

``` r
imaging.list <- search.ProjectSummary(c("imaging", "msi"), page.size = 100, filters = "Alzheimer&apos;s disease")
```

Now, we must retrieve only projects containing the file extensions
“.imzml” *and* “.ibd”.

``` r
successful.list <- search.FileDetail(imaging.list, c(".imzml", ".ibd"), all = TRUE)
successful.list
#> [[1]]
#> An object of class FileDetailList Containing 18 files
#>    Associated Project: 
#> An object of class compactProjectSummary made public in 2021-06-04
#>     Accession: PXD026459
#>     Title: Tryptic peptide imaging of an urothelial cancer tissue cohort
#>     Description: Analysis of a clinical urothelial cancer cohort for their spatial tryptic peptide composition in two different tissue types, tumor and stroma, and two tumor subtypes, muscle-infiltrating and non muscle-infiltrating tumors.
#>     Organisms:  Homo sapiens (human) 
#>     Organism Parts:  Urinary bladder 
#>     PTMs:  Not Available 
#>     Instruments:  4800 proteomics analyzer 
#>     Tags:  Not Available 
#>     Submission type: Not Available
#>     Lab PIs: Oliver Schilling
#>     Submitters: Melanie Christine Föll
#>     Affiliations: Institute for Surgical Pathology, Medical Center and Faculty of Medicine - University of Freiburg, Freiburg, Germany
#> 
#> 
#> [[2]]
#> An object of class FileDetailList Containing 92 files
#>    Associated Project: 
#> An object of class compactProjectSummary made public in 2021-05-17
#>     Accession: PXD025486
#>     Title: Mouse brain MALDI-imaging proteomics profiling
#>     Description: We developed and validated ‘HIT-MAP’ (High-resolution Informatics Toolbox in MALDI-MSI Proteomics), an open-source bioinformatics workflow using peptide mass fingerprint analysis and a dual scoring system to computationally assign peptide and protein annotations to high mass resolution MSI datasets, and generate customisable spatial distribution maps. The uploaded files are an example dataset for the HiTMaP proteomics search engine, designed for MALDI-imaging proteomics annotation. The example data files contain one bovine lens tissue section and one mouse brain tissue section. The ID folder contains the protein/peptide identification result for each tissue segment, and the summary folder contains the protein cluster images.
#>     Organisms:  Bos taurus (bovine) 
#>     Organism Parts:  Brain 
#>     PTMs:  Not Available 
#>     Instruments:  Bruker daltonics solarix series 
#>     Tags:  Not Available 
#>     Submission type: Not Available
#>     Lab PIs: GUS GREY
#>     Submitters: GUANGYU GUO
#>     Affiliations: Department of Physiology,  the University of Auckland
#> 
#> 
#> [[3]]
#> An object of class FileDetailList Containing 44 files
#>    Associated Project: 
#> An object of class compactProjectSummary made public in 2020-10-21
#>     Accession: PXD021275
#>     Title: N-glycomic signature of stage II colorectal cancer and its association with the tumor microenvironment
#>     Description: The choice for adjuvant chemotherapy in stage II colorectal cancer (CRC) is controversial as many patients are cured by surgery alone and it is difficult to identify patients with high-risk of recurrence of the disease. There is a need for better stratification of this group of patients. Mass spectrometry imaging could identify patients at risk. We report here the N-glycosylation signatures of the different cell populations in a group of stage II CRC tissue samples.
#>     Organisms:  Homo sapiens (human) 
#>     Organism Parts:  Colon 
#>     PTMs:  Not Available 
#>     Instruments:  Bruker daltonics flex series 
#>     Tags:  Not Available 
#>     Submission type: Not Available
#>     Lab PIs: Heijs, Bram
#>     Submitters: Fanny Boyaval
#>     Affiliations: Center of Proteomics & metabolomics, Leiden University medical Center, Leiden, The netherlands
#> 
#> 
#> [[4]]
#> An object of class FileDetailList Containing 16 files
#>    Associated Project: 
#> An object of class compactProjectSummary made public in 2020-10-15
#>     Accession: PXD020824
#>     Title: Analysis of Amyloid Plaque Composition in two mouse models of Alzheimer’s disease
#>     Description: MALDI mass spectrometry imaging (MSI) enables label-free, spatially resolved analysis of a wide range of analytes in tissue sections. Quantitative analysis of MSI datasets is typically performed on single pixels or manually assigned regions of interest (ROI). However, many sparse, small objects such as Alzheimer’s disease (AD) brain deposits of amyloid peptides called plaques are neither single pixels nor ROI. Here, we propose a new approach to facilitate comparative computational evaluation of amyloid plaque-like objects by MSI: a fast PLAQUE PICKER tool that enables statistical evaluation of heterogeneous amyloid peptide composition. Comparing two AD mouse models, APP NL-G-F and APP PS1, we identified distinct heterogeneous plaque populations in the NL-G-F model, but only one class of plaques in the PS1 model. We propose quantitative metrics for the comparison of technical and biological MSI replicates.
#>     Organisms:  Mus musculus (mouse) 
#>     Organism Parts:  Brain 
#>     PTMs:  Not Available 
#>     Instruments:  Bruker daltonics flex series 
#>     Tags:  Not Available 
#>     Submission type: Not Available
#>     Lab PIs: Carsten Hopf
#>     Submitters: Thomas Enzlein
#>     Affiliations: Center for Mass Spectrometry and Optical Spectroscopy (CeMOS), Mannheim University of Applied Sciences, Paul-Wittsack Str. 10, 68163, Mannheim, Germany.
#> 
#> 
#> [[5]]
#> An object of class FileDetailList Containing 34 files
#>    Associated Project: 
#> An object of class compactProjectSummary made public in 2020-08-06
#>     Accession: PXD011104
#>     Title: Tissue protease activity with MALDI MSI
#>     Description: Aberrant protease activity has been implicated in the etiology of various prevalent diseases including neurodegeneration and cancer, in particular metastasis. Matrix-assisted laser desorption/ionization (MALDI) mass spectrometry imaging (MSI) has recently been established as a key technology for bioanalysis of multiple biomolecular classes such as proteins, lipids, and glycans. However, it has not yet been systematically explored for investigation of a tissue’s endogenous protease activity. In this study, we demonstrate that different tissues, spray-coated with substance P as a tracer, digest this peptide with different time-course profiles. Furthermore, we reveal that distinct cleavage products originating from substance P are generated transiently and that proteolysis can be attenuated by protease inhibitors in a concentration-dependent manner. To show the translational potential of the method, we analyzed protease activity of gastric carcinoma in mice. Our MSI and quantitative proteomics results reveal differential distribution of protease activity – with strongest activity being observed in mouse tumor tissue, suggesting the general applicability of the workflow in animal pharmacology and clinical studies.Aberrant protease activity has been implicated in the etiology of various prevalent diseases including neurodegeneration and cancer, in particular metastasis. Matrix-assisted laser desorption/ionization (MALDI) mass spectrometry imaging (MSI) has recently been established as a key technology for bioanalysis of multiple biomolecular classes such as proteins, lipids, and glycans. However, it has not yet been systematically explored for investigation of a tissue’s endogenous protease activity. In this study, we demonstrate that different tissues, spray-coated with substance P as a tracer, digest this peptide with different time-course profiles. Furthermore, we reveal that distinct cleavage products originating from substance P are generated transiently and that proteolysis can be attenuated by protease inhibitors in a concentration-dependent manner. To show the translational potential of the method, we analyzed protease activity of gastric carcinoma in mice. Our MSI and quantitative proteomics results reveal differential distribution of protease activity – with strongest activity being observed in mouse tumor tissue, suggesting the general applicability of the workflow in animal pharmacology and clinical studies.
#>     Organisms:  Bos taurus (bovine) 
#>     Organism Parts:  Spleen 
#>     PTMs:  Not Available 
#>     Instruments:  Bruker daltonics solarix series 
#>     Tags:  Technical 
#>     Submission type: Not Available
#>     Lab PIs: Prof. Dr. Carsten Hopf
#>     Submitters: Katrin Erich
#>     Affiliations: CeMOS HS Mannheim
#> 
#> 
#> [[6]]
#> An object of class FileDetailList Containing 25 files
#>    Associated Project: 
#> An object of class compactProjectSummary made public in 2020-11-27
#>     Accession: PXD019425
#>     Title: Altered N-linked Glycosylation in Endometrial Cancer
#>     Description: It has been recognised for a long time that cell surface glycans plays a vital role in the biological process and their altered form can lead to cancer. However, the molecular details and regulatory mechanism between the glycans and cancer is yet not clear. Over the past decade mass spectrometry-based techniques has become a prominent method for analysing glycans especially N-glycan matrix assisted laser desorption/ionisation mass spectrometry imaging (MALDI MSI). It is a powerful technique that combines mass spectrometry with histology, enabling the visualisation and label free detection of N-linked glycans on a single tissue section. Here, we carried out N-glycan MALDI MSI on six endometrial cancer (EC) formalin fixed paraffin embedded (FFPE) tissue sections including the adjacent normal endometrium, and tissue microarrays (TMA) consisting of 8 EC patients with lymph node metastasis (LNM) and 20 without LNM. By doing that several m/z values were detected that can significantly distinguish normal endometrium from cancerous. Also, detected a m/z value that can discriminate the primary tumour with LNM from those without. Identification of those discriminative m/z values was performed using porous graphitized carbon liquid chromatography tandem mass spectrometry (PGC-LC-MS/MS). Overall, we observed higher abundance of oligomannose in tumour regions compared to normal with AUC ranges from 0.85-0.99. Whereas, complex N-glycans were detected in lower abundance with AUC ranges from 0.03-0.28. Comparison of N-glycans between the primary tumours with LNM and without LNM indicates reduced abundance of a complex core-fucosylated N-glycan in patients with metastasis relative to without. In summary, N-glycan MALDI MSI can be used to characterize the cancerous endometrium from the normal, also patients with LNM from those without. Identification of those discriminative m/z values was performed using porous graphitized carbon liquid chromatography tandem mass spectrometry (PGC-LC-MS/MS).
#>     Organisms:  Homo sapiens (human) 
#>     Organism Parts:  Uterus 
#>     PTMs:  Not Available 
#>     Instruments:  Ultraflex 
#>     Tags:  Not Available 
#>     Submission type: Not Available
#>     Lab PIs: Prof Peter Hoffmann
#>     Submitters: Parul Mittal
#>     Affiliations: Future Industries Institute, Mawson Lakes Campus, University of South Australia, Adelaide, South Australia, 5095
#> 
#> 
#> [[7]]
#> An object of class FileDetailList Containing 15 files
#>    Associated Project: 
#> An object of class compactProjectSummary made public in 2020-09-07
#>     Accession: PXD019255
#>     Title: Uncovering tumor-stroma inter-relationships using quantitative MALDI-Mass spectrometry imaging
#>     Description: To facilitate analysis of protein expression changes in in situ tumors and stroma, we took advantage of a mouse model that permits conditional activation of the Ser-Thr kinase ROCK within mammary tumor cells. In this study, we undertook MALDI-MSI analysis of tissue samples derived from our conditional ROCK mammary tumor model, to quantify in an unbiased manner, the proteomic changes occurring during the progression of mammary cancers in their specific spatial contexts.
#>     Organisms:  Mus musculus (mouse) 
#>     Organism Parts:  Stem cell 
#>     PTMs:  Not Available 
#>     Instruments:  Ltq 
#>     Tags:  Not Available 
#>     Submission type: Not Available
#>     Lab PIs: Manuela Klingler-Hoffmann
#>     Submitters: Parul Mittal
#>     Affiliations: Research Fellow   Future Industries Institute | University of South Australia ipc MLK-40 | p GPO Box 2471 Adelaide SA 5001
#> 
#> 
#> [[8]]
#> An object of class FileDetailList Containing 13 files
#>    Associated Project: 
#> An object of class compactProjectSummary made public in 2019-12-11
#>     Accession: PXD014310
#>     Title: Egg White as a quality control in MALDI Mass Spectrometry Imaging
#>     Description: Tryptic peptides and N-glycans were spatially mapped and visualised on formalin-fixed paraffin-embedded (FFPE) endometrial cancer tissue microarrays (TMAs) using an ultrafleXtreme MALDI-ToF/ToF MS instrument (Bruker Daltonics). FFPE egg white was placed either side of each TMA and used as an external control to monitor detector performance and sample preparation.
#>     Organisms:  Gallus gallus (chicken) 
#>     Organism Parts:  Egg 
#>     PTMs:  Not Available 
#>     Instruments:  Ultraflex 
#>     Tags:  Not Available 
#>     Submission type: Not Available
#>     Lab PIs: Peter Hoffmann
#>     Submitters: Matthew Briggs
#>     Affiliations: Future Industries Institute, The Univrsity of South Australia
#> 
#> 
#> [[9]]
#> An object of class FileDetailList Containing 5 files
#>    Associated Project: 
#> An object of class compactProjectSummary made public in 2019-11-06
#>     Accession: PXD016146
#>     Title: MALDI rat liver anticancer drug spiked-in dataset (imzML)
#>     Description: For the MALDI-MSI experiment, we selected 12 different drugs. The drugs were purchased from the LC Laboratories (Woburn, MA; CAS numbers: dabrafenib: 1195765-45-7, dasatinib: 302962-49-8, erlotinib: 183321-74-6, gefitinib: 184475-35-2, imatinib: 152459-95-5, lapatinib: 388082-78-8, pazopanib: 444731-52-6, sorafenib: 284461-73-0, sunitinib: 557795-19-4, trametinib: 871700-17-3, vatalanib: 212141-54-3) and from SelleckChem (Munich, Germany; CAS numbers: ipratropium: 60205-81-4) with >99% purity and were dissolved in methanol (MeOH, (Chromasolv Plus for HPLC) (Sigma-Aldrich, Steinheim, Germany) at 10 mg/mL concentration. These stock solutions were further diluted with 50% MeOH and five mixtures were generated, each containing four different drug compounds. The spreadsheet in Supporting Information summarizes the composition of the five drug mixtures. A 5 mg/mL solution of a-cyano-4-hydroxycinnamic acid (CHCA, Sigma-Aldrich) dissolved in 50% MeOH containing 0.1% trifluoroacetic acid (TFA, Sigma-Aldrich, Steinheim, Germany) was used as matrix solution.
#>     Organisms:  Rattus norvegicus (rat) 
#>     Organism Parts:  Liver 
#>     PTMs:  Not Available 
#>     Instruments:  Ltq orbitrap 
#>     Tags:  Not Available 
#>     Submission type: Not Available
#>     Lab PIs: Peter Horvatovich
#>     Submitters: Jonatan Eriksson
#>     Affiliations: University of Groningen, Department of Analytical Biochemistry, Groningen Research Institute of Pharmacy, Antonius Deusinglaan 1, 9713 AV Groningen, The Netherlands  Lund University, Department of Biomedical Engineering, Lund, Sweden
```

Finally, we want an optical image in our projects to analyze as well. To
do so, we may search for projects containing files with the extension
“.jpg” *or* “.jpeg” *or* “.tif” *or* “.png”. We can feed our previous
successful list into the `search.project.list()` function to do so.

``` r
final.list <- search.FileDetail(successful.list, c(".jpg", ".jpeg", ".tif", ".png"))
final.list
#> [[1]]
#> An object of class FileDetailList Containing 18 files
#>    Associated Project: 
#> An object of class compactProjectSummary made public in 2021-06-04
#>     Accession: PXD026459
#>     Title: Tryptic peptide imaging of an urothelial cancer tissue cohort
#>     Description: Analysis of a clinical urothelial cancer cohort for their spatial tryptic peptide composition in two different tissue types, tumor and stroma, and two tumor subtypes, muscle-infiltrating and non muscle-infiltrating tumors.
#>     Organisms:  Homo sapiens (human) 
#>     Organism Parts:  Urinary bladder 
#>     PTMs:  Not Available 
#>     Instruments:  4800 proteomics analyzer 
#>     Tags:  Not Available 
#>     Submission type: Not Available
#>     Lab PIs: Oliver Schilling
#>     Submitters: Melanie Christine Föll
#>     Affiliations: Institute for Surgical Pathology, Medical Center and Faculty of Medicine - University of Freiburg, Freiburg, Germany
#> 
#> 
#> [[2]]
#> An object of class FileDetailList Containing 92 files
#>    Associated Project: 
#> An object of class compactProjectSummary made public in 2021-05-17
#>     Accession: PXD025486
#>     Title: Mouse brain MALDI-imaging proteomics profiling
#>     Description: We developed and validated ‘HIT-MAP’ (High-resolution Informatics Toolbox in MALDI-MSI Proteomics), an open-source bioinformatics workflow using peptide mass fingerprint analysis and a dual scoring system to computationally assign peptide and protein annotations to high mass resolution MSI datasets, and generate customisable spatial distribution maps. The uploaded files are an example dataset for the HiTMaP proteomics search engine, designed for MALDI-imaging proteomics annotation. The example data files contain one bovine lens tissue section and one mouse brain tissue section. The ID folder contains the protein/peptide identification result for each tissue segment, and the summary folder contains the protein cluster images.
#>     Organisms:  Bos taurus (bovine) 
#>     Organism Parts:  Brain 
#>     PTMs:  Not Available 
#>     Instruments:  Bruker daltonics solarix series 
#>     Tags:  Not Available 
#>     Submission type: Not Available
#>     Lab PIs: GUS GREY
#>     Submitters: GUANGYU GUO
#>     Affiliations: Department of Physiology,  the University of Auckland
#> 
#> 
#> [[3]]
#> An object of class FileDetailList Containing 16 files
#>    Associated Project: 
#> An object of class compactProjectSummary made public in 2020-10-15
#>     Accession: PXD020824
#>     Title: Analysis of Amyloid Plaque Composition in two mouse models of Alzheimer’s disease
#>     Description: MALDI mass spectrometry imaging (MSI) enables label-free, spatially resolved analysis of a wide range of analytes in tissue sections. Quantitative analysis of MSI datasets is typically performed on single pixels or manually assigned regions of interest (ROI). However, many sparse, small objects such as Alzheimer’s disease (AD) brain deposits of amyloid peptides called plaques are neither single pixels nor ROI. Here, we propose a new approach to facilitate comparative computational evaluation of amyloid plaque-like objects by MSI: a fast PLAQUE PICKER tool that enables statistical evaluation of heterogeneous amyloid peptide composition. Comparing two AD mouse models, APP NL-G-F and APP PS1, we identified distinct heterogeneous plaque populations in the NL-G-F model, but only one class of plaques in the PS1 model. We propose quantitative metrics for the comparison of technical and biological MSI replicates.
#>     Organisms:  Mus musculus (mouse) 
#>     Organism Parts:  Brain 
#>     PTMs:  Not Available 
#>     Instruments:  Bruker daltonics flex series 
#>     Tags:  Not Available 
#>     Submission type: Not Available
#>     Lab PIs: Carsten Hopf
#>     Submitters: Thomas Enzlein
#>     Affiliations: Center for Mass Spectrometry and Optical Spectroscopy (CeMOS), Mannheim University of Applied Sciences, Paul-Wittsack Str. 10, 68163, Mannheim, Germany.
#> 
#> 
#> [[4]]
#> An object of class FileDetailList Containing 34 files
#>    Associated Project: 
#> An object of class compactProjectSummary made public in 2020-08-06
#>     Accession: PXD011104
#>     Title: Tissue protease activity with MALDI MSI
#>     Description: Aberrant protease activity has been implicated in the etiology of various prevalent diseases including neurodegeneration and cancer, in particular metastasis. Matrix-assisted laser desorption/ionization (MALDI) mass spectrometry imaging (MSI) has recently been established as a key technology for bioanalysis of multiple biomolecular classes such as proteins, lipids, and glycans. However, it has not yet been systematically explored for investigation of a tissue’s endogenous protease activity. In this study, we demonstrate that different tissues, spray-coated with substance P as a tracer, digest this peptide with different time-course profiles. Furthermore, we reveal that distinct cleavage products originating from substance P are generated transiently and that proteolysis can be attenuated by protease inhibitors in a concentration-dependent manner. To show the translational potential of the method, we analyzed protease activity of gastric carcinoma in mice. Our MSI and quantitative proteomics results reveal differential distribution of protease activity – with strongest activity being observed in mouse tumor tissue, suggesting the general applicability of the workflow in animal pharmacology and clinical studies.Aberrant protease activity has been implicated in the etiology of various prevalent diseases including neurodegeneration and cancer, in particular metastasis. Matrix-assisted laser desorption/ionization (MALDI) mass spectrometry imaging (MSI) has recently been established as a key technology for bioanalysis of multiple biomolecular classes such as proteins, lipids, and glycans. However, it has not yet been systematically explored for investigation of a tissue’s endogenous protease activity. In this study, we demonstrate that different tissues, spray-coated with substance P as a tracer, digest this peptide with different time-course profiles. Furthermore, we reveal that distinct cleavage products originating from substance P are generated transiently and that proteolysis can be attenuated by protease inhibitors in a concentration-dependent manner. To show the translational potential of the method, we analyzed protease activity of gastric carcinoma in mice. Our MSI and quantitative proteomics results reveal differential distribution of protease activity – with strongest activity being observed in mouse tumor tissue, suggesting the general applicability of the workflow in animal pharmacology and clinical studies.
#>     Organisms:  Bos taurus (bovine) 
#>     Organism Parts:  Spleen 
#>     PTMs:  Not Available 
#>     Instruments:  Bruker daltonics solarix series 
#>     Tags:  Technical 
#>     Submission type: Not Available
#>     Lab PIs: Prof. Dr. Carsten Hopf
#>     Submitters: Katrin Erich
#>     Affiliations: CeMOS HS Mannheim
#> 
#> 
#> [[5]]
#> An object of class FileDetailList Containing 15 files
#>    Associated Project: 
#> An object of class compactProjectSummary made public in 2020-09-07
#>     Accession: PXD019255
#>     Title: Uncovering tumor-stroma inter-relationships using quantitative MALDI-Mass spectrometry imaging
#>     Description: To facilitate analysis of protein expression changes in in situ tumors and stroma, we took advantage of a mouse model that permits conditional activation of the Ser-Thr kinase ROCK within mammary tumor cells. In this study, we undertook MALDI-MSI analysis of tissue samples derived from our conditional ROCK mammary tumor model, to quantify in an unbiased manner, the proteomic changes occurring during the progression of mammary cancers in their specific spatial contexts.
#>     Organisms:  Mus musculus (mouse) 
#>     Organism Parts:  Stem cell 
#>     PTMs:  Not Available 
#>     Instruments:  Ltq 
#>     Tags:  Not Available 
#>     Submission type: Not Available
#>     Lab PIs: Manuela Klingler-Hoffmann
#>     Submitters: Parul Mittal
#>     Affiliations: Research Fellow   Future Industries Institute | University of South Australia ipc MLK-40 | p GPO Box 2471 Adelaide SA 5001
```

Now we may choose to download these projects.

    download.files.from.project.list(final.list, "/users/UserName/exampleDownload")
