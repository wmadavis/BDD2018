## Large potential reduction in economic damages under UN mitigation targets

The data and code in this repository allow users to reproduce all figures, tables, and estimates appearing in the main text and extended data of *Burke, Davis, Diffenbaugh (2018)*, which users of this repository are encouraged to cite.

If you find a consequential coding error, need clarification, or have suggestions for improving the efficiency or readability of the code, please consider submitting an issue to this repository or contacting Matthew Davis at wm.alampaydavis@gmail.com. Inquiries and comments on the methodologies employed and the paper itself should be directed to the corresponding author Marshall Burke at mburke@stanford.edu.

## Organization of repository

- **scripts**: contains scripts needed to replicate estimates and figures that appear in the paper
- **data/input**: contains input data
- **data/output**: after running scripts 01-05, will contain downloaded and processed data needed to run scripts 06-16
- **figures**: after running scripts 01-15, will contain pre-processed versions of the main figures (Figs. 1-4) and extended data figures (Figs. ED1-6)
- **tables**: after running scripts 01-05 and 16, will contain .tex files used to recreate Tables ED1-3
- **paperfigures**: contains vectorized figures close to as they appear in the paper

## Instructions for replication

The repository is 160.8 MB unzipped. Full replication will occupy 5.2 GB. Script 02 requires an internet connection. The user should set their working directory in R to the folder containing the unzipped repository.

After Scripts 01-05 are run, Scripts 06-16 reproduce the figures and tables in the order they appear in the paper but can be run in any order and independently. Further, the user may consider partial replication in the interest of saving time (see: **Processing times** below). The table below maps the scripts strictly required to reproduce each object. Numbers in the rightmost column index the scripts required with superscripts indexing subsections within those scripts.

| Object | Panels | Scripts required |
| :---------------| :---------| :---------------------|
| Figure 1      |             | 01, 02, 03<sup>1</sup>, 06 |
| Figure 2      | a<br>b | 01, 02, 03<sup>1</sup>, 04<sup>1</sup>, 07 <br> 01, 02, 03<sup>1</sup>, 04<sup>1</sup>, 05, 07 |
| Figure 3      |             | 01, 02, 03<sup>3</sup>, 04<sup>3</sup>, 08 |
| Figure 4      | a<br>b-c | 01, 02, 03<sup>2</sup>, 09<br>01, 02, 03<sup>1</sup>, 04<sup>1</sup>, 05, 09 |
| Figure ED1 |             | 01, 02, 03<sup>1</sup>, 05<sup>1</sup>, 10 |
| Figure ED2 |             | 01, 02, 03<sup>4</sup>, 04<sup>3</sup>, 11 |
| Figure ED3 |             | 01, 02, 03<sup>1</sup>, 04<sup>1</sup>, 05, 12 |
| Figure ED4 |             | 01, 02, 03<sup>1</sup>, 04<sup>1</sup> , 13 |
| Figure ED5 |             | 01, 02, 03<sup>1,3</sup>, 04<sup>1,2</sup>, 05, 14 |
| Figure ED6 |             | 01, 02, 03<sup>2</sup>, 15 |
| Table ED1   |             |  01, 02, 03<sup>1</sup>, 04<sup>1</sup>, 16<sup>1</sup> |
| Table ED2   | left<br>right | 01, 02, 03<sup>1</sup>, 04<sup>1</sup>, 16<sup>2</sup> <br>01, 02, 03<sup>1</sup>, 04<sup>1</sup>, 05, 16<sup>2</sup> |
| Table ED3   |             |  01, 02, 03<sup>1</sup>, 04<sup>1</sup>, 16<sup>3</sup> |

Resultant figures and tables will appear in the **figures** and **tables** folders respectively. Some figures and Table ED2 will produce multiple outputs representing component panels and elements to be combined in post-processing and will be titled accordingly. Resultant figures and tables should differ only aesthetically from those appearing in the paper due to post-processing in Adobe Illustrator and an internal power struggle over whether to use base R or the far superior ggplot2.

## R packages required

- lfe
- dplyr
- readstata13
- data.table
- raster
- rgdal
- ncdf4
- magrittr
- reshape2
- zoo
- tibble
- ggplot2
- grid
- gridExtra
- classInt
- fields
- plotrix
- xtable

Scripts were written in R 3.4.3.

Users can run the following one-off command to install the most recent versions of these packages:

```
install.packages(c('lfe', 'dplyr', 'readstata13', 'data.table', 'raster', 'rgdal', 'ncdf4', 'magrittr', 'reshape2', 'zoo', 'tibble', 'ggplot2', 'grid', 'gridExtra', 'classInt', 'fields', 'plotrix', 'xtable'), dependencies = T)
```

## Processing times

For reference, the table below describes how long the most time-consuming scripts took to run on a standard 2.2 GHz personal laptop. These likely represent an upper bound as some processing power was unavoidably channeled towards streaming NBA playoff games while the programs were being timed.

| Script name | Parts (if applicable) | Processing time <br> (hh:mm:ss) |
| :----------------| :--------------------------|:--------------------------------------------|
| 01Bootstraps.R | | 01:51:00 |
| 02DataProcessing.R | Part 1: Global climate projections <br> Part 2: Country-level climate projections <br> Part 3: Growth and population projections | 00:22:21 <br> 00:01:30 <br> 00:00:06 |
| 03ImpactProjections.R | Part 1: Simulate global pathways <br> Part 2: Simulate single-bootstrap pathways <br> Part 3: Simulate non-linear warming pathways <br> Part 4: Simulate country-level pathways | 09:39:40 <br> 00:01:00 <br> 00:31:06 <br> 01:37:16 |
| 04PerCapitaDamages.R | Part 1: Global damages <br> Part 2: Global damages with non-linear warming <br> Part 3: Country-level damages | 03:39:13 <br> 00:01:58 <br> 00:35:52 |
| 05CumulativeDamages.R | Part 1: Preparing discount schemes <br> Part 2: Applying discount schemes <br> Part 3: Regressions to compute cumulative impacts | 00:00:00 <br> 25:29:21 <br> 00:05:22 |
| 06Figure1.R | | 00:18:19 |
| **Total** | | 44:08:42 |

The remaining scripts produce the figures and tables they are named for and should take less than two minutes each to run.