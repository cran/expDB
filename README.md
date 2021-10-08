# expDB

![R-CMD-check](https://github.com/byzheng/expDB/workflows/R-CMD-check/badge.svg)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/expDB)](https://cran.r-project.org/package=expDB)



R interface for expDB which is used to store experiment-based data.


# Introduction



## Database design

A [SQLite](https://www.sqlite.org/index.html) database is used to store all information of experiment-based data. 

* Researchers collect the basic information about researchers, e.g. name, email, etc.
* ApSoils specify the soil name in the ApSoils which distribute with [APSIM](https://www.apsim.info/).

* Sites collect the basic information about experiment fields, e.g. name, latitude, longitude, soil, etc. The soil must be the same in the table ApSoils.
* Met specify the information about weather records, e.g. type and filename. The valid met types include “daily” and “hourly”.
* Genotypes collect genotype name and other information.
* Traits collect all information about traits, e.g. name, type, label, unit and description. There aren't limitation of trait names and types. But we normally use crop and soil as trait types.
* Trials collect basic information about trials, e.g. year, site, trial code (trialcode), sowing, density, depth, row spacing (rowspacing) and researcher and met. Trial code must be unique for expDB. Site should come from table Sites.
* Design specify the design of each plot, e.g. year, trialcode, site, row (range), column (plot), replicate, block, treatment, genotype, etc. Each plot specifies by year, site, row, column which must be unique. The trialcode is from table Trials, site from table Sites, and genotype from table Genotypes.
* TrialSoils collect the soil information to some trials, e.g. trial code, bulk density, no3-n, nh4-n, etc.
* Irrigation collect the irrigation information to some trials, e.g. date, amount, etc.
* Fertilization collect the fertilization information to some trials, e.g. date, fertilizer, amount, etc.

## Import data

The experiment-based data can be imported from a excel file which supports by [readxl](https://CRAN.R-project.org/package=readxl) package. expDB is case-insensitive. The Australia style should be used for all date related fields (dd/mm/yyyy).

## Traits
The processes of plant growth and development can be classified into several hierarchical levels, e.g. biosphere, ecosystem or cropping system, field or plot, whole plant, plant organ, cell, molecule, atom, and subatomic particle (Hodges 1991). We could collect data from field, plant, organ in an agricultural experiment. The values in the higher levels can be derived from lower levels. In most cases, we don't directly record data in levels above, but through specific protocol. These observations are used to derive values in other levels. For example, population is converted from the establishment counts; dry weights of leaves, stems, and heads are converted from quadrat harvest. An extra level, measurement is added into the convention.

|Levels      | Prefix | Example              |
|------------|--------|----------------------|
|Field       | F_     | F_StemNumber         |
|Plant       | P_     | P_StemNumber         |
|Organ       | O_     | O_LeafArea           |
|Measurement | M_     | M_DryWeightGreenLeaf |

A list of traits are provided in the released version of expDB.

## Extraction of observations from datasets
The low level function `dbGetPhenotype` is provided to extract the raw observations from database. Some traits can be observed or calculated from other traits. 

### Population
The plant population (`F_Population`) is defined as the number of plants in unit area. `F_Population` is retrived from database, then calculated from measurement establishment count (`M_EstablishmentCount`) and row spacing (`RowSpacing`). The density is assumed to equal to population if none of above traits are specified in the database.

### Final leaf number
Final leaf number (`O_FinalLeafNumber`) is defined as the total completed leaf number of a fertile stem after head emergence (main stem and tillers). `O_FinalLeafNumber` is retrived from database, then calculated from Haun Index. Huan Index is assumed the full expanded flag leaf is recorded. Consequently the maximum integer value in the observations is treated as the `O_FinalLeafNumber`.

### Stem/tiller numbers for field and plant
The stem/tiller numbers are defined as the number stems/tillers per unit area and per plant. The relationships among 4 traits are as follows.

$$P\_StemNumber = P\_TillerNumber + 1$$
$$F\_StemNumber = P\_StemNumber \times F\_Population$$
$$F\_TillerNumber = P\_TillerNumber \times F\_Population$$
$$F\_StemNumber = (P\_TillerNumber + 1) \times F\_Population$$

The stem/tiller numbers are retrived from observations, then calculated by other traits. 

