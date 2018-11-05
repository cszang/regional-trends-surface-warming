Code for

Zang CS, Jochner-Oette S, Cortés J, Rammig A, Menzel A: Regional trend
changes in recent surface warming. *Climate Dynamics*.

Everything is documented in comments in `_master_.R`.

# Required packages

## From CRAN

- `ncdf4`
- `reshape2`
- `lubridate`
- `purrr`
- `tidyverse`
- `raster`
- `tmap`
- `strucchange`
- `gstat`
- `sp`

```r
pkgs <- c("ncdf4", "reshape2", "lubridate", "purrr", "tidyverse",
          "raster", "tmap", "strucchange", "gstat", "sp")
lapply(pkgs, install.package)
```

## From GitHub

- `cszang/fridge`

```r
devtools::install_github("cszang/fridge")
```
