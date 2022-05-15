data analysis
================

------------------------------------------------------------------------

## EXPLORATION

### 1. Study sites

![](2-data_analysis_files/figure-gfm/map_studies-1.png)<!-- -->

### 2. Studies by Climate

#### 2.1. Climate regions

![](2-data_analysis_files/figure-gfm/map_biomes-1.png)<!-- -->

#### 2.2. MAT-MAP spread of data

![](2-data_analysis_files/figure-gfm/studies_biomes-1.png)<!-- -->

### 3. How many datapoints?

Datapoints:

| Species | ClimateTypes | field | lab |
|:--------|:-------------|------:|----:|
| CO2     | equatorial   |    64 |  47 |
| CO2     | arid         |   165 |  40 |
| CO2     | temperate    |  1875 | 312 |
| CO2     | snow         |  2564 | 142 |
| CO2     | polar        |   120 | 111 |
| CO2     | NA           |    67 | 110 |

total datapoints:

| Species | field | lab |
|:--------|------:|----:|
| CO2     |  4855 | 762 |
| CH4     |    61 | 132 |

Studies:

| Species | ClimateTypes | field | lab |
|:--------|:-------------|------:|----:|
| CO2     | equatorial   |    19 |   8 |
| CO2     | arid         |    28 |   7 |
| CO2     | temperate    |   272 |  50 |
| CO2     | snow         |   233 |  26 |
| CO2     | polar        |    29 |  18 |
| CO2     | NA           |    11 |  20 |

total studies:

| Species | field | lab |
|:--------|------:|----:|
| CO2     |   576 | 108 |
| CH4     |    26 |  21 |

### 4. Incubation Temperature Ranges

#### 4.1. summary of incubation temperatures

![](2-data_analysis_files/figure-gfm/temp_ranges_seg-1.png)<!-- -->

#### 4.2. Q10 vs. incubation temperatures

![](2-data_analysis_files/figure-gfm/temp_ranges_q10-1.png)<!-- -->

#### 4.3. incubation temperature ranges

![](2-data_analysis_files/figure-gfm/temp_ranges_box-1.png)<!-- -->

------------------------------------------------------------------------

------------------------------------------------------------------------

## Analysis

### 1. Summary tables

#### 1.1. Overall summary

| Species | name     |  field |    lab |
|:--------|:---------|-------:|-------:|
| CO2     | mean     |   3.05 |   4.74 |
| CO2     | median   |   2.66 |   2.46 |
| CO2     | perc\_75 |   3.40 |   3.07 |
| CO2     | perc\_99 |  10.54 |  71.82 |
| CO2     | min      |   0.56 |   0.50 |
| CO2     | max      | 131.63 | 344.00 |
| CH4     | mean     |   6.14 |   5.51 |
| CH4     | median   |   4.10 |   3.10 |
| CH4     | perc\_75 |   5.31 |   5.75 |
| CH4     | perc\_99 |  56.88 |  34.69 |
| CH4     | min      |   0.80 |  -2.10 |
| CH4     | max      |  71.00 |  83.00 |

#### 1.2. By biome

| Species | ClimateTypes | name   | field |  lab |
|:--------|:-------------|:-------|------:|-----:|
| CO2     | equatorial   | mean   |  2.78 | 2.46 |
| CO2     | arid         | mean   |  1.76 | 2.48 |
| CO2     | temperate    | mean   |  2.82 | 4.01 |
| CO2     | snow         | mean   |  3.30 | 8.49 |
| CO2     | polar        | mean   |  3.37 | 5.76 |
| CO2     | NA           | mean   |  2.50 | 2.71 |
| CO2     | equatorial   | median |  2.01 | 2.20 |
| CO2     | arid         | median |  1.62 | 2.45 |
| CO2     | temperate    | median |  2.56 | 2.59 |
| CO2     | snow         | median |  2.80 | 2.56 |
| CO2     | polar        | median |  2.76 | 2.22 |
| CO2     | NA           | median |  2.41 | 2.28 |

#### 1.3. By incubation temperature

| Species | Temp\_range | name   | field |    lab |
|:--------|:------------|:-------|------:|-------:|
| CO2     | &lt; 0      | mean   | 45.95 | 113.45 |
| CO2     | 0\_5        | mean   | 27.79 |  59.42 |
| CO2     | 5\_15       | mean   |  2.93 |   3.37 |
| CO2     | 15\_25      | mean   |  2.54 |   2.22 |
| CO2     | &gt; 25     | mean   |  3.07 |   2.05 |
| CO2     | NA          | mean   |  2.93 |   2.77 |
| CO2     | &lt; 0      | median | 21.70 |  89.45 |
| CO2     | 0\_5        | median | 27.50 |   8.95 |
| CO2     | 5\_15       | median |  2.72 |   2.70 |
| CO2     | 15\_25      | median |  2.60 |   2.16 |
| CO2     | &gt; 25     | median |  2.00 |   1.92 |
| CO2     | NA          | median |  2.64 |   2.60 |

### 2. CO2: All studies, all temperatures

We included all datapoints collected, irrespective of incubation
temperature range. (0-10 C, 0-15 C, 0-20 C, 10-20 C, 5-15 C, etc.)

**ANOVA**

    #> # A tibble: 1 × 6
    #>   term          df sumsq meansq statistic      p_value
    #>   <chr>      <dbl> <dbl>  <dbl>     <dbl>        <dbl>
    #> 1 Incubation     1 1875.  1875.      31.6 0.0000000195

![](2-data_analysis_files/figure-gfm/co2_all_data-1.png)<!-- -->

### 3. CO2: Temp ranges of 10 C only

We then subset the data to include only incubations with temperature
ranges of 10 C.  
(0-10 C, 10-20 C, 5-15 C, 15-25 C, etc.)

![](2-data_analysis_files/figure-gfm/co2_10C-1.png)<!-- -->

    #> # A tibble: 1 × 6
    #>   term          df sumsq meansq statistic  p_value
    #>   <chr>      <dbl> <dbl>  <dbl>     <dbl>    <dbl>
    #> 1 Incubation     1 3230.  3230.      41.6 1.23e-10

### 4. Grouping by biome

![](2-data_analysis_files/figure-gfm/co2_biome-1.png)<!-- -->

### 5. Subsetting by temperature range

We then subset the data to include only incubations with specific
temperature ranges: &lt; 5 C, 5-15 C, 15-25 C, &gt; 25C. These ranges
were chosen because these ranges had the highest frequency of data, and
this would also avoid issues pertaining to frozen soils (if we were tp
choose 0-10 C).

![](2-data_analysis_files/figure-gfm/co2_temp_range-1.png)<!-- -->

**ANOVA**

    #> # A tibble: 3 × 7
    #> # Groups:   Temp_range [3]
    #>   Temp_range term          df sumsq meansq statistic  p_value
    #>   <fct>      <chr>      <dbl> <dbl>  <dbl>     <dbl>    <dbl>
    #> 1 5_15       Incubation     1 26.4   26.4      13.1  0.000313
    #> 2 15_25      Incubation     1  4.73   4.73      3.91 0.0494  
    #> 3 > 25       Incubation     1 13.6   13.6      12.5  0.000562

#### 5.1. Bootstrapping the data

The data were highly skewed toward the field measurements, because most
came from the highly curated SRDB. To do a more balanced analysis, we
did bootstrapping by randomly sampling 1000 times, 10 samples each.

![](2-data_analysis_files/figure-gfm/co2_bootstrapping-1.png)<!-- -->![](2-data_analysis_files/figure-gfm/co2_bootstrapping-2.png)<!-- -->

**ANOVA on bootstrapped data**

    #> # A tibble: 3 × 7
    #> # Groups:   Temp_range [3]
    #>   Temp_range term          df sumsq meansq statistic p_value
    #>   <fct>      <chr>      <dbl> <dbl>  <dbl>     <dbl>   <dbl>
    #> 1 5_15       Incubation     1  917.   917.     2659.       0
    #> 2 15_25      Incubation     1  512.   512.     3384.       0
    #> 3 > 25       Incubation     1 5107.  5107.    19059.       0

#### 5.2 Combining non-bootstrap and bootstrap plots

![](2-data_analysis_files/figure-gfm/co2_temp_bootst-1.png)<!-- -->

### 6. Heterotrophic respiration

The field measurements reported above included both autotrophic and
heterotrophic respiration, whereas the lab measurements refer only to
heterotrophic. We subset field measurements that were heterotrophic
respiration only, and compared that with lab.

The heterotrophic field measurements were available for temperate and
snow biomes only.

    #> # A tibble: 3 × 2
    #>   Rh_group             mean
    #>   <chr>               <dbl>
    #> 1 Field: Ra-dominated  3.13
    #> 2 Field: Rh-dominated  2.70
    #> 3 Lab: Rh only         2.77

![](2-data_analysis_files/figure-gfm/co2_rh-1.png)<!-- -->

-   How did field Rh vs. lab Rh compare?

<!-- -->

    #>               Df Sum Sq Mean Sq F value Pr(>F)
    #> Rh_group       1    1.8   1.837   1.116  0.291
    #> Residuals   1445 2378.7   1.646

-   How did field Rh vs. field Ra compare?

<!-- -->

    #>              Df Sum Sq Mean Sq F value   Pr(>F)    
    #> Rh_group      1   23.6  23.560   24.67 8.42e-07 ***
    #> Residuals   746  712.3   0.955                     
    #> ---
    #> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

------------------------------------------------------------------------

### 7. Analysis - CH4

![](2-data_analysis_files/figure-gfm/ch4_all-1.png)<!-- -->

**ANOVA**

    #> # A tibble: 1 × 6
    #>   term          df sumsq meansq statistic p_value
    #>   <chr>      <dbl> <dbl>  <dbl>     <dbl>   <dbl>
    #> 1 Incubation     1  16.3   16.3     0.183   0.670

------------------------------------------------------------------------

<details>
<summary>
Session Info
</summary>

Date run: 2022-05-14

    #> R version 4.1.1 (2021-08-10)
    #> Platform: x86_64-apple-darwin17.0 (64-bit)
    #> Running under: macOS Catalina 10.15.7
    #> 
    #> Matrix products: default
    #> BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
    #> LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib
    #> 
    #> locale:
    #> [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #>  [1] patchwork_1.1.1         googlesheets4_1.0.0     data.table_1.14.2       sidb_1.0.0             
    #>  [5] sf_1.0-5                rnaturalearthdata_0.1.0 rnaturalearth_0.1.0     nlme_3.1-153           
    #>  [9] drake_7.13.2            forcats_0.5.1           stringr_1.4.0           dplyr_1.0.7            
    #> [13] purrr_0.3.4             readr_2.0.2             tidyr_1.1.4             tibble_3.1.5           
    #> [17] ggplot2_3.3.5           tidyverse_1.3.1        
    #> 
    #> loaded via a namespace (and not attached):
    #>   [1] googledrive_2.0.0    minqa_1.2.4          colorspace_2.0-2     ellipsis_0.3.2      
    #>   [5] class_7.3-19         rgdal_1.5-29         fs_1.5.0             rstudioapi_0.13     
    #>   [9] proxy_0.4-26         farver_2.1.0         soilpalettes_0.1.0   bit64_4.0.5         
    #>  [13] fansi_0.5.0          lubridate_1.8.0      xml2_1.3.2           splines_4.1.1       
    #>  [17] rootSolve_1.8.2.2    knitr_1.36           jsonlite_1.7.2       broom_0.7.10        
    #>  [21] dbplyr_2.1.1         rgeos_0.5-9          ggdist_3.1.1         compiler_4.1.1      
    #>  [25] httr_1.4.2           backports_1.2.1      Matrix_1.3-4         assertthat_0.2.1    
    #>  [29] fastmap_1.1.0        gargle_1.2.0         cli_3.0.1            s2_1.0.7            
    #>  [33] htmltools_0.5.2      prettyunits_1.1.1    tools_4.1.1          igraph_1.2.6        
    #>  [37] coda_0.19-4          gtable_0.3.0         glue_1.4.2           wk_0.6.0            
    #>  [41] rappdirs_0.3.3       Rcpp_1.0.8           cellranger_1.1.0     vctrs_0.3.8         
    #>  [45] xfun_0.25            rvest_1.0.1          lifecycle_1.0.0      MASS_7.3-54         
    #>  [49] scales_1.1.1         vroom_1.5.4          hms_1.1.0            parallel_4.1.1      
    #>  [53] yaml_2.2.1           curl_4.3.2           stringi_1.7.5        highr_0.9           
    #>  [57] e1071_1.7-8          PNWColors_0.1.0      measurements_1.4.0   filelock_1.0.2      
    #>  [61] FME_1.3.6.1          storr_1.2.5          rlang_0.4.11         pkgconfig_2.0.3     
    #>  [65] distributional_0.3.0 evaluate_0.14        lattice_0.20-44      labeling_0.4.2      
    #>  [69] bit_4.0.4            tidyselect_1.1.1     deSolve_1.28         magrittr_2.0.1      
    #>  [73] R6_2.5.1             generics_0.1.0       base64url_1.4        txtq_0.2.4          
    #>  [77] DBI_1.1.1            mgcv_1.8-36          pillar_1.6.2         haven_2.4.3         
    #>  [81] withr_2.4.2          units_0.7-2          sp_1.4-5             modelr_0.1.8        
    #>  [85] crayon_1.4.1         KernSmooth_2.23-20   utf8_1.2.2           tzdb_0.1.2          
    #>  [89] rmarkdown_2.11       progress_1.2.2       grid_4.1.1           readxl_1.3.1        
    #>  [93] minpack.lm_1.2-1     reprex_2.0.1         digest_0.6.27        classInt_0.4-3      
    #>  [97] openssl_1.4.4        munsell_0.5.0        viridisLite_0.4.0    askpass_1.1

</details>
