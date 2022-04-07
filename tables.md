---
title: "tables"
output: 
  html_document:
   keep_md: true
editor_options: 
  chunk_output_type: console
---





```r
daily_indices <- read_rds(file = "data/daily_indices_rod")

cc_exposure_df <-
  read_rds(file = "data/cc_exposure_df_rod.rds")

base_service_df <-
  read_rds(file = "data/base_service_df.rds")
```


## Outcome table

```r
to_table1 <-
  cc_exposure_df %>% 
    filter(case == 1) %>% 
    dplyr::select(site_name, source, age, sex, race_ethnic, service, grade, hsi) %>% 
       mutate(service = recode(service,
            "A" = "Army",
            "C" = "Coast Guard",
            "F" = "Air Force",
            "M" = "Marine Corps",
            "N" = "Navy"),
            source = recode(source,
            "INPATIENT" = "In-Patient",
            "OUTPATIENT" = "Out-Patient",
            "RME" = "Reportable Event"),
            hsi = recode(hsi,
            "heat_exhaustion" = "Heat Exhaustion",
            "heat_stroke" = "Heat Stroke"),
            sex = recode(sex,
            "F" = "Female",
            "M" = "Male",
            "Z" = "Unknown")
            ) %>% 
  left_join(climate_regions, by = "site_name") %>% 
  mutate_if(is.character, as.factor) %>% 
            mutate(age = fct_relevel(age, ">=40", after = Inf))


label(to_table1$hsi) <- "HSI Diagnosis"
label(to_table1$age) <- "Age Group"
label(to_table1$sex) <- "Sex"
label(to_table1$race_ethnic) <- "Race/Ethnicity"
label(to_table1$grade) <- "Grade Group"
label(to_table1$service) <- "Service Branch"
label(to_table1$region) <- "NOAA NCEI Climate Region"


table1::table1(~ hsi + sex + age + sex + grade + race_ethnic  | source, data = to_table1)
table1::table1(~ hsi + sex + age + sex + grade   | source, data = to_table1)  
table1::table1(~ service + region + race_ethnic | source, data = to_table1)

table1::table1(~ factor(hsi) + factor(age) + factor(sex) + 
               factor(race_ethnic) + factor(grade) | factor(source), data = to_table1)




# Alternate Table 1 (Cases and Controls)


to_table1 <-
  cc_exposure_df %>% 
    dplyr::select(site_name, case, source, age, sex, race_ethnic, service, grade, hsi) %>% 
       mutate(service = recode(service,
            "A" = "Army",
            "C" = "Coast Guard",
            "F" = "Air Force",
            "M" = "Marine Corps",
            "N" = "Navy"),
            source = recode(source,
            "INPATIENT" = "In-Patient",
            "OUTPATIENT" = "Out-Patient",
            "RME" = "Reportable Event"),
            hsi = recode(hsi,
            "heat_exhaustion" = "Heat Exhaustion",
            "heat_stroke" = "Heat Stroke"),
            sex = recode(sex,
            "F" = "Female",
            "M" = "Male",
            "Z" = "Unknown"),
            case = as_factor(case),
            case = recode(case,
                          "0" = "Control",
                          "1" = "Case")
                      ) %>% 
  left_join(climate_regions %>% 
              mutate(site_name = janitor::make_clean_names(site_name)),
            by = "site_name") %>% 
  mutate_if(is.character, as.factor) %>% 
            mutate(age = fct_relevel(age, ">=40", after = Inf))


label(to_table1$hsi) <- "HSI Diagnosis"
label(to_table1$service) <- "Service Branch"
label(to_table1$region) <- "NOAA NCEI Climate Region"
label(to_table1$case) <- "Case Status"
label(to_table1$source) <- "Encounter Type"

table1::table1(~ hsi + source + service + region | case, data = to_table1)


table1::table1(~ hsi + source + region | case, data = to_table1)



# Cases over time

cc_exposure_df %>% 
  filter(case == 1) %>% 
  group_by(lubridate::year(date)) %>% 
  count() %>% 
  rename(year = `lubridate::year(date)`) %>% 
  ggplot(aes(x = year, y= n)) +
      geom_point() +
      geom_line(size = 1.5) +
      theme_bw() +
  labs(title = "HSI cases by year",
       x = "year")


cc_exposure_df %>% 
  filter(case == 1) %>% 
  group_by(lubridate::year(date), site_name) %>% 
  count() %>% 
  rename(year = `lubridate::year(date)`) %>% 
  ggplot(aes(x = year, y = n, colour = site_name)) +
      geom_point() +
      geom_line(size = 0.8) +
      theme_bw() +
  labs(title = "HSI cases by year",
       x = "year")
```

## OR Table


```r
# Main
selected_index <- c("tmp_f_mean", "tmp_f_max", "hi_f_mean", "hi_f_max", "wbgt_f_mean", "wbgt_f_max")

# All
selected_index <- c("tmp_f_mean", "tmp_f_max", "tmp_f_min", "tmp_f_0600", "hi_f_mean", "hi_f_max", "hi_f_min", "hi_f_0600",
                    "wbgt_f_mean", "wbgt_f_max", "wbgt_f_min", "wbgt_f_0600")

# read_rds("output/or_table_wbgt_max.rds")


or_table_df_rod <-
  purrr::map_df(.x = selected_index,
             ~bind_rows(read_rds(paste0("output/or_tables/or_table_", .x, ".rds")))
  ) %>%
  mutate(index_key = index,
         index = 
           case_when(
              index %in% "wbgt_f_mean" ~ "Mean WBGT (°F)",
              index %in% "wbgt_f_max" ~ "Max WBGT (°F)",
              index %in% "hi_f_mean" ~ "Mean Heat Index (°F)",
              index %in% "hi_f_max" ~ "Max Heat Index (°F)",
              index %in% "tmp_f_mean" ~ "Mean Temperature (°F)",
              index %in% "tmp_f_max" ~ "Max Temperature (°F)",
              index %in% "wbgt_f_0600" ~ "0600 hours WBGT (°F)",
              index %in% "wbgt_f_min" ~ "Minimum WBGT (°F)",
              index %in% "hi_f_0600" ~ "0600 hours Heat Index (°F)",
              index %in% "hi_f_min" ~ "Minimum Heat Index (°F)",
              index %in% "tmp_f_0600" ~ "0600 hours Temperature (°F)",
              index %in% "tmp_f_min" ~ "Minimum Temperature (°F)"),
          or_ci = paste0(format(round(rr, digits = 2), nsmall = 2, trim = "TRUE"), 
                         " (", format(round(ci_low, digits = 2), nsmall = 2, trim = "TRUE"), ", ",
                         format(round(ci_high, digits = 2), nsmall = 2, trim = "TRUE"), ")")) %>% 
    rename(or = rr,
           value = var) 
  
or_table_df_rod
```

```
## # A tibble: 835 x 7
##    value     or ci_low ci_high index                 index_key  or_ci           
##    <chr>  <dbl>  <dbl>   <dbl> <chr>                 <chr>      <chr>           
##  1 32    0.0498 0.0389  0.0637 Mean Temperature (°F) tmp_f_mean 0.05 (0.04, 0.0~
##  2 33    0.0495 0.0391  0.0627 Mean Temperature (°F) tmp_f_mean 0.05 (0.04, 0.0~
##  3 34    0.0493 0.0393  0.0619 Mean Temperature (°F) tmp_f_mean 0.05 (0.04, 0.0~
##  4 35    0.0492 0.0395  0.0613 Mean Temperature (°F) tmp_f_mean 0.05 (0.04, 0.0~
##  5 36    0.0492 0.0398  0.0608 Mean Temperature (°F) tmp_f_mean 0.05 (0.04, 0.0~
##  6 37    0.0493 0.0402  0.0604 Mean Temperature (°F) tmp_f_mean 0.05 (0.04, 0.0~
##  7 38    0.0495 0.0406  0.0602 Mean Temperature (°F) tmp_f_mean 0.05 (0.04, 0.0~
##  8 39    0.0498 0.0411  0.0602 Mean Temperature (°F) tmp_f_mean 0.05 (0.04, 0.0~
##  9 40    0.0502 0.0417  0.0603 Mean Temperature (°F) tmp_f_mean 0.05 (0.04, 0.0~
## 10 41    0.0507 0.0424  0.0606 Mean Temperature (°F) tmp_f_mean 0.05 (0.04, 0.0~
## # ... with 825 more rows
```

```r
or_table_rod_wide <-
  or_table_df_rod %>% 
  dplyr::select(-c(index_key, or, ci_low, ci_high)) %>% 
  pivot_wider(names_from = value, values_from = or_ci) 

or_table_rod_wide
```

```
## # A tibble: 12 x 90
##    index `32`  `33`  `34`  `35`  `36`  `37`  `38`  `39`  `40`  `41`  `42`  `43` 
##    <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
##  1 Mean~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~
##  2 Max ~ 0.06~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~
##  3 Mini~ 0.08~ 0.08~ 0.08~ 0.08~ 0.08~ 0.09~ 0.09~ 0.09~ 0.09~ 0.10~ 0.10~ 0.10~
##  4 0600~ 0.09~ 0.09~ 0.09~ 0.09~ 0.09~ 0.10~ 0.10~ 0.10~ 0.10~ 0.10~ 0.11~ 0.11~
##  5 Mean~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.06~ 0.06~ 0.06~
##  6 Max ~ 0.06~ 0.06~ 0.06~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~
##  7 Mini~ 0.08~ 0.08~ 0.08~ 0.09~ 0.09~ 0.09~ 0.09~ 0.10~ 0.10~ 0.10~ 0.11~ 0.11~
##  8 0600~ 0.09~ 0.09~ 0.10~ 0.10~ 0.10~ 0.10~ 0.10~ 0.11~ 0.11~ 0.11~ 0.12~ 0.12~
##  9 Mean~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.06~ 0.06~ 0.06~ 0.06~
## 10 Max ~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~ 0.05~
## 11 Mini~ 0.09~ 0.10~ 0.10~ 0.10~ 0.10~ 0.10~ 0.11~ 0.11~ 0.11~ 0.12~ 0.12~ 0.13~
## 12 0600~ 0.11~ 0.11~ 0.11~ 0.11~ 0.12~ 0.12~ 0.12~ 0.12~ 0.13~ 0.13~ 0.13~ 0.14~
## # ... with 77 more variables: `44` <chr>, `45` <chr>, `46` <chr>, `47` <chr>,
## #   `48` <chr>, `49` <chr>, `50` <chr>, `51` <chr>, `52` <chr>, `53` <chr>,
## #   `54` <chr>, `55` <chr>, `56` <chr>, `57` <chr>, `58` <chr>, `59` <chr>,
## #   `60` <chr>, `61` <chr>, `62` <chr>, `63` <chr>, `64` <chr>, `65` <chr>,
## #   `66` <chr>, `67` <chr>, `68` <chr>, `69` <chr>, `70` <chr>, `71` <chr>,
## #   `72` <chr>, `73` <chr>, `74` <chr>, `75` <chr>, `76` <chr>, `77` <chr>,
## #   `78` <chr>, `79` <chr>, `80` <chr>, `81` <chr>, `82` <chr>, `83` <chr>, ...
```

```r
or_table_rod <-
  or_table_df_rod  %>%
    dplyr::select(-c(index_key, or, ci_low, ci_high)) %>% 
    pivot_wider(names_from = index, values_from = or_ci)

or_table_rod
```

```
## # A tibble: 89 x 13
##    value `Mean Temperature ~` `Max Temperatu~` `Minimum Tempe~` `0600 hours Te~`
##    <chr> <chr>                <chr>            <chr>            <chr>           
##  1 32    0.05 (0.04, 0.06)    0.06 (0.04, 0.0~ 0.08 (0.07, 0.0~ 0.09 (0.07, 0.1~
##  2 33    0.05 (0.04, 0.06)    0.05 (0.04, 0.0~ 0.08 (0.07, 0.0~ 0.09 (0.08, 0.1~
##  3 34    0.05 (0.04, 0.06)    0.05 (0.04, 0.0~ 0.08 (0.07, 0.0~ 0.09 (0.08, 0.1~
##  4 35    0.05 (0.04, 0.06)    0.05 (0.04, 0.0~ 0.08 (0.07, 0.1~ 0.09 (0.08, 0.1~
##  5 36    0.05 (0.04, 0.06)    0.05 (0.04, 0.0~ 0.08 (0.07, 0.1~ 0.09 (0.08, 0.1~
##  6 37    0.05 (0.04, 0.06)    0.05 (0.04, 0.0~ 0.09 (0.07, 0.1~ 0.10 (0.08, 0.1~
##  7 38    0.05 (0.04, 0.06)    0.05 (0.04, 0.0~ 0.09 (0.08, 0.1~ 0.10 (0.08, 0.1~
##  8 39    0.05 (0.04, 0.06)    0.05 (0.04, 0.0~ 0.09 (0.08, 0.1~ 0.10 (0.09, 0.1~
##  9 40    0.05 (0.04, 0.06)    0.05 (0.04, 0.0~ 0.09 (0.08, 0.1~ 0.10 (0.09, 0.1~
## 10 41    0.05 (0.04, 0.06)    0.05 (0.04, 0.0~ 0.10 (0.08, 0.1~ 0.10 (0.09, 0.1~
## # ... with 79 more rows, and 8 more variables: `Mean Heat Index (°F)` <chr>,
## #   `Max Heat Index (°F)` <chr>, `Minimum Heat Index (°F)` <chr>,
## #   `0600 hours Heat Index (°F)` <chr>, `Mean WBGT (°F)` <chr>,
## #   `Max WBGT (°F)` <chr>, `Minimum WBGT (°F)` <chr>,
## #   `0600 hours WBGT (°F)` <chr>
```

```r
#write.csv(or_table_rod, file = "output/tables/or_table_rod.csv")
```


## Mean Annual Temperatures (1998-2019)


```r
mean_annual_index_1998_2019 <-
  daily_indices %>%
  dplyr::filter(lubridate::year(date) %in% 1998:2019) %>%
    group_by(site_name) %>% 
    summarise_at(c("tmp_f_mean", "hi_f_mean", "wbgt_f_mean", "tmp_f_max", "hi_f_max", "wbgt_f_max"), mean) %>%
    arrange(desc(tmp_f_mean)) %>% 
    as.data.frame()

mean_annual_index_1998_2019
```

```
##                 site_name tmp_f_mean hi_f_mean wbgt_f_mean tmp_f_max hi_f_max
## 1  Joint Base San Antonio   71.67346  72.33247    67.05027  82.87135 83.73748
## 2           NAS Pensacola   71.18698  73.63084    70.02755  73.41149 76.52933
## 3               Fort Hood   69.78602  70.02086    64.73603  80.48936 80.80448
## 4               Eglin AFB   69.34768  70.99221    67.65798  77.14410 79.31401
## 5            Fort Stewart   68.04250  69.29851    66.00124  77.71053 79.50258
## 6      MCRD Parris Island   67.94247  69.34557    66.64770  75.51802 77.49975
## 7        Twentynine Palms   67.49176  64.60379    56.36032  77.65912 74.17128
## 8               Fort Polk   67.47280  68.60491    65.06967  78.14482 80.01489
## 9              Fort Irwin   66.81338  63.84944    55.72723  77.61233 73.87058
## 10           Fort Benning   66.68158  67.43508    63.97499  75.78715 76.99635
## 11             Fort Bliss   65.81070  63.41896    56.22510  75.79273 72.82811
## 12            Fort Gordon   65.38670  65.89276    62.75896  75.13049 75.86006
## 13       MCB Camp Lejeune   64.73916  65.67834    63.43935  72.61089 74.15019
## 14           Fort Jackson   63.99776  64.52398    61.81875  73.88802 74.65225
## 15              Fort Sill   63.77255  63.18259    58.61846  75.17724 74.39931
## 16         MCRD San Diego   63.59804  62.98852    61.73297  74.67326 73.85825
## 17             Fort Bragg   62.93637  63.29805    60.50280  71.97627 72.63759
## 18     MCB Camp Pendleton   60.91952  60.43739    60.74360  63.47783 62.94468
## 19         NMC Portsmouth   60.86402  61.31524    59.64511  69.09356 69.93139
## 20          Fort Campbell   59.43517  59.81478    57.47040  68.59720 69.54919
## 21              Fort Knox   57.09438  57.34637    55.56166  66.02149 66.81056
## 22      Fort Leonard Wood   56.89297  56.99262    54.81957  66.85894 67.35694
## 23           MCB Quantico   56.77491  56.72042    54.94695  66.15212 66.24205
## 24             Fort Riley   56.31679  55.98980    52.94584  67.15851 66.81476
##    wbgt_f_max
## 1    76.49470
## 2    75.49845
## 3    74.12380
## 4    75.99613
## 5    75.14563
## 6    74.81400
## 7    68.10546
## 8    74.98753
## 9    67.19098
## 10   73.35348
## 11   66.93548
## 12   72.43767
## 13   71.82241
## 14   71.55327
## 15   68.88853
## 16   72.97050
## 17   69.77198
## 18   68.39227
## 19   68.33863
## 20   67.04913
## 21   65.19875
## 22   64.98955
## 23   64.50934
## 24   63.26662
```

```r
# write_rds(mean_annual_index_1998_2019, "output/tables/mean_annual_index_1998_2019.rds")


## Mean annual index (cases)

mean_index_cases_1998_2019 <-
  cc_exposure_df %>% 
   filter(case == 1,
          lubridate::year(date) %in% 1998:2019) %>% 
    group_by(site_name) %>% 
    summarise_at(c("tmp_f_mean", "hi_f_mean", "wbgt_f_mean", "tmp_f_max", "hi_f_max", "wbgt_f_max"), mean) %>%
    arrange(desc(tmp_f_mean)) %>% 
    as.data.frame()

mean_index_cases_1998_2019 
```

```
##                 site_name tmp_f_mean hi_f_mean wbgt_f_mean tmp_f_max hi_f_max
## 1              Fort Irwin   86.03668  82.49157    68.87302  98.02677 92.67464
## 2               Fort Sill   85.75371  86.26089    76.18236  97.70054 98.21089
## 3        Twentynine Palms   85.31413  82.21582    69.25229  95.93049 91.61420
## 4               Fort Hood   84.99566  87.11887    77.43977  95.82973 98.13387
## 5  Joint Base San Antonio   83.99645  86.45095    77.52313  96.00530 98.73921
## 6               Fort Polk   81.95417  86.12085    78.57584  92.46728 98.40303
## 7               Eglin AFB   81.67861  87.46177    79.70803  88.91184 95.72782
## 8            Fort Stewart   81.40579  86.04214    78.45296  90.74554 96.39147
## 9              Fort Bliss   80.73804  78.29406    67.15415  90.97566 87.33925
## 10            Fort Gordon   80.56205  83.42099    76.62539  89.99197 93.50073
## 11             Fort Riley   80.53654  82.00017    73.95128  93.05403 95.25049
## 12           Fort Benning   80.24001  83.89213    76.87199  88.53372 93.40821
## 13           Fort Jackson   80.08506  83.10981    76.32102  90.37090 94.01950
## 14          NAS Pensacola   79.73226  86.06186    79.05940  81.03862 88.49397
## 15     MCRD Parris Island   79.71812  84.65052    78.21265  86.59941 92.82020
## 16      Fort Leonard Wood   77.94291  81.11732    74.73835  87.95248 93.55425
## 17          Fort Campbell   77.55989  80.98832    75.00688  86.41375 92.19311
## 18         NMC Portsmouth   77.50262  80.82405    75.44370  86.67923 91.42896
## 19              Fort Knox   77.43528  80.91301    75.44492  86.06084 92.15529
## 20       MCB Camp Lejeune   77.40582  81.33036    75.88384  84.87321 90.54681
## 21             Fort Bragg   75.26794  77.67574    72.05043  84.13217 87.34484
## 22           MCB Quantico   75.24141  77.44340    72.96033  84.86138 88.49036
## 23         MCRD San Diego   67.77656  67.56898    65.72876  79.70495 79.27410
## 24     MCB Camp Pendleton   64.21778  64.05336    64.20870  67.01583 66.77606
##    wbgt_f_max
## 1    80.64308
## 2    85.76167
## 3    80.78531
## 4    86.15214
## 5    86.40856
## 6    87.71627
## 7    87.27109
## 8    86.76825
## 9    77.81512
## 10   85.44553
## 11   84.21695
## 12   85.41817
## 13   85.25820
## 14   85.18095
## 15   85.55900
## 16   84.54773
## 17   83.95147
## 18   84.22604
## 19   84.55712
## 20   83.31608
## 21   80.55823
## 22   82.05556
## 23   76.96048
## 24   72.01162
```

```r
# write_rds(mean__index_cases_1998_2019, "output/tables/mean_index_cases_1998_2019.rds")
```




## Mean monthly temperatures

```r
# Mean monthly temperatures, deg F
daily_indices %>% 
  dplyr::filter(lubridate::year(date) %in% 1998:2019) %>%
  group_by(site_name, lubridate::month(date)) %>% 
  summarise(mean = mean(tmp_f_mean)) %>% 
  pivot_wider(names_from = site_name, values_from = mean)
```

```
## `summarise()` has grouped output by 'site_name'. You can override using the
## `.groups` argument.
```

```
## # A tibble: 12 x 25
##    `lubridate::month(date)` `Eglin AFB` `Fort Benning` `Fort Bliss` `Fort Bragg`
##                       <dbl>       <dbl>          <dbl>        <dbl>        <dbl>
##  1                        1        53.2           47.4         45.3         42.4
##  2                        2        56.4           51.8         50.4         45.7
##  3                        3        61.2           58.0         58.1         52.4
##  4                        4        67.9           65.9         65.2         61.8
##  5                        5        75.1           74.3         73.6         70.9
##  6                        6        81.6           81.3         83.7         79.0
##  7                        7        83.3           83.5         85.1         82.3
##  8                        8        83.3           83.3         83.5         81.2
##  9                        9        79.7           77.9         77.4         74.9
## 10                       10        71.2           67.9         66.9         63.8
## 11                       11        62.0           57.3         54.6         53.3
## 12                       12        56.4           50.8         45.2         46.5
## # ... with 20 more variables: `Fort Campbell` <dbl>, `Fort Gordon` <dbl>,
## #   `Fort Hood` <dbl>, `Fort Irwin` <dbl>, `Fort Jackson` <dbl>,
## #   `Fort Knox` <dbl>, `Fort Leonard Wood` <dbl>, `Fort Polk` <dbl>,
## #   `Fort Riley` <dbl>, `Fort Sill` <dbl>, `Fort Stewart` <dbl>,
## #   `Joint Base San Antonio` <dbl>, `MCB Camp Lejeune` <dbl>,
## #   `MCB Camp Pendleton` <dbl>, `MCB Quantico` <dbl>,
## #   `MCRD Parris Island` <dbl>, `MCRD San Diego` <dbl>, ...
```

```r
# Mean of max daily temperatures
daily_indices %>% 
  dplyr::filter(lubridate::year(date) %in% 1998:2019) %>%
  group_by(site_name, lubridate::month(date)) %>% 
  summarise(max = mean(tmp_f_max)) %>% 
  pivot_wider(names_from = site_name, values_from = max)
```

```
## `summarise()` has grouped output by 'site_name'. You can override using the
## `.groups` argument.
```

```
## # A tibble: 12 x 25
##    `lubridate::month(date)` `Eglin AFB` `Fort Benning` `Fort Bliss` `Fort Bragg`
##                       <dbl>       <dbl>          <dbl>        <dbl>        <dbl>
##  1                        1        61.2           56.4         54.8         51.2
##  2                        2        64.7           61.3         60.2         55.1
##  3                        3        70.1           67.8         68.7         62.3
##  4                        4        76.9           75.8         76.2         71.7
##  5                        5        83.4           83.3         84.7         80.1
##  6                        6        88.4           89.3         94.3         87.7
##  7                        7        89.5           91.2         93.8         90.5
##  8                        8        89.5           91.3         92.2         89.6
##  9                        9        86.7           87.1         87.2         83.8
## 10                       10        79.7           78.1         77.0         73.4
## 11                       11        70.6           67.3         64.9         62.8
## 12                       12        64.3           59.8         54.8         54.8
## # ... with 20 more variables: `Fort Campbell` <dbl>, `Fort Gordon` <dbl>,
## #   `Fort Hood` <dbl>, `Fort Irwin` <dbl>, `Fort Jackson` <dbl>,
## #   `Fort Knox` <dbl>, `Fort Leonard Wood` <dbl>, `Fort Polk` <dbl>,
## #   `Fort Riley` <dbl>, `Fort Sill` <dbl>, `Fort Stewart` <dbl>,
## #   `Joint Base San Antonio` <dbl>, `MCB Camp Lejeune` <dbl>,
## #   `MCB Camp Pendleton` <dbl>, `MCB Quantico` <dbl>,
## #   `MCRD Parris Island` <dbl>, `MCRD San Diego` <dbl>, ...
```


### Counts by service - installation

```r
site_counts <-
  cc_exposure_df %>%
  filter(case == 1) %>% 
    count(site_name, service) %>% 
    spread(service, n, fill = 0) %>% 
    rowwise() %>% 
    mutate(total = sum(c_across(A:N))) %>% 
  arrange(desc(total)) %>% 
  as.data.frame()

site_counts 
```

```
##                 site_name    A  C   F    M   N total
## 1              Fort Bragg 4746  0  90    6   4  4846
## 2            Fort Benning 4790  0  13   23  13  4839
## 3        MCB Camp Lejeune   12  7   3 2729 323  3074
## 4      MCRD Parris Island    1  0   0 2460  16  2477
## 5           Fort Campbell 2040  1   5    0   2  2048
## 6            Fort Jackson 1700  0  34    3  27  1764
## 7               Fort Polk 1747  0  12    3   1  1763
## 8      MCB Camp Pendleton    8  1   1 1439  98  1547
## 9               Fort Hood 1263  1   6   43  48  1361
## 10         MCRD San Diego    8  4   1  925 239  1177
## 11           MCB Quantico    4  0   0  961   7   972
## 12           Fort Stewart  954  0   6    5   2   967
## 13 Joint Base San Antonio  345  2 572    6  30   955
## 14       Twentynine Palms    2  0   1  631  23   657
## 15              Fort Sill  623  0   8   14   0   645
## 16      Fort Leonard Wood  522  0   2   19   5   548
## 17             Fort Riley  431  0   4    0   5   440
## 18             Fort Irwin  370  0   0    5   0   375
## 19              Fort Knox  335  0   6    2   1   344
## 20          NAS Pensacola   21  3  12   88 219   343
## 21         NMC Portsmouth   23 23   3   53 241   343
## 22             Fort Bliss  324  0   6    2   4   336
## 23            Fort Gordon  292  0   4    4  12   312
## 24              Eglin AFB  100  0 189    2   2   293
```



