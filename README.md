# daily_heat
Daily heat illness DLNM, case-crossover from NLDAS 2-derived indices; re-analysis of `daily_hsi` with corrected site temperatures

Note: "rod" indicated data rod extraction from NLDAS 2

## 1. `daily_indices`
Output: "data/daily_indices_rod"

## 2. `case_control_exposures`
Output: "data/cc_exposure_df_rod.rds"

## 3. `cc_dlnm`
Run single index case-crossover/DLNM model.
  - Create lag matrix for selected index; join to case-crossover dataframe
  - Define dlnm cross-basis (natural cubic spline dfs for exposure and lag  dimensions)
  - Run model and get prediction for selected_index, cumulative over all lag days

Output: OR table, plot (.rds), plot (.tiff)

## 4. `cc_dlnm_map`
Map case cross-over function over list of variables/indices.
Output: OR table, plot (.rds), plot (.tiff)

## 5. `cc_dlnm_map_lag`
Map case cross-over function over list of variables/indices. Plot lag slice ORs (by lag slice and add-on cumulative effect) 
Output: plot (.rds), plot (.tiff)

## 6. `cc_dlnm_map_stratify`
Map case cross-over function over list of variables/indices; stratified by: region, heat illness type, branch of service 
Output:  plot (.tiff)
Note: .rds not retained in folder due to file storage size

## 7. 'figures`
Temperature index figures


