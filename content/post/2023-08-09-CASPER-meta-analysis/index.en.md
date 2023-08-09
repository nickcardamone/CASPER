---
title: "CASPER Meta-Analysis Documentation"
author: "Nick Cardamone"
date: '2023-08-09'
categories: R
tags:
- R Markdown
- meta analysis
- implementation science
---




### DistillerSR options:
* Advanced Options: Hide Empty Columns, Merge Sets by Key, Compress checkboxes to one column, "Merge Data and duplicate single forms to repeating forms", 
* Data criteria: user = cardamone, 
* Reference display options: display author and title
* Report settings: limit to label = "RCT" & NOT "throwaway"
* "study char": Study char + determinants + outcomes
* "icc_cov": Source + Covariate R-Sq
* "sample_size": Source + Conditions
* "effect_size": Source + Effect Size



### Only keep entries filled in by Nick

```r
# Remove irrelevant variables: e.g. all notes, comments, info: also collapse the analysis type variable. 
study_characteristics <- study_characteristics %>% filter(str_detect(Characteristics_k, "Nick") == T) %>% select(-ends_with("notes"), -ends_with("comment"), -ends_with("info"), -Level, -User)
sample_size <- sample_size %>% filter(str_detect(Source_k, "Nick") == T) %>% select(-ends_with("notes"), -ends_with("comment"), -ends_with("info"), -Level, -User)
icc_cov <- icc_cov %>% filter(str_detect(Source_k, "Nick") == T) %>% select(-ends_with("notes"), -ends_with("comment"), -ends_with("info"), -Level, -User) %>% tidyr::unite("analysis_type", c(analysis_type, measure_type_ep), sep= "_", remove = T, na.rm = T)
effect_size <- effect_size %>% filter(str_detect(Source_k, "Nick") == T) %>% select(-ends_with("notes"), -ends_with("comment"), -ends_with("info")) %>% tidyr::unite("analysis_type", c(analysis_type, measure_type_ep), sep= "_", remove = T, na.rm = T)
```

```r
# Make the number of time points a numeric variable -- if it's not a longitudinal study just change the number of time points to 1.
effect_size <- effect_size %>% mutate(
         num_timepoints = if_else(!is.na(num_timepoints_standard), as.numeric(num_timepoints_standard), 
                                  if_else(!is.na(num_timepoints_nonstandard), as.numeric(num_timepoints_nonstandard), 
                                          if_else(analysis_type != "long", 1, NA)))) %>% select(-measure_type_long, -num_timepoints_standard, -num_timepoints_nonstandard, -Level, -User, -cov_applicable)


# Remove "NR"
study_characteristics[study_characteristics == "NR"] <- NA
sample_size[sample_size == "NR"] <- NA
icc_cov[icc_cov == "NR"] <- NA
effect_size[effect_size == "NR"] <- NA
```

### Study Characteristics
* Outcomes
* Implementation Strategies
* Study Metadata

```r
outcomes <- study_characteristics %>% select(Refid, Outcomes_k, dv_name, dv_outcome, dv_measure_approach)
# We're only taking variables at the study level for the implementation strategies and metadata datasets so

imp_strategies <- study_characteristics %>% select(Refid, IVs_k, imp1_name:control_target) %>% distinct() 
metadata <- study_characteristics %>% select(Refid:imps_rand_1) %>% distinct() 

#Make dummy variables for randomization level

metadata$rand_L1 <- if_else(str_detect(as.character(metadata$imps_rand_1), "1"), 1, 0)
metadata$rand_L2 <- if_else(str_detect(as.character(metadata$imps_rand_1), "2"), 1, 0)
metadata$rand_L3 <- if_else(str_detect(as.character(metadata$imps_rand_1), "3"), 1, 0)
metadata$rand_L4 <- if_else(str_detect(as.character(metadata$imps_rand_1), "4"), 1, 0)
```

```r
# Pivot longer so that 1 row = 1 condition (nested within studies.)
names <- imp_strategies %>% pivot_longer(cols = c("imp1_name", "imp2_name","imp3_name", "control_name"), names_to = "comparator", values_to = "name", names_pattern = "(.*)_name") %>% select(Refid, IVs_k, comparator, name)
eric <- imp_strategies %>% pivot_longer(cols = ends_with("eric"), names_to = "comparator", values_to = "strategies", names_pattern = "(.*)_eric") %>% select(Refid, comparator, strategies)
target <- imp_strategies %>% pivot_longer(cols = c("imp1_target", "imp2_target","imp3_target", "control_target"), names_to = "comparator", values_to = "target", names_pattern = "(.*)_target") %>% select(Refid, comparator, target)
```

```r
# Mash the columns back together to make a tidied dataset for implementation strategies.

imp_strategies <- data.frame(names, eric = eric$strategies, target = target$target)

imp_strategies <- imp_strategies %>% filter(!is.na(name))

imp_strategies$cond_num <- ave(imp_strategies$comparator, imp_strategies$Refid, FUN = seq_along)
imp_strategies$target <- str_to_lower(imp_strategies$target)

imp_strategies <- imp_strategies %>% separate(target, into = c("target1", "target2"), sep = ",")
## Warning: Expected 2 pieces. Additional pieces discarded in 4 rows [9, 10, 17,
## 18].
## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 18 rows [1, 2, 3, 8, 12,
## 13, 14, 21, 22, 24, 25, 26, 27, 30, 31, 33, 36, 38].
imp_strategies <- imp_strategies %>% separate(target1, into = c("target1", "target1_detail"), sep = " : ")
## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 29 rows [2, 3, 7, 8, 9,
## 10, 11, 12, 15, 17, 18, 19, 20, 21, 22, 25, 26, 27, 28, 29, ...].
imp_strategies <- imp_strategies %>% separate(target2, into = c("target2", "target2_detail"), sep = " : ")
## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 17 rows [5, 6, 7, 9, 10,
## 11, 15, 17, 18, 19, 20, 28, 29, 32, 34, 35, 37].
```
** One row per study ** 

```r
imp_strategies_wide <- imp_strategies %>% select(Refid, IVs_k, name, eric, target1, target2, cond_num) %>% pivot_wider(names_from = "cond_num", values_from = c("name", "eric", "target1", "target2"))
```


### Sample Size

```r
## Load sample size (Multiple sets of values per DV)
sample_size <- sample_size %>% drop_na(data_source)

#sample_size <- sample_size %>% filter(!is.na(cond_num) & !is.na(data_source))
##### Pivot wider to have unique columns for sample size at each level.

sample_size <- sample_size %>% drop_na(cond_num)
sample_size <- sample_size %>% dplyr::select(Source_k, cond_num, L1_condition_N, L2_condition_N, L3_condition_N, L4_condition_N) #%>% 

# Pivot wider 
sample_size <- reshape(
  merge(sample_size,
        expand.grid(
          Source_k = unique(sample_size$Source_k),
          cond_num = 1:3
        ),
        all = TRUE
  ),
  direction = "wide",
  idvar = "Source_k",
  timevar = "cond_num"
)
## Warning in reshapeWide(data, idvar = idvar, timevar = timevar, varying =
## varying, : multiple rows match for cond_num=1: first taken
## Warning in reshapeWide(data, idvar = idvar, timevar = timevar, varying =
## varying, : multiple rows match for cond_num=2: first taken

head(sample_size)
##                                                                                                                                                                       Source_k
## 1                                     Nick-ADHD quality improvement (QI) intervention for community-based pediatric practices|imp_strat_effects|ADHD symptoms (4-levels)|Paper
## 4                                        Nick-ADHD quality improvement (QI) intervention for community-based pediatric practices|imp_strat_effects|ADHD symptoms (4-levels)|PI
## 7                             Nick-ADHD quality improvement (QI) intervention for community-based pediatric practices|imp_strat_effects|Number of contacts in first year|Paper
## 10                               Nick-ADHD quality improvement (QI) intervention for community-based pediatric practices|imp_strat_effects|Number of contacts in first year|PI
## 13  Nick-ADHD quality improvement (QI) intervention for community-based pediatric practices|imp_strat_effects|Number of parent scales to monitor treatment in first year|Paper
## 16 Nick-ADHD quality improvement (QI) intervention for community-based pediatric practices|imp_strat_effects|Number of teacher scales to monitor treatment in first year|Paper
##    L1_condition_N.1 L2_condition_N.1 L3_condition_N.1 L4_condition_N.1
## 1              <NA>              194               63               22
## 4              <NA>              194               63               22
## 7               258               71               22             <NA>
## 10              258               71               22             <NA>
## 13              235               68               22             <NA>
## 16              194               63               22             <NA>
##    L1_condition_N.2 L2_condition_N.2 L3_condition_N.2 L4_condition_N.2
## 1              <NA>              231               76               24
## 4              <NA>              231               76               22
## 7               316               89               23             <NA>
## 10              316               89               23             <NA>
## 13              287               89               24             <NA>
## 16              231               76               24             <NA>
##    L1_condition_N.3 L2_condition_N.3 L3_condition_N.3 L4_condition_N.3
## 1              <NA>             <NA>             <NA>             <NA>
## 4              <NA>             <NA>             <NA>             <NA>
## 7              <NA>             <NA>             <NA>             <NA>
## 10             <NA>             <NA>             <NA>             <NA>
## 13             <NA>             <NA>             <NA>             <NA>
## 16             <NA>             <NA>             <NA>             <NA>
```

### ICC and COVARIATE R-SQ:

```r
# Fix ICC values:
# If L1 variance and L2 variance are in the old L1/L2 variance fields, copy them onto the new fields.
# Drop the + from the "+3" in the num_levels variable so it can be numeric.
# From the PI provided data, just compute the ICC at each level from the VCEs so it's standardized, if it's from the paper leave it alone.
icc_cov <- icc_cov %>% dplyr::mutate(L1_variance_3 = if_else(is.na(L1_variance_3), as.numeric(L1_variance_2), as.numeric(L1_variance_3)),
                                     L2_variance_3 = if_else(is.na(L2_variance_3), as.numeric(L2_variance_2), as.numeric(L2_variance_3)),
                                     num_levels = as.numeric(gsub("\\+", "", `num_levels`)),
                                     ICC_2 = if_else(num_levels == 2 & data_source == "PI", as.numeric(L2_variance_3)/(as.numeric(L1_variance_3)+as.numeric(L2_variance_3)),
                                                     if_else(num_levels == 3 & data_source == "PI", as.numeric(L2_variance_3)/(as.numeric(L1_variance_3)+as.numeric(L2_variance_3)+as.numeric(L3_variance_3)),
                                                             if_else(num_levels == 4 & data_source == "PI", as.numeric(L2_variance_3)/(as.numeric(L1_variance_3)+as.numeric(L2_variance_3)+as.numeric(L3_variance_3)+as.numeric(L4_variance_3)), as.numeric(ICC_2)))),
                                     ICC_3 = if_else(num_levels == 3 & data_source == "PI", as.numeric(L3_variance_3)/(as.numeric(L1_variance_3)+as.numeric(L2_variance_3)+as.numeric(L3_variance_3)),
                                                     if_else(num_levels == 4 & data_source == "PI",
                                                             as.numeric(L3_variance_3)/(as.numeric(L1_variance_3)+as.numeric(L2_variance_3)+as.numeric(L3_variance_3)+as.numeric(L4_variance_3)), as.numeric(ICC_3))),
                                     ICC_4 = if_else(num_levels == 4 & data_source == "PI", as.numeric(L4_variance_3)/(as.numeric(L1_variance_3)+as.numeric(L2_variance_3)+as.numeric(L3_variance_3)+as.numeric(L4_variance_3)),
                                                     as.numeric(ICC_4)))
```

## Covariate R-squared

```r
# Substitute null model VCE at each level if a value isn't already there.
icc_cov <- icc_cov %>% dplyr::mutate(NULL_VCE = if_else(!is.na(NULL_VCE), as.numeric(NULL_VCE), 
                                                        if_else(level_VCE == 1, as.numeric(L1_variance_3), 
                                                                if_else(level_VCE == 2, as.numeric(L2_variance_3),
                                                                        if_else(level_VCE == 3, as.numeric(L3_variance_3),
                                                                                if_else(level_VCE == 4, as.numeric(L4_variance_3), NA))))))

icc_cov <- icc_cov %>% dplyr::mutate(
  ## 1 - (PRETEST VCE/ NULL VCE)
  pretest_covR = 1-as.numeric(pretest_cov_VCE)/as.numeric(NULL_VCE), 
  ## 1 - (DEMO VCE / NULL VCE)
  demo_covR = 1-as.numeric(demo_cov_VCE)/as.numeric(NULL_VCE), 
  ## 1 - (FULL VCE / NULL VCE)
  covR = 1-as.numeric(full_VCE)/as.numeric(NULL_VCE))
## Warning: There was 1 warning in `dplyr::mutate()`.
## ℹ In argument: `pretest_covR = 1 -
##   as.numeric(pretest_cov_VCE)/as.numeric(NULL_VCE)`.
## Caused by warning:
## ! NAs introduced by coercion
```

```r
cov <- icc_cov %>% select(Source_k, level_VCE, covR, demo_covR, demo_cov, pretest_covR, L1_variance_3, L2_variance_3, L3_variance_3, L4_variance_3, ICC_2, ICC_3, ICC_4) %>% pivot_wider(names_from = level_VCE, values_from = c(covR, demo_covR, demo_cov, pretest_covR))
```


### EFFECT SIZE

```r
#effect_size <- effect_size %>% filter(d_adj != "adj" | is.na(d_adj))
eff_ss <- left_join(effect_size, sample_size, "Source_k")
eff_ss <- eff_ss %>% select(-L1_variance_3, -L2_variance_3, -L3_variance_3, -L4_variance_3, -ICC_1, -ICC_2, -ICC_3, -ICC_4, -L1_variance_2, -L2_variance_2,)
eff_ss <- left_join(eff_ss, cov, "Source_k")

## Drop if contrast is NA
eff_ss <- eff_ss %>% drop_na(d_contrast)

## Set contrast sample sizes:
eff_ss <- eff_ss %>% mutate(L1_condition_N_1 = if_else(d_contrast == "2v3", L1_condition_N.2, L1_condition_N.1),
                            L1_condition_N_2 = if_else(d_contrast == "1v2", L1_condition_N.2, L1_condition_N.3),
                            L2_condition_N_1 = if_else(d_contrast == "2v3", L2_condition_N.2, L2_condition_N.1),
                            L2_condition_N_2 = if_else(d_contrast == "1v2", L2_condition_N.2, L2_condition_N.3),
                            L3_condition_N_1 = if_else(d_contrast == "2v3", L3_condition_N.2, L3_condition_N.1),
                            L3_condition_N_2 = if_else(d_contrast == "1v2", L3_condition_N.2, L3_condition_N.3),
                            L4_condition_N_1 = if_else(d_contrast == "2v3", L4_condition_N.2, L4_condition_N.1),
                            L4_condition_N_2 = if_else(d_contrast == "1v2", L4_condition_N.2, L4_condition_N.3))
```

```r
## Time transformations
eff_ss <- eff_ss %>% dplyr::mutate(duration = if_else(standard_time_interval == "Yes", as.numeric(measurement_interval_num) * (num_timepoints-1), 
                                                      if_else(num_timepoints ==2, as.numeric(gsub("[^0-9.-]", "", follow_up1)), 
                                                              if_else(num_timepoints ==3, as.numeric(gsub("[^0-9.-]", "", follow_up2)),
                                                                      if_else(num_timepoints ==4, as.numeric(gsub("[^0-9.-]", "", follow_up3)), 3)))),
                                   follow_up1 = if_else(standard_time_interval == "Yes", as.numeric(measurement_interval_num), 
                                                        as.numeric(gsub("[^0-9.-]", "", follow_up1))))
```

```r
## Raw SD transformation
eff_ss <- eff_ss %>% dplyr::mutate(num_levels = as.numeric(gsub("\\+", "", `num_levels`)),
                                   d_raw_SD = if_else(!is.na(d_raw_SD), as.numeric(d_raw_SD),
                                                      if_else(num_levels == 4, sqrt(as.numeric(L1_variance_3) + as.numeric(L2_variance_3) + as.numeric(L3_variance_3) + as.numeric(L4_variance_3)),
                                                              if_else(num_levels == 3, sqrt(as.numeric(L1_variance_3) + as.numeric(L2_variance_3) + as.numeric(L3_variance_3)),
                                                                      if_else(num_levels == 2, sqrt(as.numeric(L1_variance_3) + as.numeric(L2_variance_3)), NA))))) 
## Effect size transformations
eff_ss <- eff_ss %>% dplyr::mutate(
  d_cont = as.numeric(reg_coef)/as.numeric(d_raw_SD), # endpoint analysis or categorical time
  d_binary = as.numeric(reg_coef) * sqrt(3)/pi, #it spits out a logit and its converted
  ## Time-varying effect size
  d_long_cat = as.numeric(reg_coef_tx)/as.numeric(d_raw_SD), # Categorical time reg_coef is already multiplied by the duration. 
  d_long_piecewise = (as.numeric(reg_coef_tx)*follow_up1)/as.numeric(d_raw_SD), # Input directly into distillerSR
  d_long_linear = (as.numeric(reg_coef_tx)*as.numeric(duration))/as.numeric(d_raw_SD),
  d_long_quadratic = ((as.numeric(reg_coef_tx)*as.numeric(duration)) + ((as.numeric(reg_coef_tx_q)*as.numeric(duration)^2)))/as.numeric(d_raw_SD),
  ## Extract effect size from t-value or p-value (Need to add bias correction)
  d_t_test =  if_else(d_CR_type == "t", as.numeric(d_CR) * sqrt(1/as.numeric(L1_condition_N_1) + 1/as.numeric(L1_condition_N_2)), NA),
  d_from_pval =  pt(as.numeric(gsub("[^0-9.-]", "", d_pval))/2, 100) * sqrt(1/as.numeric(L1_condition_N_1) + 1/as.numeric(L1_condition_N_2)),
  d_author = as.numeric(gsub("[^0-9.-]", "", d_author)))
## Warning: There were 2 warnings in `dplyr::mutate()`.
## The first warning was:
## ℹ In argument: `d_long_quadratic = `/`(...)`.
## Caused by warning:
## ! NAs introduced by coercion
## ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
```
### COMPONENTS FOR VARIANCE ESTIMATES

```r
eff_ss <- eff_ss %>% 
  mutate(tot_v2 = if_else(num_levels == 2, as.numeric(L1_variance_3)+as.numeric(L2_variance_3), NA), # Total variance at level 2
         tot_v3 = if_else(num_levels == 3, as.numeric(L1_variance_3)+as.numeric(L2_variance_3)+as.numeric(L3_variance_3), NA), # Total variance at level 3
         tot_v4 = if_else(num_levels == 4, as.numeric(L1_variance_3)+as.numeric(L2_variance_3)+as.numeric(L3_variance_3)+as.numeric(L4_variance_3), NA), # Total variance at level 4

         L1_ss = if_else(is.na(L1_ss), as.numeric(L1_condition_N_1) + as.numeric(L1_condition_N_2), as.numeric(L1_ss)), # Add L1 conditions sample size together to make L1 sample size, if L1 sample size isn't already there.
         L2_ss = if_else(is.na(L2_ss), as.numeric(L2_condition_N_1) + as.numeric(L2_condition_N_2), as.numeric(L2_ss)), # .
         L3_ss = if_else(is.na(L3_ss), as.numeric(L3_condition_N_1) + as.numeric(L3_condition_N_2), as.numeric(L3_ss)), # .
         L4_ss = if_else(is.na(L4_ss), as.numeric(L4_condition_N_1) + as.numeric(L4_condition_N_2), as.numeric(L4_ss)), 
         vi_r2_1 = 4*covR_1*(1-covR_1)^2 / L1_ss, # Variance of COV-R^2 at L1
         vi_r2_2 = 4*covR_2*(1-covR_2)^2 / L2_ss,
         vi_r2_3 = 4*covR_3*(1-covR_3)^2 / L3_ss,
        clus_1_2 = L1_ss/L2_ss, # Average number of L1 obs in L2 clusters.
        #clus_1_3 = L1_ss/L3_ss, # Average number of L1 obs in L3 clusters.
        clus_2_3 = L2_ss/L3_ss, # Average number of L2 obs in L3 clusters.
        clus_3_4 = L3_ss/L4_ss, # Average number of L3 obs in L4 clusters.
        v2 = as.numeric(L2_variance_3_se)^2, # Variance of L2 Variance
        v3 = as.numeric(L3_variance_3_se)^2, # Variance of L3 Variance
        v4 = as.numeric(L4_variance_3_se)^2, # Variance of L4 Variance
        vi_icc_2 = if_else(num_levels == 2, (2*(1-ICC_2)^2 * (1+(clus_1_2-1)*ICC_2)^2)/(clus_1_2*(clus_1_2-1)*(L2_ss-1)), # Variance of ICC @ L2
                       if_else(num_levels == 3, (((clus_2_3*(1-ICC_2)^2 + 2*ICC_2*(1-ICC_2))*v2)/(clus_2_3*tot_v3^2)) + ((ICC_2^2 * v3)/tot_v3^2),
                               if_else(num_levels == 4, ((clus_3_4*clus_2_3^2*(1-ICC_2)^2+2*clus_3_4*clus_2_3*ICC_2*(1-ICC_2)+2*ICC_2^2)*v2)/(clus_3_4*clus_2_3^2*tot_v4^2)+((clus_3_4-2)*ICC_2^2*v3)/(clus_3_4*tot_v4^2)+(ICC_2^2*v4)/tot_v4^2, NA))),
                       
        vi_icc_3 = if_else(num_levels == 3, (((clus_2_3*(ICC_3)^2 + 2*ICC_3*(1-ICC_3))*v2)/(clus_2_3*tot_v3^2)) + (((1-ICC_3)^2 * v3)/tot_v3^2),
                           
                         if_else(num_levels == 4, (((clus_3_4*clus_2_3^2*ICC_3^2 + 2*(clus_3_4*clus_2_3 - 1)*ICC_3*(1-ICC_3))*v2)/(clus_3_4*clus_2_3^2*tot_v4^2))+ ((((clus_3_4*(1-ICC_3)^2)+(2*ICC_3*(1-ICC_3)))*v3)/(clus_3_4*tot_v4^2))+((ICC_3^2*v4)/tot_v4^2), NA)),
        
        vi_icc_4 = if_else(num_levels == 4, (((clus_3_4*clus_2_3*(clus_2_3 - 2)*ICC_4^2 - 2*ICC_4*(1-ICC_4))*v2)/(clus_3_4*clus_2_3^2*tot_v4^2))+(((clus_3_4*ICC_4^2+2*ICC_4*(1-ICC_4))*v3)/(clus_3_4*tot_v4^2))+((1-ICC_4)^2*v4)/tot_v4^2, NA)) # Variance of ICC @ L3 
```

### PIVOT LONGER TO CREATE EFFECT SIZE PARAMETER COMPONENTS:


```r
eff_ss_long <- eff_ss %>% pivot_longer(c("d_author", "d_binary", "d_cont", "d_long_piecewise", "d_long_cat", "d_long_linear", "d_long_quadratic", "d_t_test", "d_from_pval"), names_to = "dtype", values_to = "dval") #%>% drop_na(dval)

eff_ss_long <- eff_ss_long %>% 
  mutate(analysis_type = if_else(str_detect(analysis_type, "binary") == T, "binary", 
                                                                      if_else(str_detect(analysis_type, "cont") == T, "cont",
                                                                              if_else(analysis_type == "ep", "cont", "long"))),
         analysis_match = if_else(dtype == "d_cont" & analysis_type == "cont" & data_source == "PI", 1, 
                                  if_else(dtype == "d_binary" & analysis_type == "binary" & data_source == "PI", 1, 
                                    if_else(dtype == "d_long_cat" & time_modeled == "cat", 1, 
                                     if_else(dtype == "d_long_piecewise" & time_modeled == "piecewise", 1,
                                          if_else(dtype == "d_long_linear" & time_modeled == "linear", 1,
                                                  if_else(dtype == "d_long_quadratic" & time_modeled == "quad",  1, 
                                                          if_else(dtype == "d_author" & data_source == "Paper" & analysis_type == "cont", 1,
                                                                  if_else(dtype == "d_from_pval" & data_source == "Paper" & analysis_type == "cont", 1,
                                                                          if_else(dtype == "d_t_test" & data_source == "Paper"& analysis_type == "cont", 1, 0))))))))),
    num_levels = as.numeric(gsub("\\+", "", `num_levels`)),
    vi = if_else(dtype == "d_cont" & num_levels == 2, (as.numeric(reg_coef_se)^2/tot_v2) + (as.numeric(reg_coef)^2 * as.numeric(L2_variance_3_se)^2)/(4*(tot_v2)^3),
                 if_else(dtype == "d_cont" & num_levels == 3, (as.numeric(reg_coef_se)^2/tot_v3) + (as.numeric(reg_coef)^2 * (as.numeric(L2_variance_3_se)^2 + as.numeric(L3_variance_3_se)^2))/(4*tot_v3^3),
                         if_else(dtype == "d_cont" & num_levels == 4, (as.numeric(reg_coef_se)^2/tot_v4) + (as.numeric(reg_coef)^2 * (as.numeric(L2_variance_3_se)^2 + as.numeric(L3_variance_3_se)^2 + as.numeric(L4_variance_3_se)^2))/(4*tot_v4^3),
                         if_else(dtype == "d_long_linear" | dtype == "d_long_quadratic" | dtype == "d_long_piecewise" | dtype == "d_long_cat", as.numeric(reg_coef_tx_se)^2 * (as.numeric(dval)/as.numeric(reg_coef_tx))^2, 
                                                 if_else(dtype == "d_binary", 3*as.numeric(reg_coef_se)^2/pi^2, NA)))))) %>% filter(analysis_match == 1) %>% select(Refid:software, L1_ss:num_levels, d_contrast:d_adj, L1_variance_3:L4_variance_3, follow_up1, duration, analysis_type, time_modeled, dtype:vi)

```

## META-ANALYZE:

### ICC AND COV R^2
 * Analyzed at the outcome level.
 

```r
icc_cov_meta <- eff_ss %>% select(Refid, Source_k, data_source:L3_ss, L1_analysis_name, L2_analysis_name, L3_analysis_name, L4_analysis_name, L4_ss, num_levels, L1_variance_3_se, L2_variance_3_se, L3_variance_3_se, L4_variance_3_se, L1_variance_3:pretest_covR_2, tot_v2:vi_icc_4)
icc_cov_meta <- icc_cov_meta %>% separate(col = Source_k, into = c("study", "determinant", "dv_name", "dv_source"), sep = "\\|") %>% select(-dv_source)
icc_cov_meta <- left_join(icc_cov_meta, outcomes, c("Refid", "dv_name"))
metadata <- metadata %>% select(Refid, Author, hybrid_type:rand_L4)
icc_cov_meta <- left_join(icc_cov_meta, metadata, "Refid")


icc_meta_pi <- icc_cov_meta %>% filter(data_source == "PI") %>% distinct() %>% select(Author, Refid, dv_name, study, analysis_type, dv_outcome, dv_measure_approach, L1_name, L2_name, L3_name, L1_analysis_name, L2_analysis_name, L3_analysis_name, L4_analysis_name, num_levels, L1_variance_3, L2_variance_3, L3_variance_3, L4_variance_3, L1_variance_3_se, L2_variance_3_se, L3_variance_3_se, L4_variance_3_se, L1_ss, L2_ss, L3_ss, L4_ss, clus_1_2, clus_1_2, ICC_2, vi_icc_2, ICC_3, vi_icc_3, ICC_4, vi_icc_4, covR_1, vi_r2_1, covR_2, vi_r2_2)

cov_meta_pi <- icc_cov_meta %>% filter(data_source == "PI") %>% distinct() %>% select(Author, Refid, dv_name, study, analysis_type, dv_outcome, dv_measure_approach, L1_name, L2_name, L3_name, L1_analysis_name, L2_analysis_name, L3_analysis_name, L4_analysis_name, num_levels, L1_variance_3, L2_variance_3, L3_variance_3, L4_variance_3, L1_variance_3_se, L2_variance_3_se, L3_variance_3_se, covR_1, vi_r2_1, covR_2, vi_r2_2)

#icC_cov_meta_paper <- icc_cov_meta %>% filter(data_source == "Paper") %>% distinct() %>% select(Author, Refid, dv_name, study, analysis_type, dv_outcome, L1_name, L2_name, L3_name, L4_name, num_levels, L1_variance_3, L2_variance_3, L3_variance_3, L4_variance_3, L1_variance_3_se, L2_variance_3_se, L3_variance_3_se, L4_variance_3_se, L1_ss, L2_ss, L3_ss, L4_ss, clus_1_2, clus_1_3, clus_1_2, ICC_2, vi_icc_2, ICC_3, vi_icc_3, covR_1, vi_r2_1, covR_2, vi_r2_2)
```

```r
icc_meta_pi <- icc_meta_pi %>% 
  mutate(L1_name = if_else(is.na(L1_name), L1_analysis_name, L1_name),
         L2_name= if_else(is.na(L2_name), L2_analysis_name, L2_name),
         L3_name= if_else(is.na(L3_name), L3_analysis_name, L3_name),
         L4_name= L4_analysis_name, # Right now, L4_name doesn't exist because there isn't a variable with data at the study characteristics level, only at the outcome level because of the two weird cases where we have differing model structures depending on the outcome in Epstein and Cara Lewis.
         
         analysis_type = if_else(str_detect(analysis_type, "binary") == T, "binary", 
                                                                      if_else(str_detect(analysis_type, "cont") == T, "cont",
                                                                              if_else(analysis_type == "ep", "cont", "long"))),
         longitudinal = if_else(analysis_type == "long", 1, 0),
         outcome_type = if_else(dv_outcome == "symptoms" | dv_outcome == "functioning", "clinical", "implementation"),
         measurement_approach = if_else(dv_measure_approach == "provider" | dv_measure_approach == "participant", "self_report", 
                                        if_else(is.na(dv_measure_approach), NA, "coded/recorded")))

icc_meta_pi <- icc_meta_pi %>% pivot_longer(c(ICC_2, ICC_3, ICC_4), names_to = "level_ICC", values_to = "ICC") %>% 
  mutate(analysis_index = paste0(analysis_type, num_levels, level_ICC)) %>% mutate(cluster_level = if_else(level_ICC == "ICC_2", L2_name,
                                                                                                           if_else(level_ICC == "ICC_3", L3_name, NA)), 
                                                                                   ICC_vi = if_else(level_ICC == "ICC_2", vi_icc_2,
                                                                                                    if_else(level_ICC == "ICC_3", vi_icc_3, 
                                                                                                            if_else(level_ICC == "ICC_4", vi_icc_4, NA))))
```


```r
# Make the non-longitudinal, first-cluster data set.
icC_cov_meta_pi_L2 <- icc_meta_pi %>% filter(analysis_index == "long4ICC_3" | analysis_index == "long3ICC_3" | analysis_index == "cont3ICC_2" | analysis_index == "binary3ICC_2" | analysis_index == "cont2ICC_2" | analysis_index == "binary2ICC_2") %>% filter(!is.na(ICC))

# Make the non-longitudinal, second-cluster data set.

icC_cov_meta_pi_L3 <- icc_meta_pi %>% filter(analysis_index == "long4ICC_4" | analysis_index == "cont3ICC_3" | analysis_index == "binary3ICC_3")

# Make the longitudinal data set
icC_cov_meta_pi_long <- icc_meta_pi %>% filter(analysis_index == "long4ICC_2" | analysis_index == "long3ICC_2" | analysis_index == "long2ICC_2")

# Fix the observations of Epstein's 4-level variable.
#icC_cov_meta_pi_long[4, 8] <- "obs"
#icC_cov_meta_pi_long[4, 9] <- "patient"
#icC_cov_meta_pi_long[4, 10] <- "provider"
#icC_cov_meta_pi_long[4, 11] <- "site"
```

```r
summary(icC_cov_meta_pi_L2$ICC)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.00000 0.05742 0.08781 0.12913 0.24039 0.38376
summary(icC_cov_meta_pi_L3$ICC)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.00000 0.06293 0.09229 0.10073 0.14634 0.19431
summary(icC_cov_meta_pi_long$ICC)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.1193  0.2134  0.3526  0.3810  0.5186  0.7074


IQR(icC_cov_meta_pi_L2$ICC)
## [1] 0.1829735
IQR(icC_cov_meta_pi_L3$ICC)
## [1] 0.08340827
IQR(icC_cov_meta_pi_long$ICC)
## [1] 0.3051323
```
### Visualize ICC and COV R^2 Estimate

```r
meta_icc_L2 <- icC_cov_meta_pi_L2 %>% filter(!is.na(ICC_vi))
meta_icc_L3 <- icC_cov_meta_pi_L3 %>% filter(!is.na(ICC_vi)) 
meta_icc_long <- icC_cov_meta_pi_long %>% filter(!is.na(ICC_vi)) 

ma_model_2 <- rma.mv(yi = ICC, V = ICC_vi, random = ~ 1 | dv_name/study, data = meta_icc_L2)
ma_model_2i <- rma.mv(yi = ICC, V = ICC_vi, mods = as.factor(measurement_approach), random = ~ 1 | dv_name/study, data = meta_icc_L2) # Test for moderating force of measurement approach.

ma_model_3 <- rma.mv(yi = ICC, V = ICC_vi, random = ~ 1 | dv_name/study, data = meta_icc_L3)
ma_model_2_long <- rma.mv(yi = ICC, V = ICC_vi, random = ~ 1 | dv_name/study, data = meta_icc_long)

summary(ma_model_2)
## 
## Multivariate Meta-Analysis Model (k = 17; method: REML)
## 
##   logLik  Deviance       AIC       BIC      AICc   
##   8.3507  -16.7014  -10.7014   -8.3836   -8.7014   
## 
## Variance Components:
## 
##             estim    sqrt  nlvls  fixed         factor 
## sigma^2.1  0.0076  0.0870     17     no        dv_name 
## sigma^2.2  0.0076  0.0870     17     no  dv_name/study 
## 
## Test for Heterogeneity:
## Q(df = 16) = 997.2032, p-val < .0001
## 
## Model Results:
## 
## estimate      se    zval    pval   ci.lb   ci.ub      
##   0.1466  0.0371  3.9506  <.0001  0.0739  0.2194  *** 
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
summary(ma_model_3)
## 
## Multivariate Meta-Analysis Model (k = 5; method: REML)
## 
##   logLik  Deviance       AIC       BIC      AICc   
##   5.6090  -11.2181   -5.2181   -7.0592   18.7819   
## 
## Variance Components:
## 
##             estim    sqrt  nlvls  fixed         factor 
## sigma^2.1  0.0004  0.0209      5     no        dv_name 
## sigma^2.2  0.0004  0.0209      5     no  dv_name/study 
## 
## Test for Heterogeneity:
## Q(df = 4) = 6.6893, p-val = 0.1532
## 
## Model Results:
## 
## estimate      se    zval    pval   ci.lb   ci.ub      
##   0.1023  0.0208  4.9201  <.0001  0.0615  0.1430  *** 
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
summary(ma_model_2_long)
## 
## Multivariate Meta-Analysis Model (k = 11; method: REML)
## 
##   logLik  Deviance       AIC       BIC      AICc   
##   1.7688   -3.5375    2.4625    3.3703    6.4625   
## 
## Variance Components:
## 
##             estim    sqrt  nlvls  fixed         factor 
## sigma^2.1  0.0193  0.1388     11     no        dv_name 
## sigma^2.2  0.0193  0.1388     11     no  dv_name/study 
## 
## Test for Heterogeneity:
## Q(df = 10) = 9898.7726, p-val < .0001
## 
## Model Results:
## 
## estimate      se    zval    pval   ci.lb   ci.ub      
##   0.3925  0.0618  6.3533  <.0001  0.2714  0.5135  *** 
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

forest(ma_model_2, slab = paste(gsub(",", "", str_sub(meta_icc_L2$Author, 1, 10)), str_sub(meta_icc_L2$dv_name, 1, 10), cluster_level, sep = ", "))
title("ICC, Non-longitudinal clusters L2")
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-21-1.png" width="672" />

```r

forest(ma_model_3, slab = paste(gsub(",", "", str_sub(meta_icc_L3$Author, 1, 10)), str_sub(meta_icc_L3$dv_name, 1, 10), cluster_level, sep = ", "))
title("ICC, Non-longitudinal clusters L3")
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-21-2.png" width="672" />

```r

forest(ma_model_2_long, slab = paste(gsub(",", "", str_sub(meta_icc_long$Author, 1, 10)), str_sub(meta_icc_long$dv_name, 1, 10), cluster_level, sep = ", "))
title("ICC, Longitudinal clusters")
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-21-3.png" width="672" />

```r


clubSandwich::coef_test(ma_model_2, vcov = "CR2")# RVE using small-sample adjustment from Tipton and Pustejovsky (2015)
## Registered S3 method overwritten by 'clubSandwich':
##   method    from    
##   bread.mlm sandwich
##    Coef. Estimate     SE t-stat d.f. (Satt) p-val (Satt) Sig.
##  intrcpt    0.147 0.0374   3.92          13      0.00175   **
clubSandwich::coef_test(ma_model_3, vcov = "CR2")# RVE using small-sample adjustment from Tipton and Pustejovsky (2015)
##    Coef. Estimate     SE t-stat d.f. (Satt) p-val (Satt) Sig.
##  intrcpt    0.102 0.0193   5.31        2.41       0.0223    *
clubSandwich::coef_test(ma_model_2_long, vcov = "CR2")# RVE using small-sample adjustment from Tipton and Pustejovsky (2015)
##    Coef. Estimate     SE t-stat d.f. (Satt) p-val (Satt) Sig.
##  intrcpt    0.392 0.0618   6.35        9.89       <0.001  ***
```
#### Mixed effects model
##### Account for clustering in the results, but don't meta-analyze

```r
library(lme4) # Mixed effects models
#lm(ICC ~ 1, data = icC_cov_meta_pi_L2)

m1 = lmer(ICC ~ 1 + (1 | Refid), data = icC_cov_meta_pi_L2) # Level 2

summary(lmer(ICC ~ outcome_type + (1 | Refid), data = icC_cov_meta_pi_L2)) # Level 2
## Linear mixed model fit by REML ['lmerMod']
## Formula: ICC ~ outcome_type + (1 | Refid)
##    Data: icC_cov_meta_pi_L2
## 
## REML criterion at convergence: -28.8
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.3309 -0.5015 -0.2599  0.4132  1.6961 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  Refid    (Intercept) 0.009748 0.09873 
##  Residual             0.007166 0.08465 
## Number of obs: 21, groups:  Refid, 6
## 
## Fixed effects:
##                            Estimate Std. Error t value
## (Intercept)                 0.08038    0.07659   1.049
## outcome_typeimplementation  0.09683    0.07623   1.270
## 
## Correlation of Fixed Effects:
##             (Intr)
## otcm_typmpl -0.801
summary(lmer(ICC ~ measurement_approach + (1 | Refid), data = icC_cov_meta_pi_L2)) # Level 2
## Linear mixed model fit by REML ['lmerMod']
## Formula: ICC ~ measurement_approach + (1 | Refid)
##    Data: icC_cov_meta_pi_L2
## 
## REML criterion at convergence: -28.2
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -1.23664 -0.68167  0.04445  0.38071  2.00374 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  Refid    (Intercept) 0.018691 0.13672 
##  Residual             0.006253 0.07907 
## Number of obs: 21, groups:  Refid, 6
## 
## Fixed effects:
##                                 Estimate Std. Error t value
## (Intercept)                      0.13266    0.06476   2.049
## measurement_approachself_report  0.08319    0.07091   1.173
## 
## Correlation of Fixed Effects:
##             (Intr)
## msrmnt_ppr_ -0.391


m2 = lmer(ICC ~ 1 + (1 | Refid), data = icC_cov_meta_pi_L3) # Level 3
m3 = lmer(ICC ~ 1 + (1 | Refid), data = icC_cov_meta_pi_long) # Longitudinal

m1
## Linear mixed model fit by REML ['lmerMod']
## Formula: ICC ~ 1 + (1 | Refid)
##    Data: icC_cov_meta_pi_L2
## REML criterion at convergence: -30.6038
## Random effects:
##  Groups   Name        Std.Dev.
##  Refid    (Intercept) 0.11002 
##  Residual             0.08383 
## Number of obs: 21, groups:  Refid, 6
## Fixed Effects:
## (Intercept)  
##      0.1597
stats::confint(m1, method="Wald")
##                  2.5 %    97.5 %
## .sig01              NA        NA
## .sigma              NA        NA
## (Intercept) 0.06176902 0.2576201

m2
## Linear mixed model fit by REML ['lmerMod']
## Formula: ICC ~ 1 + (1 | Refid)
##    Data: icC_cov_meta_pi_L3
## REML criterion at convergence: -13.3221
## Random effects:
##  Groups   Name        Std.Dev.
##  Refid    (Intercept) 0.03987 
##  Residual             0.06254 
## Number of obs: 7, groups:  Refid, 3
## Fixed Effects:
## (Intercept)  
##     0.08718
stats::confint(m2, method="Wald")
##                  2.5 %    97.5 %
## .sig01              NA        NA
## .sigma              NA        NA
## (Intercept) 0.01723689 0.1571276

m3
## Linear mixed model fit by REML ['lmerMod']
## Formula: ICC ~ 1 + (1 | Refid)
##    Data: icC_cov_meta_pi_long
## REML criterion at convergence: -10.9698
## Random effects:
##  Groups   Name        Std.Dev.
##  Refid    (Intercept) 0.19350 
##  Residual             0.09057 
## Number of obs: 14, groups:  Refid, 6
## Fixed Effects:
## (Intercept)  
##      0.4092
stats::confint(m3, method="Wald")
##                 2.5 %    97.5 %
## .sig01             NA        NA
## .sigma             NA        NA
## (Intercept) 0.2462642 0.5721137
```
#### Clustered Standard Errors

```r
library(sandwich) # Working with clustered covariances
library(miceadds) # AUtomated function to work with clustered-random errors
## Loading required package: mice
## 
## Attaching package: 'mice'
## The following object is masked from 'package:stats':
## 
##     filter
## The following objects are masked from 'package:base':
## 
##     cbind, rbind
## * miceadds 3.16-18 (2023-01-06 10:54:00)
m1 = lm.cluster(ICC ~ 1, cluster = 'Refid', data = icC_cov_meta_pi_L2)
summary(lm.cluster(ICC ~ longitudinal, cluster = 'Refid', data = icC_cov_meta_pi_L2))
## R^2= 0.00402 
## 
##                Estimate Std. Error   t value     Pr(>|t|)
## (Intercept)  0.12443510 0.02788415 4.4625753 8.098046e-06
## longitudinal 0.01642071 0.11034674 0.1488101 8.817034e-01
summary(lm.cluster(ICC ~ outcome_type, cluster = 'Refid', data = icC_cov_meta_pi_L2))
## R^2= 0.10733 
## 
##                              Estimate Std. Error  t value    Pr(>|t|)
## (Intercept)                0.03518436 0.01864113 1.887459 0.059098669
## outcome_typeimplementation 0.10959943 0.03801050 2.883399 0.003934089
summary(lm.cluster(ICC ~ measurement_approach, cluster = 'Refid', data = icC_cov_meta_pi_L2))
## R^2= 0.00692 
## 
##                                  Estimate Std. Error  t value   Pr(>|t|)
## (Intercept)                     0.1229657 0.05056761 2.431710 0.01502775
## measurement_approachself_report 0.0215635 0.07068331 0.305072 0.76031129

m2 = lm.cluster(ICC ~ 1, cluster = 'Refid', data = icC_cov_meta_pi_L3)
m3 = lm.cluster(ICC ~ 1, cluster = 'Refid', data = icC_cov_meta_pi_long)
summary(lm.cluster(ICC ~ outcome_type, cluster = 'Refid', data = icC_cov_meta_pi_long))
## R^2= 0.07601 
## 
##                             Estimate Std. Error   t value   Pr(>|t|)
## (Intercept)                0.2977709  0.1035684 2.8751121 0.00403884
## outcome_typeimplementation 0.1164576  0.1243373 0.9366263 0.34895083
summary(lm.cluster(ICC ~ measurement_approach, cluster = 'Refid', data = icC_cov_meta_pi_long))
## R^2= 0.09609 
## 
##                                  Estimate Std. Error  t value     Pr(>|t|)
## (Intercept)                     0.3126508 0.06507261 4.804646 1.550259e-06
## measurement_approachself_report 0.1195321 0.11320318 1.055907 2.910105e-01


m1
## $lm_res
## 
## Call:
## stats::lm(formula = formula, data = data, weights = wgt__)
## 
## Coefficients:
## (Intercept)  
##      0.1291  
## 
## 
## $vcov
##             (Intercept)
## (Intercept) 0.001300772
## 
## attr(,"class")
## [1] "lm.cluster"
stats::confint(m1, method="Wald")
##                  2.5 %    97.5 %
## (Intercept) 0.05843826 0.1998152

m2
## $lm_res
## 
## Call:
## stats::lm(formula = formula, data = data, weights = wgt__)
## 
## Coefficients:
## (Intercept)  
##      0.1007  
## 
## 
## $vcov
##              (Intercept)
## (Intercept) 0.0006776838
## 
## attr(,"class")
## [1] "lm.cluster"
stats::confint(m2, method="Wald")
##                  2.5 %    97.5 %
## (Intercept) 0.04970978 0.1517548

m3
## $lm_res
## 
## Call:
## stats::lm(formula = formula, data = data, weights = wgt__)
## 
## Coefficients:
## (Intercept)  
##       0.381  
## 
## 
## $vcov
##             (Intercept)
## (Intercept) 0.006245564
## 
## attr(,"class")
## [1] "lm.cluster"
stats::confint(m3, method="Wald")
##                 2.5 %    97.5 %
## (Intercept) 0.2260611 0.5358486
```

#### Plot ICC Estimates

```r
# I manually input the estimates into a separate Excel workbook.
#icc_est <- read_xlsx("ICC_estimates.xlsx")
library(ggplot2)

#icc_plot <- ggplot(data = icc_est, mapping = aes(y = analysis, x= estimate, color = est_type)) + geom_point(position = position_dodge(width = 1), aes(shape = analysis, size = n_studies), show.legend = F) + geom_linerange(aes(xmin = lowerbound, xmax = upperbound), position = position_dodge(width = 1), size = .8) + xlab("ICC Estiamte") + coord_flip()

#icc_plot
#ggsave("ICC_estimates_plot.png", icc_plot)
```



## COV R-SQUARED

```r
meta_r2_L2 <- cov_meta_pi %>% filter(!is.na(vi_r2_1) & vi_r2_1 > 0)
meta_r2_L3 <- cov_meta_pi %>% filter(!is.na(vi_r2_2) & vi_r2_2 > 0)
#meta_r2_L4 <- cov_meta_pi %>% filter(!is.na(vi_r2_3) & vi_r2_3 > 0)

## COV R-Sq

ma_model_4 <- rma.mv(yi = covR_1, V = vi_r2_1, random = ~ 1 | dv_name/Refid, data = meta_r2_L2)
summary(ma_model_4)
## 
## Multivariate Meta-Analysis Model (k = 7; method: REML)
## 
##   logLik  Deviance       AIC       BIC      AICc   
##   6.9449  -13.8898   -7.8898   -8.5145    4.1102   
## 
## Variance Components:
## 
##             estim    sqrt  nlvls  fixed         factor 
## sigma^2.1  0.0012  0.0344      7     no        dv_name 
## sigma^2.2  0.0012  0.0344      7     no  dv_name/Refid 
## 
## Test for Heterogeneity:
## Q(df = 6) = 23.3892, p-val = 0.0007
## 
## Model Results:
## 
## estimate      se    zval    pval    ci.lb   ci.ub    
##   0.0484  0.0258  1.8765  0.0606  -0.0022  0.0990  . 
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

ma_model_5 <- rma.mv(yi = covR_2, V = vi_r2_2, random = ~ 1 | dv_name/Refid, data = meta_r2_L3)
summary(ma_model_5)
## 
## Multivariate Meta-Analysis Model (k = 11; method: REML)
## 
##   logLik  Deviance       AIC       BIC      AICc   
##  -2.1801    4.3602   10.3602   11.2679   14.3602   
## 
## Variance Components:
## 
##             estim    sqrt  nlvls  fixed         factor 
## sigma^2.1  0.0418  0.2044     11     no        dv_name 
## sigma^2.2  0.0418  0.2044     11     no  dv_name/Refid 
## 
## Test for Heterogeneity:
## Q(df = 10) = 951.3831, p-val < .0001
## 
## Model Results:
## 
## estimate      se    zval    pval   ci.lb   ci.ub      
##   0.3537  0.0960  3.6827  0.0002  0.1654  0.5419  *** 
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#ma_model_6 <- rma.mv(yi = covR_3, V = vi_r2_3, random = ~ 1 | dv_name/Refid, data = meta_r2_L4)
#summary(ma_model_6)

forest(ma_model_4, slab = paste(str_sub(meta_r2_L2$Author, 1, 10), str_sub(meta_r2_L2$dv_name, 1, 20), sep = ", "))
title("Covariate R-Squared, Level 2")
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-25-1.png" width="672" />

```r

forest(ma_model_5, slab = paste(str_sub(meta_r2_L3$Author, 1, 10), str_sub(meta_r2_L3$dv_name, 1, 20), sep = ", "))
title("Covariate R-Squared, Level 3")
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-25-2.png" width="672" />

```r

#forest(ma_model_6, slab = paste(str_sub(meta_r2_L3$Author, 1, 10), str_sub(meta_r2_L3$dv_name, 1, 20), sep = ", "))
#title("Covariate R-Squared, Level 4")
```


```r
#d_meta <- eff_ss_long %>% select(Refid, Source_k, d_contrast, data_source:L3_ss, reg_coef, reg_coef_se, d_adj, duration, dtype:vi)
d_meta <- eff_ss_long %>% separate(col = Source_k, into = c("study", "determinant", "dv_name", "dv_source"), sep = "\\|") %>% select(-dv_source)

## Create a variable that identifies if the calculated d value aligns with the analysis conducted.
d_meta <- d_meta %>% select(-Author, -Title) %>% mutate(d_tp = if_else(is.na(d_tp), 1, is.numeric(d_tp)),
                                                                       d_adj = if_else(is.na(d_adj), "unadj", d_adj))

```


```r
d_meta_wide <- d_meta %>% group_by(Refid, dv_name, d_adj, dtype) %>% summarise(dval = mean(dval, na.rm = T)) %>% pivot_wider(names_from = c(d_adj, dtype), values_from = dval) %>% ungroup()
## `summarise()` has grouped output by 'Refid', 'dv_name', 'd_adj'. You can
## override using the `.groups` argument.

d_meta_wide <- left_join(d_meta_wide, outcomes, c("Refid", "dv_name"))
d_meta_wide <- left_join(d_meta_wide, metadata, "Refid")
d_meta_wide <- left_join(d_meta_wide, imp_strategies_wide, c("Refid", "IVs_k"))

#write.csv(d_meta_wide, "d_meta_wide.csv", na="")
```



```r
# Join study characteristics dataset with effect size dataset.
d_meta <- left_join(d_meta, outcomes, c("Refid", "dv_name"))
d_meta <- left_join(d_meta, metadata, "Refid")
d_meta <- left_join(d_meta, imp_strategies_wide, c("Refid", "IVs_k"))


d_meta_wide <- d_meta %>% group_by(dv_name, data_source, d_adj, d_contrast) %>% slice(which.min(d_tp)) %>% pivot_wider(names_from = c(data_source, d_adj, d_contrast, dtype), values_from = dval)

```

```r
## Index the contrast metadata to the appropriate contrast.
d_meta <- d_meta %>% mutate(prim_name = if_else(d_contrast == "1v2" | d_contrast == "1v3" | d_contrast == "1v4", name_1, 
                                                if_else(d_contrast == "2v3" | d_contrast == "2v4", name_2, name_3)),
                            comparator_name = if_else(d_contrast == "1v4" | d_contrast == "2v4" | d_contrast == "3v4", name_4, 
                                                if_else(d_contrast == "1v3" | d_contrast == "2v3", name_3, name_2)), 
                            prim_eric = if_else(d_contrast == "1v2" | d_contrast == "1v3" | d_contrast == "1v4", eric_1, 
                                                if_else(d_contrast == "2v3" | d_contrast == "2v4", eric_2, eric_3)),
                            comparator_eric = if_else(d_contrast == "1v4" | d_contrast == "2v4" | d_contrast == "3v4", eric_4, 
                                                if_else(d_contrast == "1v3" | d_contrast == "2v3", eric_3, eric_2)),
                            prim_target1 = if_else(d_contrast == "1v2" | d_contrast == "1v3" | d_contrast == "1v4", target1_1, 
                                                if_else(d_contrast == "2v3" | d_contrast == "2v4", target1_2, target1_3)),
                            comparator_target1 = if_else(d_contrast == "1v4" | d_contrast == "2v4" | d_contrast == "3v4", target1_4, 
                                                         if_else(d_contrast == "1v3" | d_contrast == "2v3", target1_3, target1_2)),
                            prim_target2 = if_else(d_contrast == "1v2" | d_contrast == "1v3" | d_contrast == "1v4", target2_1,
                                                   if_else(d_contrast == "2v3" | d_contrast == "2v4", target2_2, target2_3)),
                            comparator_target2 = if_else(d_contrast == "1v4" | d_contrast == "2v4" | d_contrast == "3v4", target2_4,
                                                         if_else(d_contrast == "1v3" | d_contrast == "2v3", target2_3, target2_2))) %>% select(Refid:rand_L4, prim_name:comparator_target2) 


# Take only d values with a variance, group by outcome and only take the first time point
# If the outcome is symptoms, flip the sign of the d value.
meta_d <- d_meta %>% filter(dval < 3 & dval > -1) %>% group_by(dv_name, d_contrast) %>% slice(which.min(d_tp)) %>% mutate(dval = if_else(dv_outcome == "symptoms", dval*-1, dval))

```


```r
# Subtract implementation strategies from each other.
meta_d <- meta_d %>%
    mutate(
        prim_subtracted = map2(
            str_split(prim_eric, ","),
            str_split(comparator_eric, ","),
            ~ setdiff(.x, .y)
        ) %>% map_chr(~ paste0(.x, collapse = ";")) %>% str_trim(),
        comparator_subtracted = map2(
            str_split(comparator_eric, ","),
            str_split(prim_eric, ","),
            ~ setdiff(.x, .y)
        ) %>% map_chr(~ paste0(.x, collapse = ";")) %>% str_trim(),
        comparator_subtracted = if_else(comparator_subtracted == "NA", "Service as usual", comparator_subtracted),
        effect_size_type = if_else(comparator_subtracted == "Service as usual", "Service as usual",
                                   if_else(comparator_subtracted == "", "Enhanced vs. Standard", "Comparative Effectiveness"))
    )
# Split apart datasets into 3 distinct analytical datasets.

# Make paper dataset, we have 3 different types of "paper" d-values (t-test, p-value directly reported) for now I just took the minimum.
meta_d_paper <- meta_d %>% filter(data_source == "Paper") %>% group_by(dv_name, d_contrast) %>% slice(which.min(d_tp))
meta_d_service_as_usual <- meta_d %>% filter(effect_size_type == "Service as usual" & !is.na(vi))
meta_d_enhancedvstandard <- meta_d %>% filter(effect_size_type == "Enhanced vs. Standard" & !is.na(vi))
meta_d_compeff <- meta_d %>% filter(effect_size_type == "Comparative Effectiveness" & !is.na(vi))
```

### Visualize Effect Size Estiamte


```r
# PLOT FOREST PLOT OF d values from PAPER
library(ggh4x) #https://cjvanlissa.github.io/Doing-Meta-Analysis-in-R/robust-variance-estimation-1.html
meta_d_paper %>% ggplot(aes(x = dval, y = interaction(str_sub(dv_name, 1, 15), dv_outcome, str_sub(Author, 1, 8)))) + geom_point(shape = "square") + geom_vline(aes(xintercept = 0), alpha = 0.5) + scale_y_discrete(guide = "axis_nested")
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-31-1.png" width="672" />


## Strategy vs. Service as usual

```r

# Should we use robust variance estimation: https://www.tandfonline.com/doi/abs/10.1080/13645579.2016.1252189

ma_model_1 <- rma.mv(yi = dval, vi, random = ~ 1 | dv_name/Refid, data = meta_d_service_as_usual)
summary(ma_model_1)
## 
## Multivariate Meta-Analysis Model (k = 8; method: REML)
## 
##   logLik  Deviance       AIC       BIC      AICc   
##  -6.6535   13.3069   19.3069   19.1446   27.3069   
## 
## Variance Components:
## 
##             estim    sqrt  nlvls  fixed         factor 
## sigma^2.1  0.0196  0.1401      8     no        dv_name 
## sigma^2.2  0.0196  0.1401      8     no  dv_name/Refid 
## 
## Test for Heterogeneity:
## Q(df = 7) = 19.4415, p-val = 0.0069
## 
## Model Results:
## 
## estimate      se    zval    pval   ci.lb   ci.ub      
##   0.5566  0.1110  5.0160  <.0001  0.3391  0.7740  *** 
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

forest(ma_model_1, slab = paste(str_sub(meta_d_service_as_usual$Author, 1, 10), str_sub(meta_d_service_as_usual$prim_name, 1, 10), str_sub(meta_d_service_as_usual$dv_name, 1, 20)))
title("Effect Size: Strategy vs. Service as usual")
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-32-1.png" width="672" />

```r

funnel(ma_model_1)
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-32-2.png" width="672" />

```r

clubSandwich::coef_test(ma_model_1, vcov = "CR2") # RVE using method from Tipton and Pustejovsky (2015)
##    Coef. Estimate    SE t-stat d.f. (Satt) p-val (Satt) Sig.
##  intrcpt    0.557 0.111      5        4.24      0.00645   **
```

## Enhanced Implementation Strategy vs. Standard Implementation Strategy:


```r
ma_model_2 <- rma.mv(yi = dval, vi, random = ~ 1 | dv_name/Refid, data = meta_d_enhancedvstandard)
summary(ma_model_2)
## 
## Multivariate Meta-Analysis Model (k = 11; method: REML)
## 
##   logLik  Deviance       AIC       BIC      AICc   
## -10.8701   21.7401   27.7401   28.6479   31.7401   
## 
## Variance Components:
## 
##             estim    sqrt  nlvls  fixed         factor 
## sigma^2.1  0.2298  0.4794     11     no        dv_name 
## sigma^2.2  0.2298  0.4794     11     no  dv_name/Refid 
## 
## Test for Heterogeneity:
## Q(df = 10) = 146.6944, p-val < .0001
## 
## Model Results:
## 
## estimate      se    zval    pval    ci.lb   ci.ub    
##   0.2863  0.2260  1.2668  0.2052  -0.1566  0.7292    
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

forest(ma_model_2, slab = paste(str_sub(meta_d_enhancedvstandard$Author, 1, 10), str_sub(meta_d_enhancedvstandard$prim_name, 1, 10), str_sub(meta_d_enhancedvstandard$dv_name, 1, 20)))
title("Effect Size: Enhanced Strategy vs. Stategy")
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-33-1.png" width="672" />

```r

funnel(ma_model_2)
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-33-2.png" width="672" />

```r

clubSandwich::coef_test(ma_model_2, vcov = "CR2") # RVE using method from Tipton and Pustejovsky (2015)
##    Coef. Estimate    SE t-stat d.f. (Satt) p-val (Satt) Sig.
##  intrcpt    0.286 0.227   1.26        9.89        0.236
```

## Comparative Effectiveness:


```r
ma_model_3 <- rma.mv(yi = dval, vi, random = ~ 1 | dv_name/Refid, data = meta_d_compeff)
summary(ma_model_3)
## 
## Multivariate Meta-Analysis Model (k = 4; method: REML)
## 
##   logLik  Deviance       AIC       BIC      AICc   
##   0.5190   -1.0381    4.9619    2.2578   28.9619   
## 
## Variance Components:
## 
##             estim    sqrt  nlvls  fixed         factor 
## sigma^2.1  0.0006  0.0243      4     no        dv_name 
## sigma^2.2  0.0006  0.0243      4     no  dv_name/Refid 
## 
## Test for Heterogeneity:
## Q(df = 3) = 2.2185, p-val = 0.5283
## 
## Model Results:
## 
## estimate      se    zval    pval   ci.lb   ci.ub    
##   0.1704  0.0681  2.5014  0.0124  0.0369  0.3039  * 
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

forest(ma_model_3, cex=0.9, header=TRUE, top=2, slab = paste(str_sub(meta_d_compeff$Author, 1, 10), str_sub(meta_d_compeff$prim_name, 1, 10), str_sub(meta_d_compeff$comparator_name, 1, 10), str_sub(meta_d_compeff$dv_name, 1, 20)))
title("Effect Size: Comparative Effectiveness")
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-34-1.png" width="672" />

```r

funnel(ma_model_3)
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-34-2.png" width="672" />

```r
 
clubSandwich::coef_test(ma_model_3, vcov = "CR2")# RVE using method from Tipton and Pustejovsky (2015)
##    Coef. Estimate    SE t-stat d.f. (Satt) p-val (Satt) Sig.
##  intrcpt     0.17 0.067   2.54        1.21        0.202
```
