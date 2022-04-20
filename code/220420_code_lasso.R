load("~/Library/CloudStorage/OneDrive-Personal/research data/project/study-ml-doc/source/220420_d0.RData")

pacman::p_load(lubridate, survival, gtsummary, magrittr, viridis, survey, jskm, survRM2, survminer, nph, haven, glmnet,
               WeightIt, MatchIt, cobalt, ggpubr, ggconsort, ggsci, gdata, tidylog, survRM2, dcurves, rms, ggeasy, tidyverse, googlesheets4, runway, ggbeeswarm, mice, zoo, ggstream, pals, tidymodels)

dum <- function(var) {case_when(is.na(var) ~ as.numeric(NA), var == "YES" ~ 1, TRUE ~ 0)}

df <- df %>% 
  select(
    id, # identifier
    LKADT_P, DEATH, # survival data
    GLEAS_DX, TSTAG_DX, AGEGRP, RACE_C, BMI, SMOKE, ECOG_C, # clinical data
    ALP, ALT, AST, CA, CREAT, HB, LDH, NEU, PLT, PSA, # laboratory data
    BONE, LYMPH_NODES, LUNGS, LIVER, SOFT_TISSUE, # metastatic data
    PROSTATECTOMY, PRIOR_RADIOTHERAPY, # prior local therapy
    ANTI_ANDROGENS, GLUCOCORTICOID, GONADOTROPIN, BISPHOSPHONATE, CORTICOSTEROID, ACE_INHIBITORS, 
    BETA_BLOCKING, HMG_COA_REDUCT, BILATERAL_ORCHIDECTOMY, # concomitant therapy
    COPD, MHCARD, MHENDO, MHHEPATO, MHIMMUNE, MHMETAB, MHNEOPLA, MHRENAL, MHRESP, MHVASC # comorbidity
  ) %>% 
  mutate( # create dummy var.
    fu = LKADT_P, 
    death = DEATH, 
    death_bin = dum(DEATH),
    gs = GLEAS_DX, 
    tst = case_when(
      TSTAG_DX == T1 ~ 1, TSTAG_DX == T2 ~ 2, TSTAG_DX == T3 ~ 3, TSTAG_DX == T4 ~ 4, 
      TRUE ~ as.character(NA)
    ),
    age = AGEGRP, 
    bmi = BMI, 
    smoke = dum(SMOKE),
    ps = case_when(is.na(ECOG_C) ~ as.numeric(NA), ECOG_C),
    alp = ALP, alt = ALT, ast = AST, ca = CA, scr = CREAT, hgb = HB, ldh = LDH, neu = NEU, plt = PLT,
    psa = PSA,
    bone = dum(BONE)
  )