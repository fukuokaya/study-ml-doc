# ASCENT data
ascent_ae <- read_sas("~/OneDrive/research data/project/mlm-doc/source/ascent_dataset/datasets/ae.sas7bdat") 
ascent_ae$AETERM <- str_to_lower(ascent_ae$AETERM)

ascent_ae %<>% 
  select(SUBID, AETERM, AETOXGR) %>% 
  mutate(
    fn = case_when(
      str_detect(AETERM, pattern = "febrile neutropenia") ~ "yes", TRUE ~ "no"
    ),
    np = case_when(
      str_detect(AETERM, pattern = "neutropenia") ~ "yes", TRUE ~ "no"
    )
  ) %>% 
  rename(id = SUBID, grade = AETOXGR) %>%
  group_by(id) %>% 
  mutate(
    grade_highest = max(as.numeric(grade))
  ) %>% 
  select(-AETERM) 

ascent_ae$id <- str_c("ASC-", ascent_ae$id, sep = "") # modify identifier

ascent_np <- ascent_ae %>% filter(np == "yes", grade_highest >= 3) %>% select(id, grade_highest) %>% distinct(id, .keep_all = TRUE) # patients developing G3/4 neutropenia
ascent_fn <- ascent_ae %>% filter(fn == "yes") %>% select(id, grade_highest) %>% distinct(id, .keep_all = TRUE)  # patients developing febrile neutropenia

# CELGENE data
celgene_ae <- read_sas("~/OneDrive/research data/project/mlm-doc/source/celgene_dataset/ae.sas7bdat") 
celgene_ae$AEDECOD <- str_to_lower(celgene_ae$AEDECOD)

celgene_ae %<>% 
  select(RPT, AEDECOD, AESEVNN) %>% 
  mutate(
    fn = case_when(
      str_detect(AEDECOD, pattern = "febrile neutropenia") ~ "yes", TRUE ~ "no"
    ),
    np = case_when(
      str_detect(AEDECOD, pattern = "neutropenia") ~ "yes", TRUE ~ "no"
    )
  ) %>% 
  rename(id = RPT, grade = AESEVNN) %>% 
  group_by(id) %>% 
  mutate(
    grade_highest = max(as.numeric(grade))
  ) %>% 
  select(-AEDECOD)

celgene_ae$id <- str_c("CELG-", celgene_ae$id, sep = "") # modify identifier

celgene_np <- celgene_ae %>% filter(np == "yes", grade_highest >= 3) %>% select(id, grade_highest) %>% distinct(id, .keep_all = TRUE) # patients developing G3/4 neutropenia
celgene_fn <- celgene_ae %>% filter(fn == "yes") %>% select(id, grade_highest) %>% distinct(id, .keep_all = TRUE)  # patients developing febrile neutropenia

# VENICE data
venice_ae <- read_sas("~/OneDrive/research data/project/mlm-doc/source/venice_dataset/Sanofi Data Venice/adae.sas7bdat") 
venice_ae$AEDECOD <- str_to_lower(venice_ae$AEDECOD)

venice_ae %<>% 
  select(SUBJID, AEDECOD, AETOXGR) %>% 
  mutate(
    fn = case_when(
      str_detect(AEDECOD, pattern = "febrile neutropenia") ~ "yes", TRUE ~ "no"
    ),
    np = case_when(
      str_detect(AEDECOD, pattern = "neutropenia") ~ "yes", TRUE ~ "no"
    )
  ) %>% 
  rename(id = SUBJID, grade = AETOXGR) %>% 
  group_by(id) %>% 
  mutate(
    grade_highest = max(as.numeric(grade))
  ) %>% 
  select(-AEDECOD)

venice_ae$id <- str_c("VEN-", venice_ae$id, sep = "") # modify identifier

venice_np <- venice_ae %>% filter(np == "yes", grade_highest >= 3) %>% select(id, grade_highest) %>% distinct(id, .keep_all = TRUE) # patients developing G3/4 neutropenia
venice_fn <- venice_ae %>% filter(fn == "yes") %>% select(id, grade_highest) %>% distinct(id, .keep_all = TRUE)  # patients developing febrile neutropenia

# DREAM challenge dataset
# data processing
d <- readxl::read_excel("~/OneDrive/research data/project/mlm-doc/source/dream_challenge_dataset/prostate_cancer_challenge_data_training/coretable_training.xlsx") # import data from excel

d_np <- bind_rows(ascent_np, celgene_np, venice_np) %>% mutate(NP = "YES") %>% select(-grade_highest)
d_fn <- bind_rows(ascent_fn, celgene_fn, venice_fn) %>% mutate(FN = "YES") %>% select(-grade_highest)

df <- d %>% 
  left_join(., d_fn, by = "id") %>% 
  left_join(., d_np, by = "id") 
df$FN <- df$FN %>% replace_na("NO")
df$NP <- df$NP %>% replace_na("NO")

df <- df %>% 
  select(GLEAS_DX:AGEGRP, RACE_C:MHVASC, NP) %>% 
  mutate_if(is.character, factor) 

set.seed(123)
df_split <- initial_split(df, strata = NP, prop = 3/4)
df_train <- training(df_split)
df_test <- testing(df_split)

xgb_spec <- 
  boost_tree(trees = 1000, 
             tree_depth = tune(), 
             min_n = tune(), 
             loss_reduction = tune(),       
             sample_size = tune(), 
             mtry = tune(),  
             learn_rate = tune(),           
  ) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") 
xgb_spec

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), df_train),
  learn_rate(),
  size = 30
)
xgb_grid

xgb_wf <- workflow() %>%
  add_formula(NP ~ .) %>%
  add_model(xgb_spec)

xgb_wf

set.seed(123)
df_folds <- vfold_cv(df_train, strata = NP)

df_folds

doParallel::registerDoParallel()

set.seed(234)
xgb_res <- tune_grid(
  xgb_wf,
  resamples = df_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

xgb_res

collect_metrics(xgb_res)

xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC") 

show_best(xgb_res, "roc_auc")

best_auc <- select_best(xgb_res, "roc_auc")
best_auc

final_xgb <- finalize_workflow(
  xgb_wf,
  best_auc
)

final_xgb

library(vip)

final_xgb %>%
  fit(data = df_train) %>%
  extract_fit_parsnip() %>%
  vip(geom = "point")

final_res <- last_fit(final_xgb, df_split)

collect_metrics(final_res)

final_res %>%
  collect_predictions() %>%
  roc_curve(NP, .pred_NO) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(size = 1, color = "midnightblue") +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2
  )