############################################################
# 0. PACKAGES
############################################################


library(tidyverse)
library(janitor)
library(stringr)

library(psych)
library(skimr)
library(naniar)

library(pROC)
library(randomForest)
library(dbarts)

library(broom)
library(pscl)
library(car)

library(knitr)
library(kableExtra)
library(tibble)

############################################################
# 1. IMPORT & BASIC CLEANING
############################################################

raw <- read_csv("C:/Users/shiva/Downloads/thesisdat.csv", skip = 2)
raw <- read_csv(
  "C:/Users/shiva/Downloads/thesisdat.csv",
  show_col_types = FALSE
)

raw_clean <- raw %>%
  slice(-1, -2)
df <- raw_clean %>%
  clean_names()
names(df)
if ("progress" %in% names(df)) {
  df <- df %>%
    filter(progress == 100)
}
meta_cols <- c(
  "startdate", "enddate", "status", "ipaddress", "progress",
  "duration_in_seconds", "finished", "recordeddate", "recipientlastname",
  "recipientfirstname", "recipientemail", "externalreference",
  "locationlatitude", "locationlongitude", "distributionchannel",
  "user_language", "participantid", "assignmentid", "projectid"
)
df <- df %>% select(-any_of(meta_cols))
df <- df %>%
  mutate(across(
    where(~ all(grepl("^[0-9]+$", .x) | is.na(.x))),
    as.numeric
  ))
glimpse(df)

############################################################
# 2. SCALE CONSTRUCTION
############################################################

# ---------- Helper functions ----------
rev_1_5 <- function(x) ifelse(is.na(x), NA, 6 - x)
rev_1_6 <- function(x) ifelse(is.na(x), NA, 7 - x)

# ---------------------------------------------------------
# 2.1 ORGANIZATIONAL COMMITMENT (KUT)
# ---------------------------------------------------------

kut_map <- c(
  "Not at all"   = 1,
  "Slightly"     = 2,
  "Moderately"   = 3,
  "Quite a bit"  = 4,
  "Extremely"    = 5
)

oc_items <- c("q22_1", "q22_2", "q22_3", "q22_4")

df <- df %>%
  mutate(
    across(all_of(oc_items),
           ~ as.numeric(unname(kut_map[.x])))
  ) %>%
  mutate(
    oc_kut = rowMeans(select(., all_of(oc_items)), na.rm = TRUE)
  )

# ---------------------------------------------------------
# 2.2 JOB SATISFACTION (SIJS)
# ---------------------------------------------------------

sijs_map <- c(
  "Strongly disagree" = 1,
  "Disagree"          = 2,
  "Undecided"         = 3,
  "Agree"             = 4,
  "Strongly agree"    = 5
)

js_pos <- c("q23_1", "q23_2", "q23_4")
js_neg <- c("q23_3", "q23_5")
js_items <- c(js_pos, js_neg)

df <- df %>%
  mutate(
    across(all_of(js_items),
           ~ as.numeric(unname(sijs_map[.x])))
  ) %>%
  mutate(
    across(all_of(js_neg),
           rev_1_5,
           .names = "{.col}_r"),
    job_satisfaction = rowMeans(
      select(., all_of(js_pos), ends_with("_r")),
      na.rm = TRUE
    )
  )

# ---------------------------------------------------------
# 2.3 WORK ENGAGEMENT (UWES-17)
# ---------------------------------------------------------

uwes_map <- c(
  "Never"        = 0,
  "Almost never" = 1,
  "Rarely"       = 2,
  "Sometimes"    = 3,
  "Often"        = 4,
  "Very Often"   = 5,
  "Very often"   = 5,  # small capitalization variants
  "Always"       = 6
)

we_items            <- paste0("q25_", 1:17)
we_vigor_items      <- paste0("q25_", c(1, 2, 5, 8, 12, 15))
we_dedication_items <- paste0("q25_", c(3, 7, 9, 13, 16))
we_absorption_items <- paste0("q25_", c(4, 6, 10, 11, 14, 17))

df <- df %>%
  mutate(
    across(all_of(we_items),
           ~ as.numeric(unname(uwes_map[.x])))
  ) %>%
  mutate(
    we_total      = rowMeans(select(., all_of(we_items)),            na.rm = TRUE),
    we_vigor      = rowMeans(select(., all_of(we_vigor_items)),      na.rm = TRUE),
    we_dedication = rowMeans(select(., all_of(we_dedication_items)), na.rm = TRUE),
    we_absorption = rowMeans(select(., all_of(we_absorption_items)), na.rm = TRUE)
  )

# ---------------------------------------------------------
# 2.4 JOB EMBEDDEDNESS
# ---------------------------------------------------------

je_map <- c(
  "Strongly disagree"            = 1,
  "Disagree"                     = 2,
  "Neither agree nor disagree"   = 3,
  "Agree"                        = 4,
  "Strongly agree"               = 5
)

je_items <- paste0("q26_", 1:12)

df <- df %>%
  mutate(
    across(all_of(je_items),
           ~ as.numeric(unname(je_map[.x])))
  ) %>%
  mutate(
    job_embeddedness = rowMeans(select(., all_of(je_items)), na.rm = TRUE)
  )

# ---------------------------------------------------------
# 2.5 PERCEIVED ORGANIZATIONAL SUPPORT (POS)
# ---------------------------------------------------------

# POS --------------------------------------------------------------

pos_map <- c(
  "Strongly disagree"   = 1,
  "Moderately disagree" = 2,
  "Slightly disagree"   = 3,
  "Slightly agree"      = 4,
  "Moderately agree"    = 5,
  "Strongly agree"      = 6
)

rev_1_6 <- function(x) ifelse(is.na(x), NA, 7 - x)

pos_items <- paste0("q27_", 1:6)

# 1) text -> numeric for all POS items
df <- df %>%
  mutate(
    across(
      all_of(pos_items),
      ~ as.numeric(unname(pos_map[.x]))
    )
  )

# 2) create the reversed item
df <- df %>%
  mutate(
    q27_4_r = rev_1_6(q27_4)
  )

# 3) compute POS mean
df <- df %>%
  mutate(
    pos_mean = rowMeans(
      select(., q27_1, q27_2, q27_3, q27_5, q27_6, q27_4_r),
      na.rm = TRUE
    )
  )

# ---------------------------------------------------------
# 2.6 PERCEPTIONS OF ORGANIZATIONAL JUSTICE (POJ)
# ---------------------------------------------------------

poj_map <- c(
  "Strongly disagree"            = 1,
  "Disagree"                     = 2,
  "Neither agree nor disagree"   = 3,
  "Agree"                        = 4,
  "Strongly agree"               = 5
)

poj_items <- paste0("q28_", 1:20)

oj_dist_items  <- paste0("q28_", 1:4)
oj_proc_items  <- paste0("q28_", 5:11)
oj_inter_items <- paste0("q28_", 12:15)
oj_info_items  <- paste0("q28_", 16:20)

df <- df %>%
  mutate(
    across(all_of(poj_items),
           ~ as.numeric(unname(poj_map[.x])))
  ) %>%
  mutate(
    oj_distributive  = rowMeans(select(., all_of(oj_dist_items)),  na.rm = TRUE),
    oj_procedural    = rowMeans(select(., all_of(oj_proc_items)),  na.rm = TRUE),
    oj_interpersonal = rowMeans(select(., all_of(oj_inter_items)), na.rm = TRUE),
    oj_informational = rowMeans(select(., all_of(oj_info_items)),  na.rm = TRUE),
    oj_total         = rowMeans(select(., all_of(poj_items)),      na.rm = TRUE)
  )

# ---------------------------------------------------------
# 2.7 WORK ENVIRONMENT
# ---------------------------------------------------------

extent_map <- c(
  "Not at all"             = 1,
  "To a small extent"      = 2,
  "To some extent"         = 3,
  "To a large extent"      = 4,
  "To a very large extent" = 5
)

freq_map <- c(
  "Never"          = 1,
  "Rarely"         = 2,
  "Occasionally"   = 3,
  "Occassionally"  = 3,
  "Often"          = 4,
  "Very often"     = 5,
  "Very Often"     = 5
)

qty_map <- c(
  "far too few"   = 1,
  "too few"       = 2,
  "sufficient"    = 3,
  "too many"      = 4,
  "far too many"  = 5,
  "Far too few"   = 1,
  "Too few"       = 2,
  "Sufficient"    = 3,
  "Too many"      = 4,
  "Far too many"  = 5
)

we_ext_items  <- paste0("q35_", 1:5)
we_freq_items <- paste0("q36_", 1:4)
we_qty_item   <- "q37_1"

df <- df %>%
  mutate(
    across(all_of(we_ext_items),
           ~ as.numeric(unname(extent_map[.x]))),
    across(all_of(we_freq_items),
           ~ as.numeric(unname(freq_map[.x]))),
    q37_1 = as.numeric(unname(qty_map[q37_1]))
  ) %>%
  mutate(
    work_environment = rowMeans(
      select(., all_of(c(we_ext_items, we_freq_items, we_qty_item))),
      na.rm = TRUE
    )
  )

# ---------------------------------------------------------
# 2.8 ORGANIZATIONAL CAREER GROWTH
# ---------------------------------------------------------

ocg_items <- paste0("q29_", 1:15)

ocg_map <- c(
  "Strongly disagree"            = 1,
  "Disagree"                     = 2,
  "Neither agree nor disagree"   = 3,
  "Agree"                        = 4,
  "Strongly agree"               = 5
)

df <- df %>%
  mutate(
    across(all_of(ocg_items),
           ~ as.numeric(unname(ocg_map[.x])))
  ) %>%
  mutate(
    org_career_growth = rowMeans(select(., all_of(ocg_items)), na.rm = TRUE)
  )

# ---------------------------------------------------------
# 2.9 WORK–LIFE BALANCE
# ---------------------------------------------------------

wlb_map <- c(
  "Strongly disagree"            = 1,
  "Disagree"                     = 2,
  "Neither agree nor disagree"   = 3,
  "Agree"                        = 4,
  "Strongly agree"               = 5
)

wlb_items         <- paste0("q30_", 1:4)
reverse_wlb_item  <- "q30_2"

df <- df %>%
  mutate(
    across(all_of(wlb_items),
           ~ as.numeric(unname(wlb_map[.x])))
  ) %>%
  mutate(
    across(all_of(reverse_wlb_item),
           rev_1_5,
           .names = "{.col}_r")
  ) %>%
  mutate(
    work_life_balance = rowMeans(
      select(
        .,
        all_of(setdiff(wlb_items, reverse_wlb_item)),
        ends_with("_r")
      ),
      na.rm = TRUE
    )
  )

# ---------------------------------------------------------
# 2.10 EMPLOYEE NET PROMOTER SCORE (eNPS)
# ---------------------------------------------------------

df <- df %>%
  mutate(
    enps_raw = as.numeric(str_extract(q33_1, "^[0-9]+"))
  ) %>%
  mutate(
    enps_group = case_when(
      is.na(enps_raw)                ~ NA_character_,
      enps_raw <= 6                  ~ "Detractor",
      enps_raw >= 7 & enps_raw <= 8  ~ "Passive",
      enps_raw >= 9 & enps_raw <= 10 ~ "Promoter",
      TRUE                           ~ NA_character_
    )
  )

enps_summary <- df %>%
  summarise(
    n_valid   = sum(!is.na(enps_raw)),
    prop_det  = mean(enps_raw <= 6, na.rm = TRUE),
    prop_prom = mean(enps_raw >= 9 & enps_raw <= 10, na.rm = TRUE),
    eNPS      = 100 * (prop_prom - prop_det)
  )

# ---------------------------------------------------------
# 2.11 TURNOVER INTENTIONS (MEAN SCALE + BINARY)
# ---------------------------------------------------------
grep("^q34", names(df), value = TRUE)

## Mapping: text → 1–5
ti_map <- c(
  "Never"     = 1,
  "Rarely"    = 2,
  "Sometimes" = 3,
  "Often"     = 4,
  "Always"    = 5
)

ti_items <- paste0("q34_", 1:6)

# 1) Recode each q34 item individually
df[ti_items] <- lapply(df[ti_items], function(x) {
  as.numeric(unname(ti_map[x]))
})

# 2) Reverse-code q34_6
df$q34_6_r <- 6 - df$q34_6

# 3) Compute the mean TI score
df$turnover_intentions <- rowMeans(
  df[, c("q34_1", "q34_2", "q34_3", "q34_4", "q34_5", "q34_6_r")],
  na.rm = TRUE
)

# 4) Verify that it now exists
grep("turnover_intentions", names(df), value = TRUE)
summary(df$turnover_intentions)

# 75th percentile cutoff
ti_cut <- quantile(df$turnover_intentions, 0.75, na.rm = TRUE)

# Binary label: 1 = high TI, 0 = low TI
df$ti_binary <- ifelse(df$turnover_intentions >= ti_cut, 1, 0)

# Check
table(df$ti_binary)
grep("ti_binary", names(df), value = TRUE)

############################################################
# 3. FINAL CLEAN DATASETS (df_clean, df_final, analytic_df)
############################################################

# Second meta removal (post-scale)
meta_cols_clean <- c(
  "startdate", "enddate", "status", "ipaddress",
  "recipient_last_name", "recipient_first_name", "recipient_email",
  "external_reference", "location_latitude", "location_longitude",
  "distribution_channel", "q_recaptcha_score"
)

df_clean <- df %>%
  select(-any_of(meta_cols_clean)) %>%
  select(where(~ !all(is.na(.x))))   # remove all-NA columns

scale_scores <- c(
  "oc_kut",
  "job_satisfaction",
  "we_total", "we_vigor", "we_dedication", "we_absorption",
  "job_embeddedness",
  "pos_mean",
  "oj_distributive", "oj_procedural", "oj_interpersonal", "oj_informational",
  "oj_total",
  "work_environment",
  "org_career_growth",
  "work_life_balance",
  "turnover_intentions",
  "enps_raw"
)
names(df_final)[str_detect(names(df_final), "q")]

df_final <- df_clean %>%
  select(
    starts_with("q1"), starts_with("q2"), starts_with("q3"), starts_with("q6"), # All demographic questions (Q6, Q7–Q11, Q17–Q21)
    starts_with("q6"), 
    starts_with("q7"),
    starts_with("q8"),
    starts_with("q9"),
    starts_with("q10"),
    starts_with("q11"),
    starts_with("q17"),
    starts_with("q18"),
    starts_with("q19"),
    starts_with("q20"),
    starts_with("q21"),
    all_of(scale_scores),
    ti_binary
  )

# Analytic dataset for descriptives, correlations, and logit
analytic_df <- df_final %>%
  select(
    turnover_intentions, ti_binary,
    oc_kut,
    job_satisfaction,
    we_total,
    job_embeddedness,
    pos_mean,
    oj_total,
    work_environment,
    org_career_growth,
    work_life_balance,
    enps_raw,
    q6, q7, q8, q9, q10, q11,  # job characteristics
    q17, q18, q19, q20, q21    # tenure, age, gender, education, ethnicity
  )

############################################################
# 4. DESCRIPTIVE STATISTICS, FREQUENCIES, CORRELATIONS
############################################################

# 4.1 Continuous descriptives
continuous_vars <- analytic_df %>% select(where(is.numeric))
describe_continuous <- psych::describe(continuous_vars)
describe_continuous

# 4.2 Frequencies for categorical variables
categorical_vars <- analytic_df %>% select(where(is.character), where(is.factor))

freq_tables <- categorical_vars %>%
  map(~ janitor::tabyl(.x) %>% janitor::adorn_pct_formatting())
freq_tables

# 4.3 Correlation matrix (continuous vars)
cor_matrix <- cor(continuous_vars, use = "pairwise.complete.obs")
cor_matrix

# 4.4 Skim summary (for appendix)
skimmed <- skimr::skim(analytic_df)
skimmed
print(skimmed)

# 4.5 Demographic-style tables with totals
table1_demographics <- categorical_vars %>%
  map(~ janitor::tabyl(.x) %>% janitor::adorn_totals(c("row")))
table1_demographics

# 4.6 Missingness summary
missing_summary <- naniar::miss_var_summary(analytic_df)
missing_summary


# 4.7 APA-style correlation table for thesis
corr_vars <- analytic_df %>%
  select(
    turnover_intentions,
    oc_kut,
    job_satisfaction,
    we_total,
    job_embeddedness,
    pos_mean,
    oj_total,
    work_environment,
    org_career_growth,
    work_life_balance,
    enps_raw
  )

corr_out <- psych::corr.test(
  corr_vars,
  use    = "pairwise",
  method = "pearson",
  adjust = "none"
)

r_mat <- round(corr_out$r, 2)
p_mat <- corr_out$p

apa_mat <- matrix(
  as.character(r_mat),
  nrow = nrow(r_mat),
  ncol = ncol(r_mat),
  dimnames = dimnames(r_mat)
)

apa_mat[p_mat < .05  & p_mat >= .01]   <- paste0(apa_mat[p_mat < .05  & p_mat >= .01], "*")
apa_mat[p_mat < .01 & p_mat >= .001]  <- paste0(apa_mat[p_mat < .01 & p_mat >= .001], "**")
apa_mat[p_mat < .001]                 <- paste0(apa_mat[p_mat < .001], "***")

var_labels <- c(
  "Turnover intentions",
  "Organizational Commitment",
  "Job satisfaction",
  "Work engagement (UWES total)",
  "Job embeddedness",
  "Perceived Organizational Support",
  "Overall justice",
  "Work environment",
  "Organizational career growth",
  "Work–life balance",
  "eNPS"
)

rownames(apa_mat) <- var_labels
colnames(apa_mat) <- var_labels

kable(apa_mat,
      caption = "Table Z\nZero-order correlations among study variables.",
      align = "c") %>%
  kable_styling(full_width = FALSE)

############################################################
# 5. LOGISTIC REGRESSION MODEL (FULL SAMPLE)
############################################################

logit_dat <- analytic_df %>%
  select(
    ti_binary,
    oc_kut,
    job_satisfaction,
    we_total,
    job_embeddedness,
    pos_mean,
    oj_total,
    work_environment,
    org_career_growth,
    work_life_balance,
    enps_raw
  ) %>%
  filter(!is.na(ti_binary))

logit_dat_scaled <- logit_dat %>%
  mutate(across(-ti_binary, scale))

logit_full <- glm(
  ti_binary ~ .,
  data   = logit_dat_scaled,
  family = binomial(link = "logit")
)

summary(logit_full)

logit_table <- broom::tidy(
  logit_full,
  conf.int = TRUE,
  conf.level = 0.95,
  exponentiate = TRUE
)

logit_table_pretty <- logit_table %>%
  mutate(
    term = recode(
      term,
      "(Intercept)"           = "Intercept",
      "oc_kut"                = "Organizational Commitment",
      "job_satisfaction"      = "Job satisfaction",
      "we_total"              = "Work engagement (UWES total)",
      "job_embeddedness"      = "Job embeddedness",
      "pos_mean"              = "Perceived Organizational Support",
      "oj_total"              = "Overall justice",
      "work_environment"      = "Work environment",
      "org_career_growth"     = "Organizational career growth",
      "work_life_balance"     = "Work–life balance",
      "enps_raw"              = "eNPS"
    )
  )

logit_table_pretty
write.csv(logit_table_pretty,
          "logistic_regression_results.csv",
          row.names = FALSE)

# Pseudo-R² and LR test
pscl::pR2(logit_full)
anova(logit_full, test = "Chisq")

# Multicollinearity diagnostics
car::vif(logit_full)

############################################################
# 6. PREDICTIVE MODELS: LOGIT, RF, BART, H2O AutoML
############################################################

# --- 6.1 Modeling dataset (no leakage) ---

model_dat <- df_final %>%
  select(
    ti_binary,
    oc_kut,
    job_satisfaction,
    we_total,
    job_embeddedness,
    pos_mean,
    oj_total,
    work_environment,
    org_career_growth,
    work_life_balance,
    enps_raw
  ) %>%
  filter(!is.na(ti_binary))

y <- model_dat$ti_binary
x <- model_dat %>% select(-ti_binary)

# --- 6.2 Train / Test split ---

set.seed(123)
n <- nrow(model_dat)
train_index <- sample(seq_len(n), size = floor(0.7 * n))

x_train <- x[train_index, ]
x_test  <- x[-train_index, ]
y_train <- y[train_index]
y_test  <- y[-train_index]

# --- 6.3 Mean imputation for predictors (train means → test) ---

x_train <- x_train %>%
  mutate(across(everything(),
                ~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)))

train_means <- sapply(x_train, mean, na.rm = TRUE)

x_test <- x_test %>%
  mutate(across(everything(),
                ~ ifelse(is.na(.x),
                         train_means[cur_column()],
                         .x)))

# Sanity checks
colSums(is.na(x_train))
colSums(is.na(x_test))
sum(is.na(y_train)); sum(is.na(y_test))

############################################################
# 6.4 LOGISTIC REGRESSION (PREDICTION)
############################################################

logit_data_train <- cbind(y_train = y_train, x_train)

logit_model <- glm(
  y_train ~ .,
  data   = as.data.frame(logit_data_train),
  family = binomial
)

logit_pred_prob <- predict(
  logit_model,
  newdata = as.data.frame(x_test),
  type    = "response"
)

auc_logit   <- auc(y_test, logit_pred_prob)
brier_logit <- mean((logit_pred_prob - y_test)^2)
rmse_logit  <- sqrt(brier_logit)
acc_logit   <- mean(ifelse(logit_pred_prob > 0.5, 1, 0) == y_test)

auc_logit
brier_logit
rmse_logit
acc_logit

############################################################
# 6.5 RANDOM FOREST
############################################################

rf_model <- randomForest(
  x = x_train,
  y = factor(y_train),
  ntree = 500
)

rf_pred_prob <- predict(rf_model, x_test, type = "prob")[, 2]

auc_rf   <- auc(y_test, rf_pred_prob)
brier_rf <- mean((rf_pred_prob - y_test)^2)
rmse_rf  <- sqrt(brier_rf)
acc_rf   <- mean(ifelse(rf_pred_prob > 0.5, 1, 0) == y_test)

auc_rf
brier_rf
rmse_rf
acc_rf

############################################################
# 6.6 BART CLASSIFIER (dbarts)
############################################################

set.seed(123)
bart_model <- bart(
  x.train   = as.matrix(x_train),
  y.train   = factor(y_train),
  keeptrees = TRUE,
  verbose   = TRUE
)

bart_draws <- predict(bart_model, newdata = as.matrix(x_test))
bart_pred_prob <- colMeans(bart_draws)

auc_bart   <- auc(y_test, bart_pred_prob)
brier_bart <- mean((bart_pred_prob - y_test)^2)
rmse_bart  <- sqrt(brier_bart)
acc_bart   <- mean(ifelse(bart_pred_prob > 0.5, 1, 0) == y_test)

auc_bart
brier_bart
rmse_bart
acc_bart
############################################################
# 6.7 (OPTIONAL) H2O AutoML – requires Java & h2o set up
############################################################

# Uncomment this block only if you already have Java + h2o working.
# ---------------------------------------------------------------
# 1. Point JAVA_HOME to your Temurin JDK (folder *above* bin)
Sys.setenv(JAVA_HOME = "C:/Program Files/Eclipse Adoptium/jdk-17.0.17.10-hotspot")

# 2. Put the JDK bin folder at the front of PATH inside R
Sys.setenv(PATH = paste(
  "C:/Program Files/Eclipse Adoptium/jdk-17.0.17.10-hotspot/bin",
  Sys.getenv("PATH"),
  sep = ";"
))

# 3. Check that R can see the right java
system("java -version")

install.packages("rJava")
library(rJava)

h2o.shutdown(prompt = FALSE)


library(h2o)
h2o.init()
#
train_dat <- data.frame(ti_binary = y_train, x_train)
test_dat  <- data.frame(ti_binary = y_test,  x_test)

train_dat <- train_dat %>%
   mutate(
     ti_binary_num = ti_binary,
     ti_binary = factor(ti_binary, levels = c(0, 1))
   )

 test_dat <- test_dat %>%
   mutate(
     ti_binary_num = ti_binary,
     ti_binary = factor(ti_binary, levels = c(0, 1))
   )

 train_h2o <- as.h2o(train_dat)
 test_h2o  <- as.h2o(test_dat)

 x_cols <- setdiff(names(train_h2o), c("ti_binary", "ti_binary_num"))
 y_col  <- "ti_binary"

 aml <- h2o.automl(
   x = x_cols,
   y = y_col,
   training_frame    = train_h2o,
   leaderboard_frame = test_h2o,
   max_runtime_secs  = 300,
   seed              = 123,
   sort_metric       = "AUC"
 )
 
 lb <- aml@leaderboard
 lb

 best_model <- aml@leader
 perf_aml   <- h2o.performance(best_model, newdata = test_h2o)

 auc_aml   <- h2o.auc(perf_aml)
 pred_aml  <- h2o.predict(best_model, test_h2o)
 p1        <- as.vector(pred_aml[ , "p1"])

 y_true    <- test_dat$ti_binary_num
 brier_aml <- mean((p1 - y_true)^2)
 rmse_aml  <- sqrt(brier_aml)
 acc_aml   <- mean(ifelse(p1 > 0.5, 1, 0) == y_true)
# ---------------------------------------------------------------

############################################################
# 6.8 MODEL COMPARISON TABLE (WITHOUT / WITH H2O)
############################################################


# If you did run H2O, instead use:
model_compare <- tibble(
   model    = c("Logistic regression", "Random forest", "BART", "H2O AutoML"),
   AUC      = c(as.numeric(auc_logit), auc_rf, auc_bart, auc_aml),
   Brier    = c(brier_logit, brier_rf, brier_bart, brier_aml),
   RMSE     = c(rmse_logit, rmse_rf, rmse_bart, rmse_aml),
   Accuracy = c(acc_logit, acc_rf, acc_bart, acc_aml)
 )

model_compare

############################################################
# 6.9 THRESHOLD SENSITIVITY TABLE
############################################################

threshold_metrics <- function(pred_prob, y_true, threshold = 0.5) {
  pred_class <- ifelse(pred_prob > threshold, 1, 0)
  acc  <- mean(pred_class == y_true)
  brier <- mean((pred_prob - y_true)^2)
  rmse  <- sqrt(brier)
  auc_val <- as.numeric(auc(y_true, pred_prob))
  
  tibble(
    threshold = threshold,
    accuracy  = acc,
    Brier     = brier,
    RMSE      = rmse,
    AUC       = auc_val
  )
}

thr_values <- c(0.40, 0.50, 0.60)

sens_logit <- bind_rows(lapply(thr_values, function(t) {
  threshold_metrics(logit_pred_prob, y_test, threshold = t) %>%
    mutate(model = "Logistic regression")
}))

sens_rf <- bind_rows(lapply(thr_values, function(t) {
  threshold_metrics(rf_pred_prob, y_test, threshold = t) %>%
    mutate(model = "Random forest")
}))

sens_bart <- bind_rows(lapply(thr_values, function(t) {
  threshold_metrics(bart_pred_prob, y_test, threshold = t) %>%
    mutate(model = "BART")
}))

# If H2O run, add sens_aml as well; otherwise skip
 sens_aml <- bind_rows(lapply(thr_values, function(t) {
   threshold_metrics(p1, y_test, threshold = t) %>%
     mutate(model = "H2O AutoML")
 }))

threshold_sensitivity <- bind_rows(
  sens_logit,
  sens_rf,
  sens_bart,
  sens_aml
  # , sens_aml
) %>%
  select(model, threshold, AUC, Brier, RMSE, accuracy)

threshold_sensitivity

#############################################################
# TABLES
#############################################################

# Descriptive Statistics Table

# Install if needed:
# install.packages(c("dplyr", "psych", "knitr", "kableExtra"))

library(dplyr)
library(psych)
library(knitr)
library(kableExtra)

library(dplyr)
library(psych)
library(knitr)
library(kableExtra)

# Your variables (confirm names match df_final)
vars <- c(
  "turnover_intentions",
  "oc_kut",
  "job_satisfaction",
  "we_total",
  "job_embeddedness",
  "pos_mean",
  "oj_total",
  "work_environment",
  "org_career_growth",
  "work_life_balance",
  "enps_raw"
)

# 1. Compute descriptive stats
desc_raw <- df_final %>%
  select(all_of(vars)) %>%
  psych::describe()

# 2. Convert to data frame
desc_tbl <- as.data.frame(desc_raw)

# 3. Fix column naming issues depending on psych::describe() version
# Standard psych output uses "n", "mean", "sd", "min", "max"
desc_tbl <- desc_tbl %>%
  select(
    N = n,
    M = mean,
    SD = sd,
    Min = min,
    Max = max
  )

# 4. Add readable variable names (optional but APA-friendly)
variable_labels <- c(
  "Turnover intentions",
  "Organizational commitment",
  "Job satisfaction",
  "Work engagement",
  "Job embeddedness",
  "Perceived organizational support",
  "Organizational justice",
  "Work environment",
  "Career growth opportunities",
  "Work–life balance",
  "Employee Net Promoter Score (eNPS)"
)

desc_tbl <- desc_tbl %>%
  mutate(Variable = variable_labels) %>%
  select(Variable, N, M, SD, Min, Max)

# Round values
desc_tbl <- desc_tbl %>% mutate(across(where(is.numeric), round, 2))


kable(
  desc_tbl,
  caption = "Table 1. Descriptive Statistics for Continuous Study Variables",
  booktabs = TRUE,
  align = "lccccc"
) %>%
  kable_styling(full_width = FALSE, position = "center")

# Zero-Order Pearson correlations Table

library(dplyr)
library(psych)
library(knitr)
library(kableExtra)

# 1. List the variables you want in the correlation table
vars <- c(
  "turnover_intentions",
  "oc_kut",
  "job_satisfaction",
  "we_total",
  "job_embeddedness",
  "pos_mean",
  "oj_total",
  "work_environment",
  "org_career_growth",
  "work_life_balance",
  "enps_raw"
)

# 2. Optional: nice labels for the rows/columns
var_labels <- c(
  "Turnover intentions",
  "Organizational commitment",
  "Job satisfaction",
  "Work engagement",
  "Job embeddedness",
  "Perceived organizational support",
  "Organizational justice",
  "Work environment",
  "Career growth opportunities",
  "Work–life balance",
  "Employee Net Promoter Score (eNPS)"
)

# 3. Compute Pearson correlations and p-values
corr_obj <- psych::corr.test(
  df_final %>% select(all_of(vars)),
  use   = "pairwise",
  method = "pearson",
  adjust = "none"
)

# Extract r and p matrices
r_mat <- corr_obj$r
p_mat <- corr_obj$p

# Round correlations
r_rounded <- round(r_mat, 2)

# Significance stars
star_fun <- function(p) {
  ifelse(p < .001, "***",
         ifelse(p < .01,  "**",
                ifelse(p < .05,  "*", "")))
}

stars <- star_fun(p_mat)

# Convert r to character and clean -0.00
r_char <- sprintf("%.2f", r_rounded)
r_char[r_char == "-0.00"] <- "0.00"

# 🔧 IMPORTANT: rebuild as a matrix, not a vector
corr_display <- matrix(
  paste0(r_char, stars),
  nrow = nrow(r_mat),
  ncol = ncol(r_mat)
)

# Set row/col names now (before we blank upper triangle)
rownames(corr_display) <- var_labels
colnames(corr_display) <- var_labels

# Diagonal = em dash
diag(corr_display) <- "—"

# Show only lower triangle
corr_display[upper.tri(corr_display)] <- ""

# Turn into data frame for kable
corr_df <- as.data.frame(corr_display)

kable(
  corr_df,
  caption = "Table 2\nZero-order Pearson correlations among key study variables.",
  booktabs = TRUE,
  align = "lccccccccccc"
) %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  footnote(
    general = "Note. Values below the diagonal are Pearson correlations. * p < .05, ** p < .01, *** p < .001.",
    general_title = ""
  )

# LOGISTIC REGRESSION RESULTS

# Install once if needed:
# install.packages(c("broom", "pscl", "dplyr", "kableExtra"))

library(broom)
library(pscl)
library(dplyr)
library(kableExtra)
library(knitr)

# Your fitted logistic regression model
model <- logit_full   # <-- CHANGE THIS to your glm object name if different

# Tidy coefficients (remove intercept)
coef_tbl <- broom::tidy(model) %>%
  filter(term != "(Intercept)")

# Wald confidence intervals (faster & stable)
ci_tbl <- as.data.frame(confint.default(model))
ci_tbl$term <- rownames(ci_tbl)

# Join and compute OR + CI on OR scale
logit_table <- coef_tbl %>%
  left_join(ci_tbl, by = "term") %>%
  mutate(
    OR      = exp(estimate),
    CI_low  = exp(`2.5 %`),
    CI_high = exp(`97.5 %`),
    p       = p.value
  ) %>%
  select(
    term,
    B  = estimate,
    SE = std.error,
    OR,
    CI_low,
    CI_high,
    p
  )

# Look at current names
unique(logit_table$term)

# Manually map them to readable labels
pretty_labels <- c(
  "oc_kut"            = "Organizational commitment",
  "job_satisfaction"  = "Job satisfaction",
  "we_total"          = "Work engagement",
  "job_embeddedness"  = "Job embeddedness",
  "pos_mean"          = "Perceived organizational support",
  "oj_total"          = "Organizational justice",
  "work_environment"  = "Work environment",
  "org_career_growth" = "Career growth opportunities",
  "work_life_balance" = "Work–life balance",
  "enps_raw"          = "Employee Net Promoter Score (eNPS)"
)

logit_table <- logit_table %>%
  mutate(
    Predictor = recode(term, !!!pretty_labels)
  ) %>%
  select(Predictor, B, SE, OR, CI_low, CI_high, p)

logit_table <- logit_table %>%
  mutate(
    across(c(B, SE, OR, CI_low, CI_high), ~ round(.x, 2)),
    p = ifelse(p < .001, "< .001", sprintf("%.3f", round(p, 3)))
  )

model_aic <- AIC(model)

# Nagelkerke R² from pscl::pR2
nag <- pscl::pR2(model)["Nagelkerke"]
nag <- as.numeric(nag)

kable(
  logit_table,
  caption = "Table 3\nLogistic regression predicting high turnover intention.",
  booktabs = TRUE,
  align = "lcccccc",
  col.names = c("Predictor", "B", "SE", "OR", "95% CI (LL)", "95% CI (UL)", "p")
) %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  footnote(
    general = paste0(
      "Note. OR = odds ratio. LL = lower limit; UL = upper limit of 95% confidence interval. ",
      "Model fit: AIC = ", round(model_aic, 2),
      ", Nagelkerke R\u00B2 = ", round(nag, 2), "."
    ),
    general_title = ""
  )

# MODEL PERFORMANCE COMPARISON

library(dplyr)
library(pROC)
library(kableExtra)
library(knitr)

# If h2o is still running and pred_aml is an H2OFrame:
library(h2o)

# If your cluster is off, you may need:
# h2o.init()

# Convert AutoML predictions to a plain numeric vector
pred_aml_vec <- as.vector(as.data.frame(pred_aml)[, 1])


## 1. True labels from your test set
truth <- test_dat$ti_binary_num   # numeric 0/1

## 2. Predicted probabilities from each model  ----
##    (these names are from your screenshot)
pred_df <- data.frame(
  Logistic_Regression = logit_pred_prob,
  Random_Forest       = rf_pred_prob,
  BART                = bart_pred_prob,
  AutoML_Best         = pred_aml
)

# Optional sanity check: all lengths should be 72
nrow(test_dat); sapply(pred_df, length)

## 3. Helper function to compute metrics  ----
get_metrics <- function(pred_probs, truth, threshold = 0.5) {
  
  # ensure numeric 0/1
  truth_num <- as.numeric(truth)
  
  # Accuracy at chosen threshold
  pred_class <- ifelse(pred_probs >= threshold, 1, 0)
  accuracy   <- mean(pred_class == truth_num)
  
  # AUC
  roc_obj <- pROC::roc(truth_num, pred_probs)
  auc_val <- as.numeric(roc_obj$auc)
  
  # Brier score & RMSE
  brier <- mean((pred_probs - truth_num)^2)
  rmse  <- sqrt(brier)
  
  data.frame(
    Accuracy = accuracy,
    AUC      = auc_val,
    Brier    = brier,
    RMSE     = rmse
  )
}

## 4. Apply to each model  ----
perf_list <- lapply(pred_df, get_metrics, truth = truth, threshold = 0.5)
perf_tbl  <- bind_rows(perf_list, .id = "Model")

## 5. Round for APA-style reporting  ----
perf_tbl <- perf_tbl %>%
  mutate(across(c(Accuracy, AUC, Brier, RMSE), ~ round(.x, 3)))

perf_tbl

library(h2o)
h2o.init()

leader     <- aml@leader
pred_aml   <- h2o.predict(leader, test_h2o)[, "p1"]  # or correct positive class column
pred_aml_vec <- as.vector(pred_aml)

library(dplyr)
library(pROC)
library(kableExtra)
library(knitr)

# True labels
truth <- test_dat$ti_binary_num   # numeric 0/1

# Predicted probabilities from each model (all plain R vectors now)
pred_df <- data.frame(
  Logistic_Regression = logit_pred_prob,
  Random_Forest       = rf_pred_prob,
  BART                = bart_pred_prob,
  AutoML_Best         = pred_aml_vec   # <-- note the _vec
)

# Quick sanity check
nrow(test_dat); sapply(pred_df, length)

get_metrics <- function(pred_probs, truth, threshold = 0.5) {
  truth_num <- as.numeric(truth)
  pred_class <- ifelse(pred_probs >= threshold, 1, 0)
  accuracy   <- mean(pred_class == truth_num)
  
  roc_obj <- pROC::roc(truth_num, pred_probs)
  auc_val <- as.numeric(roc_obj$auc)
  
  brier <- mean((pred_probs - truth_num)^2)
  rmse  <- sqrt(brier)
  
  data.frame(
    Accuracy = accuracy,
    AUC      = auc_val,
    Brier    = brier,
    RMSE     = rmse
  )
}

perf_list <- lapply(pred_df, get_metrics, truth = truth, threshold = 0.5)
perf_tbl  <- bind_rows(perf_list, .id = "Model") %>%
  mutate(across(c(Accuracy, AUC, Brier, RMSE), ~ round(.x, 3)))

kable(
  perf_tbl,
  caption = "Table 4\nComparison of model performance across predictive approaches.",
  booktabs = TRUE,
  align = "lcccc",
  col.names = c("Model", "Accuracy", "AUC", "Brier score", "RMSE")
) %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  footnote(
    general = "Note. Metrics are based on the held-out test set. Accuracy is calculated using a classification threshold of .50. AUC = area under the receiver operating characteristic curve; RMSE = root mean squared error.",
    general_title = ""
  )

# THRESHOLD SENSITIVITY

# install.packages(c("dplyr", "tidyr", "kableExtra"))  # if needed

library(dplyr)
library(tidyr)
library(kableExtra)
library(knitr)

# True labels (0/1)
truth <- test_dat$ti_binary_num

# Predicted probabilities for each model
pred_list <- list(
  "Logistic regression" = logit_pred_prob,
  "Random forest"       = rf_pred_prob,
  "BART"                = bart_pred_prob,
  "AutoML (best model)" = pred_aml_vec  # <-- use your AutoML prob vector here
)

# Thresholds to evaluate
thresholds <- c(0.40, 0.50, 0.60)

get_threshold_metrics <- function(pred_probs, truth, thresholds) {
  
  truth_num <- as.numeric(truth)
  
  # If it's 1/2 because of factor coding, fix to 0/1
  if (!all(truth_num %in% c(0, 1))) {
    truth_num <- truth_num - min(truth_num)
  }
  
  purrr::map_df(thresholds, function(th) {
    pred_class <- ifelse(pred_probs >= th, 1, 0)
    
    TP <- sum(pred_class == 1 & truth_num == 1)
    TN <- sum(pred_class == 0 & truth_num == 0)
    FP <- sum(pred_class == 1 & truth_num == 0)
    FN <- sum(pred_class == 0 & truth_num == 1)
    
    accuracy    <- (TP + TN) / (TP + TN + FP + FN)
    sensitivity <- ifelse((TP + FN) > 0, TP / (TP + FN), NA_real_)  # recall
    specificity <- ifelse((TN + FP) > 0, TN / (TN + FP), NA_real_)
    precision   <- ifelse((TP + FP) > 0, TP / (TP + FP), NA_real_)
    F1          <- ifelse(
      is.na(precision) | is.na(sensitivity) | (precision + sensitivity) == 0,
      NA_real_,
      2 * precision * sensitivity / (precision + sensitivity)
    )
    
    data.frame(
      Threshold  = th,
      Accuracy   = accuracy,
      Sensitivity = sensitivity,
      Specificity = specificity,
      F1         = F1
    )
  })
}

# Compute metrics for each model across thresholds
threshold_results <- purrr::imap_dfr(
  pred_list,
  ~ get_threshold_metrics(pred_probs = .x, truth = truth, thresholds = thresholds) %>%
    mutate(Model = .y)
)

# Reorder columns
threshold_results <- threshold_results %>%
  select(Model, Threshold, Accuracy, Sensitivity, Specificity, F1)

# Round for APA-style reporting
threshold_results <- threshold_results %>%
  mutate(
    Threshold  = sprintf("%.2f", Threshold),
    across(c(Accuracy, Sensitivity, Specificity, F1), ~ round(.x, 3))
  )

threshold_results

kable(
  threshold_results,
  caption = "Table 5\nClassification performance of models across alternative probability thresholds.",
  booktabs = TRUE,
  align = "lcccccc",
  col.names = c("Model", "Threshold", "Accuracy", "Sensitivity", "Specificity", "F1 score")
) %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  footnote(
    general = "Note. Sensitivity reflects the true positive rate; specificity reflects the true negative rate. F1 score is the harmonic mean of precision and recall. Metrics are based on the held-out test set.",
    general_title = ""
  )

# VARIABLE IMPORTANCE RANKINGS

library(dplyr)
library(kableExtra)
library(knitr)
library(randomForest)
library(bartMachine)
library(h2o)

# Predictor names in the order you used for modeling
vars <- colnames(x_train)    # 10 predictors
vars

var_labels <- c(
  "Organizational commitment",
  "Job satisfaction",
  "Work engagement",
  "Job embeddedness",
  "Perceived organizational support",
  "Overall justice",
  "Work environment",
  "Career growth opportunities",
  "Work–life balance",
  "Employee Net Promoter Score (eNPS)"
)
# Make sure length(var_labels) == length(vars)

# MeanDecreaseGini from randomForest
imp_rf_raw <- randomForest::importance(rf_model, type = 2)[, 1]

# Reorder to match your predictor order
imp_rf_raw <- imp_rf_raw[vars]

# Scale 0–1 for readability
imp_rf_scaled <- imp_rf_raw / max(imp_rf_raw, na.rm = TRUE)

# BART variable importance (proportion of splits), no plot
bart_imp <- bartMachine::investigate_var_importance(
  bart_model,
  num_replicates = 20,
  type = "splits",
  plot = FALSE   # <-- this avoids the figure margins error
)

# See what components are available
names(bart_imp)

# This is usually a named numeric vector with proportions:
imp_bart_all <- bart_imp$avg_var_props   # use the name you see in names(bart_imp)

# Reorder to match your predictor order
imp_bart <- imp_bart_all[vars]



# H2O variable importance for the leader model
varimp_aml <- as.data.frame(h2o.varimp(leader))

# Match AutoML importance to your predictor order
imp_aml_raw <- varimp_aml$relative_importance[
  match(vars, varimp_aml$variable)
]

imp_aml_scaled <- imp_aml_raw / max(imp_aml_raw, na.rm = TRUE)

# Use var_labels if you created them, otherwise use vars
predictor_names <- if (exists("var_labels")) var_labels else vars

varimp_tbl <- data.frame(
  Predictor             = predictor_names,
  RF_Importance         = round(imp_rf_scaled, 3),
  BART_Inclusion_Prob   = round(imp_bart, 3),
  AutoML_Importance     = round(imp_aml_scaled, 3),
  row.names             = NULL
)

varimp_tbl

kable(
  varimp_tbl,
  caption = "Table 6\nVariable importance rankings across machine-learning models.",
  booktabs = TRUE,
  align = "lccc",
  col.names = c(
    "Predictor",
    "Random forest importance",
    "BART inclusion probability",
    "AutoML relative importance"
  )
) %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  footnote(
    general = "Note. Random forest and AutoML importance values are rescaled to range from 0 to 1 within each model. BART values represent posterior inclusion probabilities from the Bayesian Additive Regression Trees model.",
    general_title = ""
  )

# TI Binary PLOT

# Load packages
library(dplyr)
library(ggplot2)

# 1. Extract turnover intention variable ------------------------------

# Replace 'turnover_intentions' with your actual variable name if needed
df_ti <- df_final %>%
  select(turnover_intentions) %>%
  filter(!is.na(turnover_intentions))

# 75th percentile cut-off used for the binary split
ti_cutoff <- quantile(df_ti$turnover_intentions, probs = 0.75, na.rm = TRUE)

# For placing the annotation text nicely
dens_obj <- density(df_ti$turnover_intentions)
y_max <- max(dens_obj$y)

ti_cutoff

# 2. Plot: histogram + density curve + vertical cut-off line ----------

ggplot(df_ti, aes(x = turnover_intentions)) +
  # histogram as density
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.5,
                 colour = "black",
                 fill   = "grey90") +
  # smoothed density curve
  geom_density(linewidth = 1) +
  # vertical line at 75th percentile
  geom_vline(xintercept = ti_cutoff,
             linetype   = "dashed",
             linewidth  = 0.9) +
  # label for the cut-off
  annotate("text",
           x = as.numeric(ti_cutoff),
           y = y_max * 0.9,
           label = "75th percentile\nbinary cut-off",
           hjust = -0.05,
           size = 4) +
  scale_x_continuous(breaks = seq(1, 5, by = 1)) +
  labs(
    x = "Turnover intention (Likert scale)",
    y = "Density"
  ) +
  theme_classic(base_size = 14)

# Correlation Heatmap

# -------------------------------------------------------
# FIGURE 4: Correlation heatmap of key study variables
# -------------------------------------------------------

library(dplyr)
library(ggplot2)
library(reshape2)

# 1. Define variable order exactly as shown in your correlation table ----------
var_order <- c(
  "Turnover Intentions",
  "Organizational Commitment",                     # Organizational commitment
  "Job Satisfaction",
  "Work Engagement",                  # Work engagement
  "Job Embeddedness",
  "Perceived Org Support",                  # Perceived organizational support
  "Perceived Org Justice",                  # Organizational justice
  "Work Environment",
  "Org Career Growth",
  "Work Life Balance",
  "eNPS"
)

# 2. Extract these variables from your dataset -------------------------------
corr_df <- df_final %>% select(all_of(var_order))

# 3. Compute correlation matrix ----------------------------------------------
cor_mat <- cor(corr_df, use = "pairwise.complete.obs")

# 4. Keep only lower triangle -------------------------------------------------
cor_mat[upper.tri(cor_mat)] <- NA

# 5. Convert to long format for ggplot ----------------------------------------
cor_long <- melt(cor_mat, varnames = c("Var1", "Var2"), value.name = "r") %>%
  na.omit()

# Enforce same variable order as your correlation table
cor_long$Var1 <- factor(cor_long$Var1, levels = rev(var_order))  
cor_long$Var2 <- factor(cor_long$Var2, levels = var_order)

# 6. Plot heatmap -------------------------------------------------------------
ggplot(cor_long, aes(x = Var2, y = Var1, fill = r)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#2166AC",   # blue for negative correlations
    mid = "white",
    high = "#B2182B",  # red for positive correlations
    midpoint = 0,
    limits = c(-1, 1),
    name = "r"
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )
pred_aml_probs <- as.vector(pred_aml$predict)
names(pred_aml)
pred_aml_probs <- as.vector(pred_aml$p1)

# Calibration Plots for Models

library(dplyr)
library(ggplot2)
library(scales)

# 0. True labels (0/1)
truth <- as.numeric(test_dat$ti_binary_num)

# 1. Ensure all predictors are numeric vectors -----------------------------
logit_probs <- as.numeric(logit_pred_prob)
rf_probs    <- as.numeric(rf_pred_prob)
bart_probs  <- as.numeric(bart_pred_prob)

aml_pred_df <- as.data.frame(pred_aml)
names(aml_pred_df)   # optional: inspect
aml_probs   <- as.numeric(aml_pred_df[["p1"]])   # prob of class 1

# 2. Helper to compute calibration bins -----------------------------------
calibration_df <- function(truth, preds, model_name, n_bins = 10){
  df <- data.frame(truth = truth, pred = preds)
  
  df$bin <- cut(
    df$pred,
    breaks = quantile(df$pred, probs = seq(0, 1, length.out = n_bins + 1),
                      na.rm = TRUE),
    include.lowest = TRUE
  )
  
  df %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(
      mean_pred = mean(pred, na.rm = TRUE),
      mean_obs  = mean(truth, na.rm = TRUE),
      n         = dplyr::n(),
      .groups   = "drop"
    ) %>%
    dplyr::mutate(model = model_name)
}

# 3. Build calibration datasets for all models -----------------------------
cal_logit <- calibration_df(truth, logit_probs, "Logistic regression")
cal_rf    <- calibration_df(truth, rf_probs,    "Random forest")
cal_bart  <- calibration_df(truth, bart_probs,  "BART")
cal_aml   <- calibration_df(truth, aml_probs,   "AutoML (best model)")

cal_all <- dplyr::bind_rows(cal_logit, cal_rf, cal_bart, cal_aml)

# 4. Plot calibration curves -----------------------------------------------
ggplot(cal_all, aes(x = mean_pred, y = mean_obs, color = model)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_x_continuous("Predicted probability",
                     labels = percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  scale_y_continuous("Observed proportion",
                     labels = percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# VARIABLE IMPORTANCE PLOT RANDOM FOREST

# Packages
library(randomForest)
library(dplyr)
library(ggplot2)

# ------------------------------------------------------------------
# 1. Define predictor variable names (in the order used in your thesis)
# ------------------------------------------------------------------
vars <- c(
  "oc_kut",          # Organizational commitment
  "job_satisfaction",
  "we_total",        # Work engagement
  "job_embeddedness",
  "pos_mean",        # Perceived organizational support
  "oj_total",        # Organizational justice
  "work_environment",
  "org_career_growth",
  "work_life_balance",
  "enps_raw"         # eNPS
)

# Nice, thesis-friendly labels for the plot
var_labels <- c(
  "Organizational commitment",
  "Job satisfaction",
  "Work engagement",
  "Job embeddedness",
  "Perceived organizational support",
  "Organizational justice",
  "Work environment",
  "Career growth opportunities",
  "Work–life balance",
  "Employee Net Promoter Score (eNPS)"
)

# ------------------------------------------------------------------
# 2. Extract and scale random forest importance
#    (MeanDecreaseGini, scaled 0–1 for readability)
# ------------------------------------------------------------------
# Get importance vector (type = 2 -> MeanDecreaseGini)
imp_rf_raw <- randomForest::importance(rf_model, type = 2)[, 1]

# Reorder to match your vars vector
imp_rf_raw <- imp_rf_raw[vars]

# Scale 0–1 so the largest = 1
imp_rf_scaled <- imp_rf_raw / max(imp_rf_raw, na.rm = TRUE)

# Put into a data frame for ggplot
imp_df <- data.frame(
  Predictor  = factor(var_labels, levels = rev(var_labels)),  # reversed for top at top
  Importance = imp_rf_scaled
)

# ------------------------------------------------------------------
# 3. Plot: Random Forest variable importance (Figure 7)
# ------------------------------------------------------------------
rf_varimp_plot <- ggplot(imp_df, aes(x = Importance, y = Predictor)) +
  geom_col() +
  labs(
    x = "Relative importance (scaled 0–1)",
    y = NULL,
    title = "Random forest variable importance for predicting turnover intention"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

# Print to the Plots pane
rf_varimp_plot

# (Optional) Save to file for your thesis
# ggsave("figure7_rf_variable_importance.png",
#        rf_varimp_plot, width = 8, height = 6, dpi = 300)

# Assuming your dataframe is called `df`
colSums(is.na(df_final))

library(dplyr)

df_final <- df_final %>%
  rename(
    Turnover_Intentions = turnover_intentions,
    Organizational_Commitment = oc_kut,
    Job_Satisfaction = job_satisfaction,
    Work_Engagement = we_total,
    Job_Embeddedness = job_embeddedness,
    Perceived_Org_Support = pos_mean,
    Perceived_Org_Justice = oj_total,
    Work_Environment = work_environment,
    Career_Growth_Opportunities = org_career_growth,
    Work_Life_Balance = work_life_balance,
    eNPS = enps_raw
  )

var_order <- c(
  "Turnover_Intentions",
  "Organizational_Commitment",
  "Job_Satisfaction",
  "Work_Engagement",
  "Job_Embeddedness",
  "Perceived_Org_Support",
  "Perceived_Org_Justice",
  "Work_Environment",
  "Career_Growth_Opportunities",
  "Work_Life_Balance",
  "eNPS"
)

corr_df <- df_final %>% select(all_of(var_order))

library(dplyr)
library(reshape2)
library(ggplot2)

# (Optional) clear old correlation objects that still have old names
rm(list = c("corr_df", "corr_obj"), envir = .GlobalEnv)

corr_df <- df_final %>%
  select(
    Turnover_Intentions,
    Organizational_Commitment,
    Job_Satisfaction,
    Work_Engagement,
    Job_Embeddedness,
    Perceived_Org_Support,
    Perceived_Org_Justice,
    Work_Environment,
    Career_Growth_Opportunities,
    Work_Life_Balance,
    eNPS
  )

corr_mat <- cor(corr_df, use = "pairwise.complete.obs")

corr_long <- melt(corr_mat, varnames = c("Var1", "Var2"), value.name = "r")

corr_long <- corr_long %>%
  filter(as.numeric(Var1) > as.numeric(Var2))

ggplot(corr_long, aes(x = Var1, y = Var2, fill = r)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#4575B4",
    mid = "white",
    high = "#D73027",
    midpoint = 0,
    name = "r"
  ) +
  coord_fixed() +
  labs(x = "", y = "", title = "Correlation Heatmap of Study Variables") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )


library(ggplot2)
library(reshape2)

# Convert correlation matrix to long format
corr_long <- corr_obj %>%
  as.matrix() %>%
  melt(na.rm = TRUE)

# Plot (triangular)
ggplot(corr_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#4575B4", mid = "white", high = "#D73027",
                       midpoint = 0, name = "r") +
  coord_fixed() +
  labs(x = "", y = "", 
       title = "Correlation Heatmap of Study Variables") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

# Install / load pROC if you haven’t already
# install.packages("pROC")
library(pROC)

# Ensure outcome is coded correctly: 1 = "high turnover risk" (positive class)
# If your outcome is a factor, pROC will treat the *second* level as the "event"
# You can force this explicitly:
# truth <- factor(truth, levels = c(0, 1))   # 0 = low, 1 = high

# Helper function: get AUC and 95% CI via stratified bootstrap
get_auc_ci <- function(truth, probs, boot_n = 2000, conf_level = 0.95) {
  roc_obj <- roc(truth, probs, quiet = TRUE)
  ci_obj  <- ci.auc(roc_obj,
                    boot.n      = boot_n,
                    conf.level  = conf_level,
                    boot.stratified = TRUE)
  
  c(
    AUC    = as.numeric(auc(roc_obj)),
    CI_low = as.numeric(ci_obj[1]),
    CI_mid = as.numeric(ci_obj[2]),  # same as AUC but from CI object
    CI_high= as.numeric(ci_obj[3])
  )
}

# Run for each model ----------------------------------------------

results_mat <- rbind(
  Logistic_Regression = get_auc_ci(truth, logit_probs),
  Random_Forest       = get_auc_ci(truth, rf_probs),
  BART                = get_auc_ci(truth, bart_probs),
  AutoML_Best         = get_auc_ci(truth, aml_probs)
)

auc_ci_results <- as.data.frame(results_mat)
round(auc_ci_results, 3)

## RAW LOG REG

install.packages("yhat")  # Only run once
library(yhat)

logit_model <- glm(ti_binary ~ Organizational_Commitment,
                   Job_Satisfaction,
                   Work_Engagement,
                   Job_Embeddedness,
                   Perceived_Org_Support,
                   Perceived_Org_Justice,
                   Work_Environment,
                   Career_Growth_Opportunities,
                   Work_Life_Balance,
                   eNPS,
                   data = df_final, family = binomial)

names(df_final)
names(df_final)[grepl("Embed", names(df_final))]
any(names(df_final) == "Job_Embeddedness")
head(df_final$Job_Embeddedness)

logit_model <- glm(
  ti_binary ~ Organizational_Commitment +
    Job_Satisfaction +
    Work_Engagement +
    Job_Embeddedness +
    Perceived_Org_Support +
    Perceived_Org_Justice +
    Work_Environment +
    Career_Growth_Opportunities +
    Work_Life_Balance +
    eNPS,
  data = df_final,
  family = binomial()
)

library(yhat)

# 1. Design matrix from your logistic regression (drop intercept)
X       <- model.matrix(logit_model)[, -1]

# 2. Linear predictor (log-odds) from the logistic regression
y_logit <- logit_model$linear.predictors

# 3. Put everything into a data frame
logit_dat <- data.frame(y_logit = y_logit, X)

# 4. Fit linear model: logit (y_logit) ~ all predictors
lm_logit <- lm(y_logit ~ ., data = logit_dat)

# 5. Get RAW / relative weights
raw_out <- calc.yhat(lm_logit)

# 6. Inspect the predictor metrics table
raw_out$PredictorMetrics

names(raw_out$PredictorMetrics)


str(raw_out)
str(raw_out$PredictorMetrics)
raw_df <- as.data.frame(raw_out$PredictorMetrics)
raw_df
raw_df <- data.frame(
  Predictor = raw_out$PredictorMetrics$Predictor,
  RawWeight = raw_out$PredictorMetrics$raw.weight,
  RescaledWeight = raw_out$PredictorMetrics$rescaled.weight
)

# 1. Grab the matrix
raw_mat <- as.matrix(raw_out$PredictorMetrics)

# 2. Turn into a data frame and keep rownames as a column
raw_df <- data.frame(
  Predictor     = rownames(raw_mat),
  RawWeight     = raw_mat[, 1],  # first column
  RescaledWeight = raw_mat[, 3]  # third column
)

# 3. (Optional) Drop the 'Total' row if you only want predictors
raw_df <- subset(raw_df, Predictor != "Total")

raw_df