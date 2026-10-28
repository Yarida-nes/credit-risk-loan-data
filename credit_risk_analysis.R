# Credit risk: what the loan data shows
# Yarida Kaewthong, Master of Commerce (AI and Business Analytics), Curtin University
# Group assignment, ECOM5005 Business Analytics, October 2025
#
# Data: credit_risk_dataset.csv (public dataset, see README)
# Run:  Rscript credit_risk_analysis.R   -> charts in outputs/, numbers printed below

suppressPackageStartupMessages({
  library(tidyverse)
  library(corrplot)
})
dir.create("outputs", showWarnings = FALSE)

# ---- 1. Load and clean ----------------------------------------------------
loan_status <- read.csv("data/credit_risk_dataset.csv")

cat("Rows loaded:", nrow(loan_status), "\n")
cat("Variables:", ncol(loan_status),
    "| character variables:", sum(sapply(loan_status, is.character)), "\n")
cat("Missing data points:", sum(is.na(loan_status)), "\n")

# Drop rows with any missing value
loan_clean <- na.omit(loan_status)

# Character columns to factors; recode loan_status 1 = default, 0 = non-default
loan_clean <- loan_clean %>%
  mutate(across(where(is.character), as.factor),
         loan_status = as.factor(ifelse(loan_status == 1, "default", "non-default")))

# Shorter variable names
loan_clean <- loan_clean %>%
  rename(
    age           = person_age,
    income        = person_income,
    home          = person_home_ownership,
    empLength     = person_emp_length,
    intent        = loan_intent,
    grade         = loan_grade,
    loanamt       = loan_amnt,
    intrate       = loan_int_rate,
    status        = loan_status,
    loanpctincome = loan_percent_income,
    defaultflag   = cb_person_default_on_file,
    credhist      = cb_person_cred_hist_length
  )

# Drop the ID and grade columns, and rows with zero income
loan_clean <- loan_clean %>%
  select(-ID, -grade) %>%
  filter(income > 0)

cat("Rows after cleaning:", nrow(loan_clean), "\n")

# ---- 2. Explore ------------------------------------------------------------
# Income distribution by loan status
p_income <- ggplot(loan_clean, aes(x = income)) +
  geom_histogram(binwidth = 10000, color = "darkblue", fill = "lightblue") +
  coord_cartesian(xlim = c(0, 250000)) +
  facet_grid(~ status, scales = "free_y") +
  labs(title = "Income distribution by loan status", x = "Income", y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("outputs/income_by_status.png", p_income, width = 9, height = 4.5, dpi = 150)

# Correlation matrix of the numeric variables
numeric_data <- loan_clean %>% select(where(is.numeric))
cor_mat <- cor(numeric_data, use = "pairwise.complete.obs", method = "pearson")
png("outputs/correlation_matrix.png", width = 1000, height = 900, res = 150)
corrplot(cor_mat, method = "color", type = "upper",
         tl.cex = 0.8, addCoef.col = "black", number.cex = 0.6)
dev.off()

# Count and share of each loan status
loan_summary <- loan_clean %>%
  group_by(status) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(proportion = round(count / sum(count) * 100, 2))
print(loan_summary)
write.csv(loan_summary, "outputs/loan_status_summary.csv", row.names = FALSE)

p_bar <- ggplot(loan_summary, aes(x = status, y = count, fill = status)) +
  geom_col(width = 0.6) +
  labs(title = "Loan status", x = "Loan status", y = "Count of individuals") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("outputs/loan_status_bar.png", p_bar, width = 6, height = 4.5, dpi = 150)

# ---- 3. Income regression --------------------------------------------------
income_model <- lm(
  income ~ age + home + empLength + intent + loanamt +
    intrate + status + loanpctincome + defaultflag + credhist,
  data = loan_clean
)
s <- summary(income_model)
cat("\nIncome regression (all predictors): R-squared =", round(s$r.squared, 3),
    "| adjusted R-squared =", round(s$adj.r.squared, 3), "\n")

# Backward stepwise selection
income_bwd_step <- step(income_model, direction = "backward", trace = 0)
cat("Predictors kept by backward stepwise:",
    paste(attr(terms(income_bwd_step), "term.labels"), collapse = ", "), "\n")
cat("Stepwise model: adjusted R-squared =",
    round(summary(income_bwd_step)$adj.r.squared, 3), "\n")

capture.output(summary(income_model), file = "outputs/income_model_summary.txt")
capture.output(summary(income_bwd_step), file = "outputs/income_stepwise_summary.txt")
cat("\nDone. Charts and summaries are in outputs/\n")

# ---- 4. Logistic regression: who defaults? --------------------------------
# Reconstructed after the submitted group file was lost. Follows the assignment
# steps: 70/30 split stratified by status x intent, seed 123, forward stepwise
# selection on the training data, evaluation on the test data at threshold 0.5.
suppressPackageStartupMessages({
  library(rsample)
  library(yardstick)
  library(caTools)
})
set.seed(123)

loan_split_df <- loan_clean %>% mutate(strata_var = interaction(status, intent))
split    <- initial_split(loan_split_df, prop = 0.7, strata = strata_var)
train    <- training(split) %>% select(-strata_var)
test     <- testing(split)  %>% select(-strata_var)

cat("\nTrain rows:", nrow(train), "| test rows:", nrow(test), "\n")
cat("Share of default in train:", round(mean(train$status == "default"), 3),
    "| in test:", round(mean(test$status == "default"), 3), "\n")

# status is a factor with levels default / non-default; model P(status = non-default)
# so flip it to model P(default), which is what we care about.
train <- train %>% mutate(status = relevel(status, ref = "non-default"))
test  <- test  %>% mutate(status = relevel(status, ref = "non-default"))

Status_m0  <- glm(status ~ 1, data = train, family = binomial)
Status_all <- glm(status ~ ., data = train, family = binomial)
Status_fwd_step <- step(Status_m0, scope = formula(Status_all),
                        direction = "forward", trace = 0)
cat("Predictors chosen by forward stepwise:",
    paste(attr(terms(Status_fwd_step), "term.labels"), collapse = ", "), "\n")

Status_final <- glm(formula(Status_fwd_step), data = train, family = binomial)
capture.output(summary(Status_final), file = "outputs/logistic_model_summary.txt")

# Predicted probability of default on the test data
Status_final_pred <- predict(Status_final, newdata = test, type = "response")
Status_pred_pt50  <- factor(ifelse(Status_final_pred >= 0.5, "default", "non-default"),
                            levels = c("default", "non-default"))
truth <- factor(test$status, levels = c("default", "non-default"))

acc_th50  <- accuracy_vec(truth, Status_pred_pt50)
sens_th50 <- sens_vec(truth, Status_pred_pt50)   # share of real defaults caught
spec_th50 <- spec_vec(truth, Status_pred_pt50)   # share of non-defaults correctly cleared
auc_val   <- colAUC(Status_final_pred, truth)[1, 1]

cat("Test accuracy:", round(acc_th50, 3),
    "| sensitivity:", round(sens_th50, 3),
    "| specificity:", round(spec_th50, 3),
    "| AUC:", round(auc_val, 3), "\n")

png("outputs/roc_curve.png", width = 900, height = 800, res = 150)
colAUC(Status_final_pred, truth, plotROC = TRUE)
dev.off()

write.csv(data.frame(metric = c("accuracy", "sensitivity", "specificity", "auc"),
                     value = round(c(acc_th50, sens_th50, spec_th50, auc_val), 3)),
          "outputs/logistic_metrics.csv", row.names = FALSE)

cat("\nDone. Charts and summaries are in outputs/\n")
