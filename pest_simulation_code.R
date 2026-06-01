# Behavioural-Economic Simulation of Pest Reporting and Phytosanitary Policy

# -----------------------------
# 1. Load Packages
# -----------------------------

# Install ggplot2 once if needed:
# install.packages("ggplot2")

library(ggplot2)

# -----------------------------
# 2. Set Seed
# -----------------------------

set.seed(123)

# -----------------------------
# 3. Create Folders
# -----------------------------

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 4. Generate Synthetic Farm Data
# -----------------------------

n <- 200

farm_id <- 1:n

farm_type <- sample(
  c("small", "medium", "large"),
  size = n,
  replace = TRUE,
  prob = c(0.60, 0.30, 0.10)
)

farm_size_ha <- numeric(n)

farm_size_ha[farm_type == "small"] <- runif(
  sum(farm_type == "small"), 1, 5
)

farm_size_ha[farm_type == "medium"] <- runif(
  sum(farm_type == "medium"), 5, 30
)

farm_size_ha[farm_type == "large"] <- runif(
  sum(farm_type == "large"), 30, 100
)

crop_type <- sample(
  c("wheat", "potato", "tomato", "apple"),
  size = n,
  replace = TRUE,
  prob = c(0.40, 0.25, 0.20, 0.15)
)

crop_value_ha <- numeric(n)
pest_risk <- numeric(n)
prevention_cost_ha <- numeric(n)

crop_value_ha[crop_type == "wheat"] <- runif(sum(crop_type == "wheat"), 1200, 2500)
crop_value_ha[crop_type == "potato"] <- runif(sum(crop_type == "potato"), 6000, 12000)
crop_value_ha[crop_type == "tomato"] <- runif(sum(crop_type == "tomato"), 10000, 25000)
crop_value_ha[crop_type == "apple"] <- runif(sum(crop_type == "apple"), 8000, 20000)

pest_risk[crop_type == "wheat"] <- runif(sum(crop_type == "wheat"), 0.05, 0.20)
pest_risk[crop_type == "potato"] <- runif(sum(crop_type == "potato"), 0.15, 0.35)
pest_risk[crop_type == "tomato"] <- runif(sum(crop_type == "tomato"), 0.20, 0.45)
pest_risk[crop_type == "apple"] <- runif(sum(crop_type == "apple"), 0.10, 0.30)

prevention_cost_ha[crop_type == "wheat"] <- runif(sum(crop_type == "wheat"), 50, 150)
prevention_cost_ha[crop_type == "potato"] <- runif(sum(crop_type == "potato"), 150, 400)
prevention_cost_ha[crop_type == "tomato"] <- runif(sum(crop_type == "tomato"), 250, 700)
prevention_cost_ha[crop_type == "apple"] <- runif(sum(crop_type == "apple"), 200, 600)

loss_rate <- runif(n, 0.10, 0.60)

compensation <- sample(c(0, 100, 300, 600), n, replace = TRUE)

monitoring <- sample(c(0, 1), n, replace = TRUE, prob = c(0.50, 0.50))

# -----------------------------
# 5. Add Behavioural Variables
# -----------------------------

risk_aversion <- runif(n, 0, 1)

expected_loss <- farm_size_ha * crop_value_ha * loss_rate * pest_risk

behaviour_noise <- rnorm(n, mean = 0, sd = 0.08)

report_prob <- 0.20 +
  0.0005 * compensation +
  0.20 * monitoring +
  0.15 * pest_risk -
  0.0002 * prevention_cost_ha +
  0.15 * risk_aversion +
  behaviour_noise

report_prob <- pmin(pmax(report_prob, 0.05), 0.95)

reports <- rbinom(n, size = 1, prob = report_prob)

final_loss <- ifelse(
  reports == 1,
  expected_loss * 0.6,
  expected_loss * 1.4
)

farm_data <- data.frame(
  farm_id,
  farm_type,
  farm_size_ha,
  crop_type,
  crop_value_ha,
  pest_risk,
  prevention_cost_ha,
  loss_rate,
  compensation,
  monitoring,
  risk_aversion,
  expected_loss,
  report_prob,
  reports,
  final_loss
)

farm_data$farm_type <- factor(farm_data$farm_type)
farm_data$crop_type <- factor(farm_data$crop_type)

# -----------------------------
# 6. Estimate Linear and Logistic Models
# -----------------------------

linear_model <- lm(
  report_prob ~ compensation + monitoring + pest_risk +
    prevention_cost_ha + risk_aversion + farm_size_ha,
  data = farm_data
)

logistic_model <- glm(
  reports ~ compensation + monitoring + pest_risk +
    prevention_cost_ha + risk_aversion + farm_size_ha,
  family = binomial,
  data = farm_data
)

# -----------------------------
# 7. Create Policy Scenarios
# -----------------------------

calculate_reporting <- function(data) {
  noise <- rnorm(nrow(data), mean = 0, sd = 0.08)
  
  prob <- 0.20 +
    0.0005 * data$compensation +
    0.20 * data$monitoring +
    0.15 * data$pest_risk -
    0.0002 * data$prevention_cost_ha +
    0.15 * data$risk_aversion +
    noise
  
  prob <- pmin(pmax(prob, 0.05), 0.95)
  
  return(prob)
}

create_scenario <- function(data, scenario_name, compensation_level, monitoring_level) {
  scenario_data <- data
  
  scenario_data$Scenario <- scenario_name
  scenario_data$compensation <- compensation_level
  scenario_data$monitoring <- monitoring_level
  
  scenario_data$report_prob <- calculate_reporting(scenario_data)
  
  scenario_data$reports <- rbinom(
    nrow(scenario_data),
    size = 1,
    prob = scenario_data$report_prob
  )
  
  scenario_data$final_loss <- ifelse(
    scenario_data$reports == 1,
    scenario_data$expected_loss * 0.6,
    scenario_data$expected_loss * 1.4
  )
  
  return(scenario_data)
}

baseline <- create_scenario(
  farm_data,
  "Baseline",
  compensation_level = 0,
  monitoring_level = 0
)

compensation_only <- create_scenario(
  farm_data,
  "Compensation only",
  compensation_level = 300,
  monitoring_level = 0
)

monitoring_only <- create_scenario(
  farm_data,
  "Monitoring only",
  compensation_level = 0,
  monitoring_level = 1
)

combined_policy <- create_scenario(
  farm_data,
  "Combined policy",
  compensation_level = 300,
  monitoring_level = 1
)

# -----------------------------
# 8. Calculate Results
# -----------------------------

calculate_results <- function(data) {
  reporting_rate <- mean(data$reports)
  pest_loss <- sum(data$final_loss)
  government_cost <- sum(data$compensation * data$reports)
  social_cost <- pest_loss + government_cost
  
  results_row <- data.frame(
    Scenario = unique(data$Scenario),
    ReportingRate = reporting_rate,
    PestLoss = pest_loss,
    GovernmentCost = government_cost,
    SocialCost = social_cost
  )
  
  return(results_row)
}

results <- rbind(
  calculate_results(baseline),
  calculate_results(compensation_only),
  calculate_results(monitoring_only),
  calculate_results(combined_policy)
)

results$Scenario <- factor(
  results$Scenario,
  levels = c(
    "Baseline",
    "Compensation only",
    "Monitoring only",
    "Combined policy"
  )
)

# -----------------------------
# 9. Create and Save Figures
# -----------------------------

reporting_rates_plot <- ggplot(
  results,
  aes(x = Scenario, y = ReportingRate)
) +
  geom_col(fill = "darkgreen") +
  ylim(0, 1) +
  labs(
    title = "Reporting Rates under Alternative Policies",
    x = "",
    y = "Reporting Rate"
  ) +
  theme_minimal()

social_costs_plot <- ggplot(
  results,
  aes(x = Scenario, y = SocialCost)
) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Social Costs under Alternative Pest Management Policies",
    x = "",
    y = "Total Social Cost (€)"
  ) +
  theme_minimal()

pest_losses_plot <- ggplot(
  results,
  aes(x = Scenario, y = PestLoss)
) +
  geom_col(fill = "tomato") +
  labs(
    title = "Pest Losses under Alternative Policies",
    x = "",
    y = "Total Pest Loss"
  ) +
  theme_minimal()

baseline_pest_loss <- results$PestLoss[results$Scenario == "Baseline"]

results$PestLossReduction <- baseline_pest_loss - results$PestLoss

pest_loss_reduction_plot <- ggplot(
  results,
  aes(x = Scenario, y = PestLossReduction)
) +
  geom_col(fill = "darkorange") +
  labs(
    title = "Reduction in Pest Loss Compared with Baseline",
    x = "",
    y = "Pest Loss Reduction"
  ) +
  theme_minimal()

ggsave(
  filename = "outputs/figures/pest_loss_reduction.png",
  plot = pest_loss_reduction_plot,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  filename = "outputs/figures/reporting_rates.png",
  plot = reporting_rates_plot,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  filename = "outputs/figures/social_costs.png",
  plot = social_costs_plot,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  filename = "outputs/figures/pest_losses.png",
  plot = pest_losses_plot,
  width = 8,
  height = 5,
  dpi = 300
)


# -----------------------------
# 10. Export Tables
# -----------------------------

write.csv(
  results,
  file = "outputs/tables/results.csv",
  row.names = FALSE
)
write.csv(
  farm_data,
  file = "outputs/tables/synthetic_farm_data.csv",
  row.names = FALSE
)

# -----------------------------
# 11. Print Final Outputs
# -----------------------------

cat("\nFirst rows of farm_data:\n")
print(head(farm_data))

cat("\nSummary of farm_data:\n")
print(summary(farm_data))

cat("\nLinear regression results:\n")
print(summary(linear_model))

cat("\nLogistic regression results:\n")
print(summary(logistic_model))

cat("\nFinal policy results:\n")
print(results)