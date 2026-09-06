# Downloading libraries
library(openxlsx)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lmtest)
library(sandwich)
library(ggrepel)
library(qtl2)
library(car)
library(ggeffects)
library(emmeans)
library(mediation)
library(betareg)
library(e1071)
library(patchwork)
library(RColorBrewer)
library(effectsize)
library(knitr)
library(gt)
library(broom)

# Setting visuals to default
colors <- brewer.pal(3, "Dark2")
### Part 1 Reading and filtering datasets

# Reading aipi index
aipi_index <- read.xlsx("aipidata.xlsx",startRow = 2, colNames = TRUE )
aipi_index_state <- list(aipi_index$Country)

#Reading law_ment 
ai_ment <- read.csv("ai_mentions.csv")
ai_ment_state <- list(ai_ment$Geographic.area)

# Renaming unmatched names of the states and merging 2 dfs
rename_map <- c(
  "Czechia"              = "Czech Republic",
  "Korea"                = "South Korea",
  "Russian Federation"   = "Russia"
)

aipi_index <- aipi_index %>%
  mutate(Country = dplyr::recode(Country, !!!rename_map))

ai_ment_full <- ai_ment %>%
  inner_join(aipi_index, by = c("Geographic.area" = "Country"))

#Filter out all states with no AIPI (ai_ment df is clean) - Macao is deleted
ai_ment_full <- ai_ment_full%>%
  filter(!is.na(AIPI))

# Rename the var. name for convenience in use 
ai_ment_full <- rename(ai_ment_full, "ai_mention" = "Number.of.mentions.of.AI.in.legislative.proceedings")

# Calculating AIPI_no_reg to avoid overlapping between X (already represents law) and Y
ai_ment_full <- ai_ment_full %>%
  mutate(AIPI_no_reg = Digitial.Infrastructure + Innovation.and.Economic.Integration + Human.Capital.and.Labor.Market.Policies)

#Getting the list of the iso3 names to add gdp per capita and median age
country_m_iso3 <- ai_ment_full$iso3

# Adding gdp per capita
gdp_pc_m_df <- read_csv("GDP_per_capita.csv", skip = 3)

gdp_pc_m_df <- gdp_pc_m_df %>%
  dplyr::select(`Country Code`, `2023`, `2024`) %>% 
  rename("country_code" = `Country Code`) %>%
  mutate(gdp_pc_latest = coalesce(`2024`, `2023`)) %>%  # take 2024 if available, else 2023
  filter(country_code %in% country_m_iso3) %>%
  dplyr::select(country_code, gdp_pc_latest)

ai_ment_full <- ai_ment_full %>%
  left_join(gdp_pc_m_df, by = c("iso3" = "country_code"))

# Adding median age (in years)
age_m_df <- read.xlsx("Median age(years).xlsx", sheet = 2)

age_m_df <- age_m_df %>%
  dplyr::select('Country.ISO.3.code', 'Value.Numeric') %>%
  rename("median_age_in_years" = "Value.Numeric") %>%
  filter(Country.ISO.3.code %in% country_m_iso3)

ai_ment_full <- ai_ment_full %>%
  left_join(age_m_df,by = c("iso3" = "Country.ISO.3.code"))

# Description of variables
# ai_mention

# Basic stats:
summary(ai_ment_full$a_mention)
mean(ai_ment_full$a_mention)
sd(ai_ment_full$ai_mention)
median(ai_ment_full$ai_mention)
range(ai_ment_full$ai_mention)
sum(ai_ment_full$ai_mention == 0)

# Summary statistics to use on the plot
stats_ai_mention <- ai_ment_full %>%
  summarise(
    Mean = mean(ai_mention, na.rm = TRUE),
    Median = median(ai_mention, na.rm = TRUE),
    SD = sd(ai_mention, na.rm = TRUE),
    Min = min(ai_mention, na.rm = TRUE),
    Max = max(ai_mention, na.rm = TRUE)
  )

#Formating
stats_text <- paste0(
  "Mean = ", round(stats_ai_mention$Mean, 1),
  "\nMedian = ", round(stats_ai_mention$Median, 1),
  "\nSD = ", round(stats_ai_mention$SD, 1),
  "\nMin = ", stats_ai_mention$Min,
  "\nMax = ", stats_ai_mention$Max
)
# Plot of raw distr.
p1 <- ggplot(ai_ment_full, aes(x = ai_mention)) +
  geom_density(fill = colors[1], alpha = 0.5) +
  labs(
    title = "Raw Data",
    x = "AI Mentions (num. of mentions)", y = "Density"
  ) +
  theme_minimal(base_size = 12) +
  annotate("text", x = Inf, y = Inf, label = stats_text,
           hjust = 1.1, vjust = 1.1, size = 3.5, color = "gray20")

# Plot of log1p transformation
p2 <- ggplot(ai_ment_full, aes(x = log1p(ai_mention))) +
  geom_density(fill = colors[2], alpha = 0.5) +
  labs(
    title = "log(1+x) Transformed",
    x = "log(1 + AI Mentions)", y = "Density"
  ) +
  theme_minimal(base_size = 12)

# Combine side-by-side
combined_plot <- p1 + p2 + plot_annotation(
  title = "Distribution of AI Mentions 2016-2024 (Sum) in Legal Proceeings in General Sample",
  subtitle = "Raw vs. log(1+x)-transformed values with summary statistics",
  caption = "n = 64",
  theme = theme(plot.title = element_text(face = "bold", size = 12)),
)

combined_plot

# AIPI_no_reg

# Compute summary statistics
aipi_summary <- ai_ment_full %>%
  summarise(
    mean = mean(AIPI_no_reg, na.rm = TRUE),
    median = median(AIPI_no_reg, na.rm = TRUE),
    sd = sd(AIPI_no_reg, na.rm = TRUE),
    min = min(AIPI_no_reg, na.rm = TRUE),
    max = max(AIPI_no_reg, na.rm = TRUE)
  )

# Create formatted summary text
summary_text <- paste0(
  "Mean = ", round(aipi_summary$mean, 3),
  "; Median =",round(aipi_summary$median, 3),
  "; SD = ", round(aipi_summary$sd, 3),
  "; Range = [", round(aipi_summary$min, 3), ", ", round(aipi_summary$max, 3), "]"
)

# Plot
ggplot(ai_ment_full, aes(x = AIPI_no_reg)) +
  geom_histogram(aes(y = ..density..), bins = 20,
                 fill = colors[1], color = "white", alpha = 0.8) +
  geom_density(color = colors[2], size = 1) +
  geom_vline(xintercept = aipi_summary$mean,   color = colors[2], linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = aipi_summary$median, color = colors[3], linetype = "dotted", linewidth = 0.8) +
  labs(
    title = "Distribution of AIPI (without regulation component)",
    subtitle = summary_text,
    x = "AIPI",
    y = "Density",
    caption = "Dashed line = Mean; Dotted line = Median; n = 64"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor = element_line(color = "gray90", linewidth = 0.2),
    panel.background = element_blank(),
    plot.title = element_text(face = "bold")
  )

# GDP per capita

# Summary statistics
gdp_stats <- summary(ai_ment_full$gdp_pc_latest)
gdp_mean <- mean(ai_ment_full$gdp_pc_latest, na.rm = TRUE)
gdp_sd <- sd(ai_ment_full$gdp_pc_latest, na.rm = TRUE)

summary_text <- paste0(
  "Min = ", round(gdp_stats[1], 0), "\n",
  "Median = ", round(gdp_stats[3], 0), "\n",
  "Mean = ", round(gdp_mean, 0), "\n",
  "Max = ", round(gdp_stats[6], 0), "\n",
  "SD = ", round(gdp_sd, 0)
)

# Raw GDP per capita
p1 <- ggplot(ai_ment_full, aes(x = gdp_pc_latest)) +
  geom_histogram(aes(y = ..density..), bins = 25, 
                 fill = colors[1], color = "white", alpha = 0.8) +
  geom_density(color = colors[2], size = 1.1) +
  annotate("text", 
           x = max(ai_ment_full$gdp_pc_latest, na.rm = TRUE) * 0.4, 
           y = max(density(ai_ment_full$gdp_pc_latest, na.rm = TRUE)$y) * 1.5, 
           label = summary_text, hjust = 0, size = 4.2, color = "grey40") +
  labs(title = "GDP per capita (USD)", 
       x = "GDP per capita", 
       y = "Density") +
  theme_classic(base_size = 12) +
  theme(panel.grid.major = element_line(color = "gray85", size = 0.3),
        panel.grid.minor = element_line(color = "gray90", size = 0.2),
        panel.background = element_blank())
# Log-transformed GDP per capita
p2 <- ggplot(ai_ment_full, aes(x = log10(gdp_pc_latest))) +
  geom_histogram(aes(y = ..density..), bins = 25, 
                 fill = colors[1], color = "white", alpha = 0.8) +
  geom_density(color = colors[2], size = 1.1) +
  labs(title = "log10(GPD per capita)", 
       x = expression(log[10](GDP~per~capita)), 
       y = "Density") +
  theme_classic(base_size = 13) +
  theme(panel.grid.major = element_line(color = "gray85", size = 0.3),
        panel.grid.minor = element_line(color = "gray90", size = 0.2),
        panel.background = element_blank())

# Combine
combined_plot <- p1 + p2 + plot_annotation(
  title = "Distribution of GDP per capita",
  subtitle = "Raw vs. log10(x)-transformed values with summary statistics",
  caption = "n = 64",
  theme = theme(plot.title = element_text(face = "bold", size = 13)),
)
combined_plot

# Median age

# Take stats
stats_age <- ai_ment_full %>%
  summarise(
    Mean   = mean(median_age_in_years, na.rm = TRUE),
    Median = median(median_age_in_years, na.rm = TRUE),
    SD     = sd(median_age_in_years, na.rm = TRUE),
    Min    = min(median_age_in_years, na.rm = TRUE),
    Max    = max(median_age_in_years, na.rm = TRUE)
  )

summary_text <- paste0(
  "Min = ", round(stats_age$Min, 1), "\n",
  "Median = ", round(stats_age$Median, 1), "\n",
  "Mean = ", round(stats_age$Mean, 1), "\n",
  "Max = ", round(stats_age$Max, 1), "\n",
  "SD = ", round(stats_age$SD, 1)
)


# position for the summary label
dens  <- density(ai_ment_full$median_age_in_years, na.rm = TRUE)
x_pos <- quantile(ai_ment_full$median_age_in_years, 0.80, na.rm = TRUE)
y_pos <- max(dens$y) * 0.90

# Plot
ggplot(ai_ment_full, aes(x = median_age_in_years)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = colors[1], color = "white", alpha = 0.85) +
  geom_density(color = colors[2], linewidth = 1.1) +
  geom_vline(xintercept = stats_age$Mean,   color = colors[2], linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = stats_age$Median, color = colors[3], linetype = "dotted", linewidth = 0.8) +
  annotate("label", x = x_pos, y = y_pos, label = summary_text, hjust = 0,
           size = 4, label.size = 0.25, fill = "white", color = "grey40") +
  labs(
    title = "Distribution of Median Age (years)",
    subtitle = "Supplemented with summary statistics",
    x = "Median age (years)",
    y = "Density",
    caption = "Dashed line = Mean; Dotted line = Median; n = 64"
  ) +
  theme_classic(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor = element_line(color = "gray90", linewidth = 0.2),
    panel.background = element_blank(),
    plot.title = element_text(face = "bold")
  )

# Modeling

# GDP + median_age -> ai_mention

m1.1 <- lm(log1p(ai_mention) ~ median_age_in_years, data = ai_ment_full)
m1.2 <- lm(log1p(ai_mention) ~ log10(gdp_pc_latest), data = ai_ment_full)
m1.3 <- lm(log1p(ai_mention) ~ log10(gdp_pc_latest) + median_age_in_years, data = ai_ment_full)
plot(m1.1)
plot(m1.2)
plot(m1.3)

summary(m1.1)
summary(m1.2)
summary(m1.3)


models <- list(m1.1 = m1.1, m1.2 = m1.2, m1.3 = m1.3)

results <- purrr::map_df(models, broom::tidy, .id = "Model") %>%
  mutate(Model = dplyr::recode(Model,
                        m1.1 = "Model 1.1: Median age",
                        m1.2 = "Model 1.2: GDP per capita",
                        m1.3 = "Model 1.3: Both predictors"))

results_clean <- results %>%
  mutate(term = dplyr::recode(term,
                       "(Intercept)" = "Intercept",
                       "median_age_in_years" = "Median age (years)",
                       "log10(gdp_pc_latest)" = "log10(GDP per capita)")) %>%
  mutate(estimate = round(estimate, 3),
         std.error = round(std.error, 3),
         statistic = round(statistic, 2),
         p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3)))

gt_table <- results_clean %>%
  dplyr::select(Model, term, estimate, std.error, statistic, p.value) %>%
  gt(groupname_col = "Model") %>%
  tab_header(
    title = md("**Predictors of AI-related Legislative Mentions**"),
    subtitle = "Ordinary Least Squares (OLS) regression models"
  ) %>%
  cols_label(
    term = "Variable",
    estimate = "Estimate (β)",
    std.error = "Std. Error",
    statistic = "t-value",
    p.value = "p-value"
  ) %>%
  fmt_number(
    columns = c(estimate, std.error),
    decimals = 3
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.font.size = 13,
    table.border.top.width = 2,
    table.border.bottom.width = 2,
    heading.align = "left"
  )

gt_table

model_summary <- purrr::map_df(models, glance, .id = "Model") %>%
  dplyr::select(Model, r.squared, adj.r.squared, statistic, p.value, df.residual) %>%
  mutate(across(where(is.numeric), round, 3))

model_summary %>%
  gt() %>%
  tab_header(title = md("**Model Fit Statistics for m1.1-m1.3**")) %>%
  cols_label(
    Model = "Model",
    r.squared = "R²",
    adj.r.squared = "Adj. R²",
    statistic = "F-statistic",
    p.value = "Model p-value",
    df.residual = "Residual DF"
  )

# ai_mention -> AIPI
m2 <- lm(AIPI_no_reg ~ log1p(ai_mention), data = ai_ment_full)
par(mfrow=c(2,2))
plot(m2)
summary(m2)

# Create a tidy table
m2_table <- broom::tidy(m2, conf.int = TRUE)

# Build a formatted gt table
m2_table %>%
  gt() %>%
  fmt_number(
    columns = c(estimate, std.error, statistic, p.value, conf.low, conf.high),
    decimals = 3
  ) %>%
  cols_label(
    term = "Variable",
    estimate = "Estimate (β)",
    std.error = "Std. Error",
    statistic = "t value",
    p.value = "p-value",
    conf.low = "95% CI (Lower)",
    conf.high = "95% CI (Upper)"
  ) %>%
  tab_header(
    title = md("**OLS regression results for AIPI_no_reg on log(1 + AI mentions)**"),
    subtitle = md("Dependent variable: *AIPI_no_reg*")
  ) 

#Plotting ai_ment vs. AIPI
ggplot(ai_ment_full, aes(x = log1p(ai_mention), y = AIPI_no_reg)) +
  geom_point(color = colors[1], size = 3, alpha = 0.8) +  # Dark2 green
  geom_smooth(method = "lm", se = TRUE, color = colors[2], fill = "#D95F0233", size = 1) +  # Dark2 orange
  labs(
    x = "log(1 + AI mentions in legislative proceedings, 2016–2022)",
    y = "AI Preparedness Index (without Regulation)",
    title = "Relationship between AI mentions and AIPI across countries",
    subtitle = "OLS fit with 95% confidence interval"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank()
  ) +
  annotate("text", x = 0.5, y = 0.55,
           label = "Higher frequency of AI mentions\n→ higher preparedness",
           color = "grey30", size = 3.5, hjust = 0)

# Full model

m3 <- lm(AIPI_no_reg ~ log1p(ai_mention) + log10(gdp_pc_latest) + median_age_in_years, data = ai_ment_full)
m3_wo_ai <- lm(AIPI_no_reg ~ log10(gdp_pc_latest) + median_age_in_years, data = ai_ment_full)

summary(m3)
par(mfrow=c(2,2))
plot(m3)
v <- vif(m3)

# Presenting vif
vif_tbl <- tibble(
  Variable = names(v),
  `VIF` = as.numeric(v),
  `1/VIF` = round(1 / v, 3)
) %>%
  mutate(
    `Interpretation` = case_when(
      VIF < 5 ~ "Low multicollinearity",
      VIF < 10 ~ "Moderate multicollinearity",
      TRUE ~ "High multicollinearity"
    ),
    VIF = round(VIF, 3)
  )

# Make gt table
vif_tbl %>%
  gt() %>%
  tab_header(
    title = md("**Variance Inflation Factors (VIF)**"),
    subtitle = md("Diagnostic test for multicollinearity in Model 3")
  ) %>%
  cols_label(
    Variable = "Variable",
    `VIF` = "VIF",
    `1/VIF` = "1/VIF",
    `Interpretation` = "Interpretation"
  ) %>%
  fmt_number(columns = c(`VIF`, `1/VIF`), decimals = 3) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(table.font.size = 12) %>%
  tab_source_note(
    md("**Note:** VIF < 5 indicates acceptable multicollinearity.")
  )

#Mediating model

med_df <- ai_ment_full %>%
  transmute(
    iso3,
    AIPI_no_reg,
    ai_mention   = log1p(ai_mention),
    gdp_pc_latest  = log10(gdp_pc_latest),
    median_age_in_years
  ) 

# Mediator model: M ~ X
m_mediator <- lm(ai_mention ~ gdp_pc_latest + median_age_in_years,
                 data = med_df)

# Outcome model: Y ~ M + X
m_outcome <- lm(AIPI_no_reg ~ ai_mention + gdp_pc_latest + median_age_in_years,
                data = med_df)

set.seed(123)  # fix the bootstrapped res

med_gdp <- mediate(model.m = m_mediator,
                   model.y = m_outcome,
                   treat = "gdp_pc_latest",
                   mediator = "ai_mention",
                   boot = TRUE,
                   sims = 1000)
summary(med_gdp)
med_gdp_summary <- summary(med_gdp)

med_age <- mediate(model.m = m_mediator,
                   model.y = m_outcome,
                   treat = "median_age_in_years",
                   mediator = "ai_mention",
                   boot = TRUE,
                   sims = 1000)

summary(med_age)
med_age_summary <- summary(med_age)

# Beta-regression /robustness check
betam<- betareg(AIPI_no_reg ~ log1p(ai_mention) + log10(gdp_pc_latest) + median_age_in_years, data = ai_ment_full)
summary(betam)

m3_tidy <- tidy(m3) %>%
  mutate(
    term = dplyr::recode(term,
                  "(Intercept)" = "Intercept",
                  "log1p(ai_mention)" = "AI mentions (log₁₊x)",
                  "log10(gdp_pc_latest)" = "GDP per capita (log₁₀)",
                  "median_age_in_years" = "Median age"),
    p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
  )

m3_tidy %>%
  dplyr::select(term, estimate, std.error, statistic, p.value) %>%
  gt() %>%
  fmt_number(columns = c(estimate, std.error, statistic), decimals = 3) %>%
  tab_header(
    title = md("**Full model OLS Regression Results: AIPI (No Regulation)**"),
    subtitle = "Predictors: AI mentions, GDP per capita, and median age"
  ) %>%
  cols_label(
    term = "Variable",
    estimate = "Estimate (β)",
    std.error = "Std. Error",
    statistic = "t-value",
    p.value = "p-value"
  ) %>%
  tab_source_note(
    md("**Notes:** Dependent variable is *AIPI_no_reg*.  
    R² = 0.864, Adjusted R² = 0.857.")
  ) %>%
  tab_options(
    table.font.size = 12,
    data_row.padding = px(5),
    heading.title.font.size = 14,
    heading.subtitle.font.size = 12
  )

# Glance fit stats
fit <- bind_rows(
  glance(m3_wo_ai) %>% mutate(Model = "m3_base: GDPpc + Age"),
  glance(m3)       %>% mutate(Model = "m3_full")
) %>%
  dplyr::select(Model, r.squared, adj.r.squared, AIC, BIC, statistic, p.value, df.residual, nobs)

# Delta row (full − base)
delta <- fit %>%
  summarise(
    Model = "Δ (m3_full − m3_base)",
    r.squared     = diff(r.squared),
    adj.r.squared = diff(adj.r.squared),
    AIC           = diff(AIC),
    BIC           = diff(BIC),
    statistic     = NA_real_,
    p.value       = NA_real_,
    df.residual   = diff(df.residual),
    nobs          = unique(nobs)
  )

fit_tbl <- bind_rows(fit, delta) %>%
  mutate(
    across(c(r.squared, adj.r.squared), ~round(.x, 4)),
    across(c(AIC, BIC), ~round(.x, 1)),
    statistic = ifelse(is.na(statistic), NA, round(statistic, 2)),
    p.value   = ifelse(is.na(p.value), NA, ifelse(p.value < 0.001, "<0.001", round(p.value, 3)))
  )

# Partial F-test
cmp <- anova(m3_wo_ai, m3)
pf_F  <- round(cmp$`F`[2], 2)
pf_p  <- cmp$`Pr(>F)`[2]
pf_p_fmt <- ifelse(is.na(pf_p), NA, ifelse(pf_p < 0.001, "<0.001", sprintf("%.3f", pf_p)))

# Build gt table
fit_tbl %>%
  gt() %>%
  tab_header(
    title = md("**Model Fit Comparison: With vs. Without AI Mentions**"),
    subtitle = md("Dependent variable: *AIPI_no_reg*")
  ) %>%
  cols_label(
    Model = "Model",
    r.squared = "R²",
    adj.r.squared = "Adj. R²",
    AIC = "AIC",
    BIC = "BIC",
    statistic = "F-stat",
    p.value = "Model p",
    df.residual = "Residual DF",
    nobs = "N"
  ) %>%
  fmt_missing(everything(), missing_text = "") %>%
  tab_source_note(
    md(paste0("Partial F-test (m3_full vs. m3_base): **F = ", pf_F, "**, p = **", pf_p_fmt, "**"))
  ) %>%
  tab_source_note(md("Δ (delta) row shows difference between m3_full and m3_base model (w/o ai mention). Positive ΔR² demonstrated additional explanatory power from AI mentions."))

# Presenting mediation tests
# Extract results
med_gdp_tbl <- data.frame(
  Effect = c("ACME (indirect)", "ADE (direct)", "Total Effect", "Prop. Mediated"),
  Estimate = c(0.0130, 0.1129, 0.1260, 0.1035),
  CI_Lower = c(0.0006, 0.0887, 0.1014, 0.0052),
  CI_Upper = c(0.0308, 0.1396, 0.1528, 0.2310),
  p_value = c(0.038, "<0.001", "<0.001", 0.038)
)

med_age_tbl <- data.frame(
  Effect = c("ACME (indirect)", "ADE (direct)", "Total Effect", "Prop. Mediated"),
  Estimate = c(0.00005, 0.00262, 0.00267, 0.0202),
  CI_Lower = c(-0.00074, 0.00070, 0.00080, -0.379),
  CI_Upper = c(0.00082, 0.00438, 0.00439, 0.369),
  p_value = c(0.892, 0.002, 0.006, 0.890)
)

# Combine the two
combined_tbl <- med_gdp_tbl %>%
  rename_with(~paste0(., "_GDP"), -Effect) %>%
  inner_join(
    med_age_tbl %>%
      rename_with(~paste0(., "_Age"), -Effect),
    by = "Effect"
  )

# Format with gt
combined_tbl %>%
  gt() %>%
  tab_header(
    title = md("**Causal Mediation Analysis Results**"),
    subtitle = "Comparing indirect and direct effects for GDP per capita and Median age"
  ) %>%
  cols_label(
    Effect = "Effect Type",
    Estimate_GDP = "Estimate (GDP)",
    CI_Lower_GDP = "95% CI Low (GDP)",
    CI_Upper_GDP = "95% CI High (GDP)",
    p_value_GDP = "p-value (GDP)",
    Estimate_Age = "Estimate (Age)",
    CI_Lower_Age = "95% CI Low (Age)",
    CI_Upper_Age = "95% CI High (Age)",
    p_value_Age = "p-value (Age)"
  ) %>%
  fmt_number(columns = matches("Estimate|CI"), decimals = 4) %>%
  tab_source_note(md("**ACME** = Average Causal Mediation Effect; **ADE** = Average Direct Effect.  
  Significant indirect effect (p < .05) observed only for GDP per capita."))

# Presenting beta-regression
# Extract details
s   <- summary(betam)
phi <- s$phi

# Prepare coefficient table
coef_tbl <- as.data.frame(s$coefficients$mean) %>%
  rownames_to_column("term") %>%
  mutate(block = "Mean model (logit link)") %>%
  bind_rows(
    as.data.frame(s$coefficients$precision) %>%
      rownames_to_column("term") %>%
      mutate(block = "Precision (φ)")
  ) %>%
  rename(
    Estimate = Estimate,
    `Std. Error` = `Std. Error`,
    `z value` = `z value`,
    `p-value` = `Pr(>|z|)`
  ) %>%
  mutate(
    term = dplyr::recode(term,
                  "(Intercept)"          = "Intercept",
                  "log1p(ai_mention)"    = "AI mentions (log₁₊x)",
                  "log10(gdp_pc_latest)" = "GDP per capita (log₁₀)",
                  "median_age_in_years"  = "Median age",
                  "(phi)"                = "φ (precision)"
    ),
    across(c(Estimate, `Std. Error`, `z value`), ~round(.x, 3)),
    `p-value` = ifelse(`p-value` < 0.001, "<0.001", sprintf("%.3f", `p-value`))
  )

# Create gt table with fit stats in footer
coef_tbl %>%
  gt(groupname_col = "block") %>%
  tab_header(
    title = md("**Beta Regression Results: AIPI (No Regulation)**"),
    subtitle = md("Mean model calculated in logit manner; precision reported as φ")
  ) %>%
  cols_label(
    term       = "Variable",
    Estimate   = "Estimate",
    `Std. Error` = "Std. Error",
    `z value`  = "z",
    `p-value`  = "p"
  ) %>%
  fmt_number(columns = c(Estimate, `Std. Error`, `z value`), decimals = 3) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(table.font.size = 12) %>%
  tab_source_note(
    md(
      paste0(
        "**Model fit:** Pseudo R² = ", round(s$pseudo.r.squared, 3),
        "; Log-likelihood = ", round(as.numeric(logLik(betam)), 1),
        "; N = ", nobs(betam),
        "; φ (precision) = ", round(phi, 2), "."
      )
    )
  )

ggplot(ai_ment_full, aes(x = log1p(ai_mention), y = AIPI_no_reg)) +
  geom_point(aes(size = median_age_in_years, color = log10(gdp_pc_latest)), alpha = 0.9) +
  geom_smooth(method = "lm", se = TRUE, color = colors[2], linewidth = 0.8) +
  geom_text(aes(label = iso3), vjust = -1, size = 3, check_overlap = TRUE) +
  scale_color_distiller(palette = "YlGn", direction = 1, name = "GDP per capita\n(log10)") +
  scale_size_continuous(name = "Median age\n(in years)") +
  labs(
    x = "Mentions of AI in legislative proceedings (log(1+x)-transformed)",
    y = "AI Preparedness Index (no regulatory component)",
    title = "M3 model representing plot incl. Age, GDPpc, AI mention, AIPI"
  ) +
  guides(
    size = guide_legend(order = 1),
    color = guide_colorbar(order = 2)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

save(ai_ment_full, file = "Main df for analysis.rds")