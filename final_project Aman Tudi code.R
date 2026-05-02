



if (!require(ggplot2)) {
  install.packages("ggplot2", repos = "https://cloud.r-project.org")
  library(ggplot2)
}

if (file.exists("adult-all.csv")) {
  filepath <- "adult-all.csv"
} else if (file.exists("adult-all (1).csv")) {
  filepath <- "adult-all (1).csv"
} else if (file.exists("adult-all__1_.csv")) {
  filepath <- "adult-all__1_.csv"
} else {
  # If none of those work, let the user pick the file manually
  cat("Could not find the CSV file automatically.\n")
  cat("A file picker window will open -- please select your CSV file.\n")
  filepath <- file.choose()
}

cat("Loading data from:", filepath, "\n")
# Read in the CSV file
adult <- read.csv(filepath,
                  header = FALSE,
                  stringsAsFactors = FALSE,
                  strip.white = TRUE)

# Add column names

colnames(adult) <- c("age", "workclass", "fnlwgt", "education", "education_num",
                     "marital_status", "occupation", "relationship", "race",
                     "sex", "capital_gain", "capital_loss", "hours_per_week",
                     "native_country", "income")


cat("\n--- Data Structure ---\n")
str(adult)
cat("\nFirst 6 rows:\n")
print(head(adult))




cat("\n--- Missing values per column ---\n")
print(colSums(adult == "?"))


adult_clean <- adult[adult$workclass != "?" &
                     adult$occupation != "?" &
                     adult$native_country != "?", ]

cat("\nRows before cleaning:", nrow(adult), "\n")
cat("Rows after cleaning:", nrow(adult_clean), "\n")


# 1 = earns more than $50K, 0 = earns $50K or less
adult_clean$high_income <- ifelse(adult_clean$income == ">50K", 1, 0)
table(adult_clean$high_income)

# "Degree" = Bachelors, Masters, Doctorate, or Professional school
# "No Degree" = everything else
adult_clean$has_degree <- ifelse(
  adult_clean$education %in% c("Bachelors", "Masters", "Doctorate", "Prof-school"),
  "Degree",
  "No Degree"
)
table(adult_clean$has_degree)



adult_clean$education_group <- NA

adult_clean$education_group[adult_clean$education %in%
  c("Preschool", "1st-4th", "5th-6th", "7th-8th",
    "9th", "10th", "11th", "12th")] <- "Below HS"

adult_clean$education_group[adult_clean$education == "HS-grad"] <- "High School"

adult_clean$education_group[adult_clean$education %in%
  c("Some-college", "Assoc-voc", "Assoc-acdm")] <- "Some College"

adult_clean$education_group[adult_clean$education == "Bachelors"] <- "Bachelors"
adult_clean$education_group[adult_clean$education == "Masters"] <- "Masters"

adult_clean$education_group[adult_clean$education %in%
  c("Doctorate", "Prof-school")] <- "Doctorate/Prof"

# Set the order for charts (low to high education)
adult_clean$education_group <- factor(
  adult_clean$education_group,
  levels = c("Below HS", "High School", "Some College",
             "Bachelors", "Masters", "Doctorate/Prof")
)
table(adult_clean$education_group)



cat("\n--- Overall Income Distribution ---\n")
income_table <- table(adult_clean$income)
print(income_table)
cat("Percentages:\n")
print(round(prop.table(income_table) * 100, 1))


cat("\n--- % Earning >50K by Education Group ---\n")
edu_pct <- tapply(adult_clean$high_income, adult_clean$education_group, mean) * 100
edu_pct <- round(edu_pct, 1)
print(edu_pct)


cat("\n--- Degree vs. No Degree ---\n")
degree_pct <- tapply(adult_clean$high_income, adult_clean$has_degree, mean) * 100
degree_pct <- round(degree_pct, 1)
print(degree_pct)


edu_count <- table(adult_clean$education_group)
edu_hours <- tapply(adult_clean$hours_per_week, adult_clean$education_group, mean)


summary_table <- data.frame(
  Education_Group = names(edu_pct),
  Count = as.numeric(edu_count),
  Pct_Earning_Over_50K = as.numeric(edu_pct),
  Avg_Hours_Per_Week = round(as.numeric(edu_hours), 1)
)
cat("\n--- Summary Table ---\n")
print(summary_table)
write.csv(summary_table, "summary_table.csv", row.names = FALSE)
cat("Saved: summary_table.csv\n")




edu_plot_data <- data.frame(
  education_group = factor(names(edu_pct),
                           levels = c("Below HS", "High School", "Some College",
                                      "Bachelors", "Masters", "Doctorate/Prof")),
  pct = as.numeric(edu_pct)
)

fig1 <- ggplot(edu_plot_data, aes(x = education_group, y = pct,
                                   fill = education_group)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.5, size = 3.5,
            fontface = "bold") +
  scale_fill_brewer(palette = "Blues", direction = 1) +
  scale_y_continuous(limits = c(0, 80)) +
  labs(title = "Percentage Earning >$50K by Education Level",
       subtitle = "Higher education is strongly associated with higher income",
       x = "Education Level",
       y = "% Earning >$50K") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

print(fig1)
ggsave("fig1_education_income.png", fig1, width = 9, height = 5.5, dpi = 300)
cat("\nSaved: fig1_education_income.png\n")




fig2 <- ggplot(adult_clean, aes(x = has_degree, fill = income)) +
  geom_bar(position = "fill", width = 0.6) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("<=50K" = "#B0C4DE", ">50K" = "#1B4F72"),
                    name = "Income") +
  labs(title = "Income Distribution: Degree vs. No Degree",
       subtitle = "Degree holders are far more likely to earn above $50K",
       x = "", y = "Proportion") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

print(fig2)
ggsave("fig2_degree_comparison.png", fig2, width = 7, height = 5, dpi = 300)
cat("Saved: fig2_degree_comparison.png\n")




occ_pct <- tapply(adult_clean$high_income, adult_clean$occupation, mean) * 100
occ_count <- table(adult_clean$occupation)

occ_data <- data.frame(
  occupation = names(occ_pct),
  pct = round(as.numeric(occ_pct), 1),
  count = as.numeric(occ_count)
)

occ_data <- occ_data[occ_data$count >= 100, ]
occ_data <- occ_data[order(-occ_data$pct), ]
occ_data <- head(occ_data, 10)

fig3 <- ggplot(occ_data, aes(x = reorder(occupation, pct), y = pct)) +
  geom_col(fill = "#1B4F72", width = 0.7) +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.1, size = 3.5,
            fontface = "bold") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 80)) +
  labs(title = "Top 10 Occupations by Percentage Earning >$50K",
       subtitle = "Executive and professional roles dominate high earners",
       x = "", y = "% Earning >$50K") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(fig3)
ggsave("fig3_occupation_income.png", fig3, width = 9, height = 5.5, dpi = 300)
cat("Saved: fig3_occupation_income.png\n")


top_occs <- c("Exec-managerial", "Prof-specialty", "Tech-support",
              "Sales", "Protective-serv")

sub <- adult_clean[adult_clean$occupation %in% top_occs, ]

fig4_data <- aggregate(high_income ~ occupation + has_degree, data = sub, mean)
fig4_data$pct <- round(fig4_data$high_income * 100, 1)

fig4 <- ggplot(fig4_data, aes(x = occupation, y = pct, fill = has_degree)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(pct, "%")),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3.2) +
  scale_fill_manual(values = c("Degree" = "#1B4F72", "No Degree" = "#B0C4DE"),
                    name = "Education") +
  scale_y_continuous(limits = c(0, 75)) +
  labs(title = "Income by Degree Status Within Top Occupations",
       subtitle = "Even within the same occupation, degree holders earn more",
       x = "", y = "% Earning >$50K") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

print(fig4)
ggsave("fig4_occupation_degree.png", fig4, width = 10, height = 5.5, dpi = 300)
cat("Saved: fig4_occupation_degree.png\n")



fig5 <- ggplot(adult_clean, aes(x = education_group, y = hours_per_week,
                                 fill = income)) +
  geom_boxplot(outlier.alpha = 0.2) +
  scale_fill_manual(values = c("<=50K" = "#B0C4DE", ">50K" = "#1B4F72"),
                    name = "Income") +
  labs(title = "Hours Worked Per Week by Education Level and Income",
       subtitle = "Higher earners work more hours, but education still matters",
       x = "Education Level", y = "Hours Per Week") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

print(fig5)
ggsave("fig5_hours_education.png", fig5, width = 10, height = 5.5, dpi = 300)
cat("Saved: fig5_hours_education.png\n")




model1 <- glm(high_income ~ education_num,
              data = adult_clean,
              family = binomial)

cat("\n===== MODEL 1: Education Only =====\n")
summary(model1)

odds_ratio <- round(exp(coef(model1)["education_num"]), 3)
pct_increase <- round((odds_ratio - 1) * 100, 1)
cat("\nOdds ratio:", odds_ratio, "\n")
cat("Each extra year of education increases odds of earning >50K by",
    pct_increase, "%\n")


model2 <- glm(high_income ~ education_num + age + sex + hours_per_week + race,
              data = adult_clean,
              family = binomial)

cat("\n===== MODEL 2: Education + Controls =====\n")
summary(model2)

cat("\nOdds ratios for Model 2:\n")
print(round(exp(coef(model2)), 3))



model3 <- glm(high_income ~ has_degree + age + sex + hours_per_week,
              data = adult_clean,
              family = binomial)

cat("\n===== MODEL 3: Degree vs. No Degree =====\n")
summary(model3)

odds_no_degree <- round(exp(coef(model3)["has_degreeNo Degree"]), 3)
cat("\nOdds ratio for No Degree:", odds_no_degree, "\n")
cat("People without a degree have only", odds_no_degree,
    "times the odds of earning >50K compared to degree holders.\n")




pred_data <- data.frame(education_num = 1:16)
pred_data$predicted_prob <- predict(model1, newdata = pred_data, type = "response")

cat("\n--- Predicted Probabilities ---\n")
print(pred_data)


fig6 <- ggplot(pred_data, aes(x = education_num, y = predicted_prob)) +
  geom_line(color = "#1B4F72", linewidth = 1.5) +
  geom_point(color = "#1B4F72", size = 3) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.85)) +
  scale_x_continuous(breaks = 1:16) +
  labs(title = "Predicted Probability of Earning >$50K by Years of Education",
       subtitle = "Based on logistic regression (Model 1)",
       x = "Years of Education",
       y = "Predicted Probability") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(fig6)
ggsave("fig6_predicted_probability.png", fig6, width = 9, height = 5.5, dpi = 300)
cat("Saved: fig6_predicted_probability.png\n")


