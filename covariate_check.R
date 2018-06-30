# Load data
data <- read.csv("./clean_data/qualtrics_data.csv")

# Rename columns
names(data) <- c("x", "start_date", "end_data", "status", "ip_address", "progress", "duration_secs", 
               "finished", "recorded_date", "response_id", "last_name", "first_name",
               "email", "reference", "latitude", "longitude", "distribution_channel",
               "age", "gender", "state", "income", "prev_donate", "employment_status", "grammar",
               "first_click", "last_click", "page_submit", "num_clicks", "donate", "custom_donation",
               "donation_notification", "sid", "mturkcode", "group", "amt", "topics", "donate_perc",
               "donate_amount", "bonus_amount", "donate_yn", "proofread", "grammar_score", "rank")

# Set data.table
library(data.table)
setDT(data)
data[,mean(donate_perc), by = group]

# Create most common function
most.common <- function(x) {
  count <- sapply(unique(x), function(i) sum(x==i, na.rm=TRUE))
  unique(x)[which(count==max(count))]
}

# Covariate balance table (needs work)
data_with_groups <- subset(data, group == 1 | group == 2 | group == 3)
summary.table <- data_with_groups[ , .(
                        proofread = mean(proofread, na.rm = TRUE), 
                        gender = mean(gender == "Male", na.rm = TRUE),
                        age = mean(age), 
                        income = most.common(income),
                        prev_donate = mean(prev_donate == "Yes", na.rm = TRUE),
                        grammar_score = mean(grammar_score)
                        ), 
                    by = group]

library(stargazer)
stargazer(summary.table[c(1,2,5)], summary = FALSE, header = FALSE, title = "Summary Table of Key Covariates", digits = 2, type="text", omit="3")

# AOV Test
study1 <- subset(data, sid =="p2" | sid == "p3" | sid == "p4")
study1 <- study1[rank==1,]

group1 <- subset(study1, group == "1")
group2 <- subset(study1, group == "2")
group3 <- subset(study1, group == "3")
  
fit_g1 <- aov(donate_perc ~ proofread + gender + age + income + prev_donate + grammar_score + donate_yn, data = group1)
fit_g2 <- aov(donate_perc ~ proofread + gender + age + income + prev_donate + grammar_score + donate_yn, data = group2)
fit_g3 <- aov(donate_perc ~ proofread + gender + age + income + prev_donate + grammar_score + donate_yn, data = group3)

summary(fit_g1)
summary(fit_g2)
summary(fit_g3)

# Regression
regress_s1 <- lm(donate_perc ~ factor(group), data = study1)
regress_s2 <- lm(donate_perc ~ proofread + gender + age + factor(income) + prev_donate + grammar_score + factor(group), data = study1)
regress_s3 <- lm(donate_yn ~ factor(group), data = study1)
regress_s4 <- lm(donate_yn ~ proofread + gender + age + factor(income) + prev_donate + grammar_score + factor(group), data = study1)

stargazer(regress_s1, regress_s2, regress_s3, regress_s4, summary = FALSE, header = FALSE, title = "Regression", digits = 2, type="text",
          omit = "income")
