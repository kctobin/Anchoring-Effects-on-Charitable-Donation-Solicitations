library(dplyr)

bonus_amt = 1.2
qualtrics.raw = ".//raw_data//241final.csv"
qualtrics.data = read.csv(qualtrics.raw, stringsAsFactors = FALSE, na.strings = c(""))

# Convert dates from string to POSTIX
qualtrics.tf = "%Y-%m-%d %H:%M:%S"
qualtrics.tz = "US/Mountain"

qualtrics.data$StartDate = as.POSIXct(qualtrics.data$StartDate, tz = qualtrics.tz, format = qualtrics.tf, origin = "1970-01-01")
qualtrics.data$EndDate = as.POSIXct(qualtrics.data$EndDate, tz = qualtrics.tz, format = qualtrics.tf, origin = "1970-01-01")

# Remove rows with metadata which now have NA for start date
qualtrics.data = qualtrics.data[!is.na(qualtrics.data$StartDate), ]

# Calculate bonus amounts
qualtrics.data$donate_perc = as.integer(ifelse(qualtrics.data$Q16 == "Yes, I'd like to donate a different percentage of my bonus",
                                    gsub(".*\\.","",qualtrics.data$Q16_2_TEXT),
                                    ifelse(qualtrics.data$Q16 == "Yes, I'd like to donate the average amount",
                                            qualtrics.data$amt,
                                            0)
))
qualtrics.data[is.na(qualtrics.data$donate_perc),]$donate_perc = 0
qualtrics.data = transform(qualtrics.data, donate_perc = as.numeric(donate_perc))

qualtrics.data$donate_amount = round(bonus_amt * (qualtrics.data$donate_perc / 100), 2)
qualtrics.data$bonus_amount = bonus_amt - qualtrics.data$donate_amount

qualtrics.data$donate = qualtrics.data$donate_perc>0
qualtrics.data$proofread = ifelse(!is.na(qualtrics.data$Q17_First.Click) &
                                    !is.na(qualtrics.data$Q17_Page.Submit),
                                  as.numeric(qualtrics.data$Q17_Page.Submit)-
                                    as.numeric(qualtrics.data$Q17_First.Click),
                                  "NA")
qualtrics.data$Q10 = gsub("[\n]", " ", qualtrics.data$Q10)

qualtrics.data$grammar.score = 0
a = qualtrics.data$Q10
qualtrics.data$grammar.score[grep("African", a, ignore.case = T)] = 
  qualtrics.data$grammar.score[grep("African", a, ignore.case = T)]+1

patterns1 = c("It's","Its")
qualtrics.data$grammar.score[grep(paste(patterns1,collapse = "|"), a, ignore.case = T)] = 
  qualtrics.data$grammar.score[grep(paste(patterns1,collapse = "|"), a, ignore.case = T)]+1

patterns2 = c("insecticide", "insectiside")
qualtrics.data$grammar.score[grep(paste(patterns2,collapse = "|"), a, ignore.case = T)] = 
  qualtrics.data$grammar.score[grep(paste(patterns2,collapse = "|"), a, ignore.case = T)]+1

patterns3 = c("counts", "count")
qualtrics.data$grammar.score[grep(paste(patterns3,collapse = "|"), a, ignore.case = T)] = 
  qualtrics.data$grammar.score[grep(paste(patterns3,collapse = "|"), a, ignore.case = T)]+1

patterns4 = c(".!", "exclamation", "!")
qualtrics.data$grammar.score[grep(paste(patterns4,collapse = "|"), a, ignore.case = T)] = 
  qualtrics.data$grammar.score[grep(paste(patterns4,collapse = "|"), a, ignore.case = T)]+1

# Added rank variable - only those with rank = 1 should be used for analysis
# to remove people's second takes 
qualtrics.data = qualtrics.data %>% group_by(IPAddress) %>%
  mutate(rank = rank(StartDate)) %>%
  arrange(StartDate)


write.csv(qualtrics.data, ".//clean_data//qualtrics_data.csv")
