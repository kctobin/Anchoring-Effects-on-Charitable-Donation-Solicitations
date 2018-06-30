require(MTurkR)

qualtrics.clean = ".//clean_data//qualtrics_data.csv"

# Load AWS mTurk credentials
source("aws_keys.r")

# Reads full access key for payments if the file exists - file isn't in git repo so should be created if
# needed based on aws_keys.r file, just with different keys

if (file.exists("aws_full_keys.r")) {
  source("aws_full_keys.r")
}

options("MTurkR.sandbox" = FALSE)

#Get latest hit by CreationTime
hits = SearchHITs(sortproperty = "CreationTime")$HITs

#33KGGVH24TGRJE5J0BP49LTQK691XU
#3SV8KD29L3RIUARK93J7BL5016FKZ9

current_hit = '3TKSOBLOHKFLJ9KAXCQBQ0F7PVSBBS'
current_p = 'p5'

# Get assignment from hits (will pull for multiple hits if there is more than one in 'hits')
hitassignments <- GetAssignments(hit = current_hit,
                                 return.all = TRUE)


hitassignments$surveycode = as.character(hitassignments$surveycode)
hitassignments$surveycode = sapply(strsplit(hitassignments$surveycode, " "), "[[", 1)

# Read in qualtrics data
survey_data = read.csv(qualtrics.clean, stringsAsFactors = FALSE, na.strings = c(""))
survey_data = survey_data[survey_data$sid == current_p,]

#check for non matches to fix manually 
mismatches = survey_data[!(survey_data$MTurkCode %in% hitassignments$surveycode), ]
mismatches_mturk = hitassignments[!(hitassignments$surveycode %in% survey_data$MTurkCode), ]

#p3 manual fix
if (current_p == 'p3') {
  hitassignments[hitassignments$AssignmentId == '3CPLWGV3MOY7J9R4SKMQF9FX9LMN9A', ]$surveycode = '82119' 
}
if(current_p == 'p5') {
  hitassignments[hitassignments$AssignmentId == '340UGXU9DY0IB6OGSM38LMWOGSEVUG', ]$surveycode = '85811' 
}

# current responses are those with matching code
current_assignments = merge(survey_data, hitassignments, by.x = "MTurkCode", by.y = "surveycode")
current_assignments$bonus_reason = ifelse(current_assignments$donate_amount > 0,
                                          "Thank you for your participation in our HIT of proofreading services.

We also want to thank you on behalf of the Against Malaria Foundation for your donation. When the HIT is complete we will donate your selected portion of the bonus along with others anonymously to aid in their distribution of nets to the at risk population."
                                          ,"Thank you for your participation in our HIT of proofreading services.")

# Calculate total amount needed for bonuses
bonus_sum = sum(current_assignments$bonus_amount) * 1.2
donation_sum = sum(current_assignments$donate_amount)

# Is there certain feedback we want to provide?
ApproveAssignment(assignments = current_assignments$AssignmentId, feedback = "Thank you for your participation in our HIT of proofreading services.")

# Check that we haven't already paid a bonus
paid_bonuses = GetBonuses(hit = current_hit)
pay_bonuses = current_assignments[!(current_assignments$AssignmentId %in% paid_bonuses$AssignmentId), ]

GrantBonus(workers = pay_bonuses$WorkerId,
           assignments = pay_bonuses$AssignmentId,
           amounts = pay_bonuses$bonus_amount, 
           reasons = pay_bonuses$bonus_reason)
# Add users to qualification to prevent them from retaking future surveys

w <- hitassignments$WorkerId

AssignQualification(
  qual = "3CFGE88WF7ST2K2BKVJ2KM4YWET4S0",
  workers = w,
  value = "50")

