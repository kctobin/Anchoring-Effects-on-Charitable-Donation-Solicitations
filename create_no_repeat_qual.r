require(MTurkR)

source("aws_keys.r")

thenewqual <-
  CreateQualificationType(
    name="Qualification to Prevent Retakes",
    description="This qualification is for people who have worked for me on this task before.",
    status = 'Active',
    keywords="Worked for me before",
    auto = FALSE)


