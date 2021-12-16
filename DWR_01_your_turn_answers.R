# DWR_01 your turn answers

# YOUR TURN #1
# VA DOE: Public high school graduate and completer data for 2006 - 2016.
# http://www.doe.virginia.gov/statistics_reports/research_data/index.shtml

# Read in all CSV files and bind into one data frame called "grads_df".

setwd("../doe")
grads <- list.files()

grads_ls <- lapply(grads, read_csv)
grads_df <- bind_rows(grads_ls)

# YOUR TURN #2

# 1) Merge the grads_df_2016_2017 and va_schools data frames based on Division
# and School Number such that all rows from grads_df_2016_2017 are retained.
# Save the new data frame as va2016_2017
va2016_2017 <- left_join(grads_df_2016_2017, va_schools, 
                         by = c("DIV_NUM" = "DivNo", 
                                "SCH_NUM" = "SchoolNo"))


# YOUR TURN #3

# The file "mhp.csv" contains data on the number of patients in mental hospitals
# (in thousands), by type of hospital, from 1904-1970. Each year's data was
# estimated in July.

mhp <- read.csv("mhp.csv")
head(mhp)

# Reshape mhp to have three columns: Year, Hospital.Type, and Number.Patients.
# Save the new data frame as "mhp_long". The first few rows of mhp_long should
# look like this:

#   Year Hospital.Type Number.Patients
# 1 1923       Federal              29
# 2 1931       Federal              12
# 3 1933       Federal              19


mhp_long <- gather(mhp, key = HospitalType, 
                  value = NumberPatients, -Year)





# Extra -------------------------------------------------------------------




# subset va2016_2017 so that we have non-missing records for "SCH_NUM",
# "SCH_NAME", "FEDERAL_RACE_CODE", "GENDER", "DISABILITY_FLAG" "LEP_FLAG"
# "DISADVANTAGED_FLAG", and "HS_COMPLETION_NUM" (columns 5-12)
c.out <- sapply(va2016_2017[,5:12],function(x)!is.na(x))
keep <- apply(c.out,1,all)
va2016_2017_complete <- va2016_2017[keep,]

# label the race code
va2016_2017_complete$FEDERAL_RACE_CODE <- factor(va2016_2017_complete$FEDERAL_RACE_CODE, 
                                                 labels = c("Asian","Black or African/American",
                                                            "Hispanic of any race", "White",
                                                            "Two or more races, non-Hispanic"))

