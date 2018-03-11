library(dplyr)
library(tidyr)
library(readr)

sc <- read_csv("C:/Users/Curtis/Desktop/rcode/college_scorecard/Data/CollegeScorecard_Raw_Data/MERGED2014_15_PP.csv")


# Filter main campuses
scorecard_main <- sc %>% filter(sc$MAIN == 1)

# Eliminate columns corresponding to zipcode, location coordinates, region, school urls, accrediting agency,
# and not under investigation
scorecard_main$NPCURL = NULL
scorecard_main$INSTURL = NULL
scorecard_main$ZIP = NULL
scorecard_main$LATITUDE = NULL
scorecard_main$LONGITUDE = NULL
scorecard_main$REGION = NULL
scorecard_main$LOCALE = NULL
scorecard_main$ACCREDAGENCY = NULL
scorecard_main$HCM2 = NULL
scorecard_main$ALIAS = NULL

# Focus on institutions that offer Associate degree or higher
scorecard <- scorecard_main %>% filter(scorecard_main$HIGHDEG > 1,
                                     scorecard_main$CURROPER == 1)

# Disregard historically minority serving or single-gender only institutions
scorecard_2 <- scorecard %>% filter(scorecard$HBCU == 0 &
                                   scorecard$PBI == 0 &
                                   scorecard$ANNHI == 0 &
                                   scorecard$TRIBAL == 0 &
                                   scorecard$AANAPII == 0 &
                                   scorecard$HSI == 0 &
                                   scorecard$NANTI == 0 &
                                   scorecard$MENONLY == 0 &
                                   scorecard$WOMENONLY == 0)

# Eliminate all but Associates or Bachelor's degree programs
filter_assoc_bach <- grepl("CERT", colnames(scorecard_2))
assoc_bach <- scorecard_2[!filter_assoc_bach]


# Institution identifiers
group <- assoc_bach[,1:4]


y <- substr(year, 7, 13)

# School information data frame
school_df <- assoc_bach[,grep("UNITID",colnames(assoc_bach)):grep("RELAFFIL",colnames(assoc_bach))]

school_df <- cbind(school_df, assoc_bach$PFTFTUG1_EF)

school_df <- cbind(school_df, assoc_bach$ICLEVEL)

write.csv(school_df, "C:/Users/Curtis/Desktop/rcode/college_scorecard/Data/2014_15/school_df.csv")

# Admissions data frame
admissions_info <- assoc_bach %>% select(UNITID:INSTNM,
                                          ADM_RATE:SAT_AVG_ALL)

admissions_df <- cbind(group, admissions_info)

write.csv(school_df, "C:/Users/Curtis/Desktop/rcode/college_scorecard/Data/2014_15/admissions_df.csv")


# Academics data frame
academics_info <- assoc_bach %>% select(PCIP01:CIP54BACHL)

academics_df <- cbind(group, academics_info)
  
write.csv(school_df, "C:/Users/Curtis/Desktop/rcode/college_scorecard/Data/2014_15/academics_df.csv")


# Student data frame
student_info <- assoc_bach %>% select(UGDS:PPTUG_EF2,
                                      RET_FT4:RET_PTL4,
                                      UG25ABV,
                                      INC_PCT_LO:IND_INC_AVG,
                                      INC_N:APPL_SCH_N,
                                      PELL_EVER:FSEND_5,
                                      UGDS_MEN:UGDS_WOMEN)

student_df <- cbind(group, student_info)

write.csv(school_df, "C:/Users/Curtis/Desktop/rcode/college_scorecard/Data/2014_15/student_df.csv")


# Finance data frame
financial_info <- assoc_bach %>% select(NPT4_PUB:PCTPELL,
                                        PCTFLOAN,
                                        CDR2:CDR3,
                                        RPY_1YR_RT:NOTFIRSTGEN_RPY_7YR_RT,
                                        DEBT_MDN:CUML_DEBT_P10,
                                        REPAY_DT_MDN,
                                        REPAY_DT_N,
                                        RPY_1YR_N:NOTFIRSTGEN_RPY_7YR_N,
                                        LOAN_EVER,
                                        DEBT_MDN_SUPP:NOTFIRSTGEN_RPY_3YR_RT_SUPP,
                                        CDR2_DENOM:CDR3_DENOM)

financial_df <- cbind(group, financial_info)
  
write.csv(school_df, "C:/Users/Curtis/Desktop/rcode/college_scorecard/Data/2014_15/financial_df.csv")
  
# Completion data frame
completion_info <- assoc_bach %>% select(C150_4:POOLYRS,
                                         D150_4:C150_L4_HISPOLD,
                                         DEATH_YR2_RT:NOT1STGEN_UNKN_2YR_TRANS_YR8_RT,
                                         OVERALL_YR2_N:NOT1STGEN_YR8_N,
                                         SEPAR_DT_MDN,
                                         SEPAR_DT_N,
                                         C150_L4_POOLED_SUPP:DTRANS_L4)

completion_df <- cbind(group, completion_info)
  
write.csv(school_df, "C:/Users/Curtis/Desktop/rcode/college_scorecard/Data/2014_15/completion_df.csv")


# Earings data frame
earnings_info <- assoc_bach %>% select(COUNT_ED,
                                        COUNT_NWNE_P10:GT_25K_P9)

earnings_df <- cbind(group, earnings_info)
  
write.csv(school_df, "C:/Users/Curtis/Desktop/rcode/college_scorecard/Data/2014_15/earnings_df.csv")

