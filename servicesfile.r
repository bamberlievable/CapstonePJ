setwd("~/Desktop/FC research/Capstone/datasets/230 NYTD Service FY2011-2018/Data/Stata")
library(haven)
services<-read_dta("Services2018.dta")

View(services)
table(services$fy)

ls(services)
# [1] "acsuppsv"   "agemp"      "amiakn"     "asian"      "blkafram"   "budgetsv"   "careersv"   "cohort"    
# [9] "delinqntsv" "dob"        "edlevlsv"   "educfinasv" "emplytrsv"  "famsuppsv"  "fcstatsv"   "fy"        
# [17] "hawaiipi"   "hisorgin"   "hlthedsv"   "housedsv"   "ilnasv"     "lclfipssv"  "mentorsv"   "othrfinasv"
# [25] "psedsuppsv" "race"       "racedcln"   "raceethn"   "raceunkn"   "recnumbr"   "repdate"    "rmbrdfasv" 
# [33] "sex"        "silsv"      "specedsv"   "st"         "stfcid"     "stfips"     "tribesv"    "white"  

table(services$st)

#subset ny
NYservices<-subset(services, st=="NY")

table(NYservices$cohort) 
# 0         2011   2014 
# 131435   1867   4409 

#subset 2014 cohort....are there youth that were discharged that received no services? Check outcomes, that will determine if I add outomes to 2014services or two NY services
services2014<-subset(NYservices, cohort==2014)

# Descriptives
attach(NYservices)
table(raceethn)
table(sex)
table(delinqntsv)


#services
#1. add var code 1 if received services
NYservices$received<-0

# "acsuppsv" "budgetsv"   "careersv" "educfinasv" "emplytrsv"  "famsuppsv"  "fcstatsv"       
# "hlthedsv"  "housedsv"   "ilnasv"  "mentorsv"   "othrfinasv"
# "psedsuppsv" "rmbrdfasv" 
#  "silsv"      "specedsv 

NYservices$received<- ifelse(NYservices$acsuppsv==1,1,NYservices$received)
NYservices$received<- ifelse(NYservices$budgetsv==1,1,NYservices$received)
NYservices$received<- ifelse(NYservices$careersv==1,1,NYservices$received)
NYservices$received<- ifelse(NYservices$educfinasv==1,1,NYservices$received)
NYservices$received<- ifelse(NYservices$emplytrsv==1,1,NYservices$received)
NYservices$received<- ifelse(NYservices$famsuppsv==1,1,NYservices$received)
NYservices$received<- ifelse(NYservices$fcstatsv==1,1,NYservices$received)
NYservices$received<- ifelse(NYservices$hlthedsv==1,1,NYservices$received)
NYservices$received<- ifelse(NYservices$housedsv==1,1,NYservices$received)
NYservices$received<- ifelse(NYservices$ilnasv==1,1,NYservices$received)
NYservices$received<- ifelse(NYservices$mentorsv==1,1,NYservices$received)
NYservices$received<- ifelse(NYservices$othrfinasv==1,1,NYservices$received)
NYservices$received<- ifelse(NYservices$psedsuppsv==1,1,NYservices$received)
NYservices$received<- ifelse(NYservices$rmbrdfasv==1,1,NYservices$received)
NYservices$received<- ifelse(NYservices$silsv==1,1,NYservices$received)
NYservices$received<- ifelse(NYservices$specedsv==1,1,NYservices$received)


table(received)
# if (condition 1) {
#   statement 1
# } else if (statement 2) {
#   statement 2
# } else {
#   statement 3
# }

#county fips:  "lclfipssv"

#install.packages("pivottabler")

NYservices$servDesc<-"No Services"

NYservices$servDesc<- ifelse(NYservices$acsuppsv==1,"Academic Support",NYservices$servDesc)
NYservices$servDesc<- ifelse(NYservices$budgetsv==1,"Fin Mgmt",NYservices$servDesc)
NYservices$servDesc<- ifelse(NYservices$careersv==1,"Career Prep",NYservices$servDesc)
NYservices$servDesc<- ifelse(NYservices$educfinasv==1,"Edu Financial Assist",NYservices$servDesc)
NYservices$servDesc<- ifelse(NYservices$emplytrsv==1,"Emply Programs or Vocational Training",NYservices$servDesc)
NYservices$servDesc<- ifelse(NYservices$famsuppsv==1,"Family and Healthy Marriage",NYservices$servDesc)
NYservices$servDesc<- ifelse(NYservices$fcstatsv==1,"FC Status Services",NYservices$servDesc) ##check out
NYservices$servDesc<- ifelse(NYservices$hlthedsv==1,"Health Education",NYservices$servDesc)
NYservices$servDesc<- ifelse(NYservices$housedsv==1,"Housing Education and Home Mgmt",NYservices$servDesc)
NYservices$servDesc<- ifelse(NYservices$ilnasv==1,"Ind Living Needs Assessment",NYservices$servDesc)
NYservices$servDesc<- ifelse(NYservices$mentorsv==1,"Mentoring",NYservices$servDesc)
NYservices$servDesc<- ifelse(NYservices$othrfinasv==1,"Other financial assistance",NYservices$servDesc)
NYservices$servDesc<- ifelse(NYservices$psedsuppsv==1,"Post-Sec. Edu Support",NYservices$servDesc)
NYservices$servDesc<- ifelse(NYservices$rmbrdfasv==1,"Room and Board Fin. Assist",NYservices$servDesc)
NYservices$servDesc<- ifelse(NYservices$silsv==1,"Supervised Ind. Living",NYservices$servDesc)
NYservices$servDesc<- ifelse(NYservices$specedsv==1,"Special Education",NYservices$servDesc)


table(NYservices$servDesc,NYservices$received)

table(NYservices$acsuppsv,NYservices$servDesc=="Academic Support")


# r issues: the data totals in my table NYservices$servDesc are not matching the totals in each category...sent out a message on R4DS

library(pivottabler) #for info: https://cran.r-project.org/web/packages/pivottabler/vignettes/v00-vignettes.html
pt <- PivotTable$new()
pt$addData(NYservices)
pt$addColumnDataGroups("servDesc")
pt$defineCalculation(calculationName="Total Services Reseived", summariseExpression="n()")
pt$renderPivot()

#save to project

