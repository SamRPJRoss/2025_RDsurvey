# Preliminary data analysis/visualisation carried out in Okinawa
# March 2024, MSF, SR, OP

rm(list = ls())

## check/setwd by user
if (basename(getwd()) != "2023_RDNSurvey") {
  if (Sys.getenv("USERNAME") == "<mike's>")
    setwd("~/Documents/Github/2023_RDNSurvey/")
}
if (!"Require" %in% installed.packages())
  install.packages("Require")

invisible(Require::setLibPaths("packages/"))

Require::Require("nnet")

# The commented analysis below was based on an 'old' version of the data. Updated data/analysis appears further down

# RD.clarity <- read.csv('~/Library/CloudStorage/Dropbox/ResponseDiversity/RD_data/RDclarity.csv', stringsAsFactors = TRUE)
# RD.clarity$Q4sum <- rowSums(RD.clarity[,5:9])

# Old naming approach
# mnom.1 <- multinom(RD.clear~EcoStab.Rank, data = RD.clarity)
# mnom.2 <- multinom(RD.clear~EcoStab.Rank*FD.Rank, data = RD.clarity)
# mnom.3 <- multinom(RD.clear~EcoStab.Rank*FD.Rank*RD.Rank, data = RD.clarity)
# mnom.4 <- multinom(RD.clear~FD.Rank, data = RD.clarity)
# mnom.5 <- multinom(RD.clear~RD.Rank, data = RD.clarity)

# car::Anova(mnom.6 <- multinom(RD.clear~RD.Rank*EcoStab.Rank, data = RD.clarity), type = 3)
# car::Anova(mnom.7 <- multinom(RD.clear~RD.Rank*FD.Rank, data = RD.clarity), type = 3)

# car::Anova(mnom.5b <- multinom(RD.clear~as.factor(RD.Rank), data = RD.clarity))

# # summary(mnom)
# car::Anova(mnom.1, type = 3)
# car::Anova(mnom.2, type = 3)
# car::Anova(mnom.3, type = 3)
# car::Anova(mnom.4, type = 3)
# car::Anova(mnom.5, type = 3)

# # Best model?
# AICcmodavg::aictab(list(mnom.1,mnom.2,mnom.3,mnom.4,mnom.5, mnom.6, mnom.7))
# visreg::visreg(mnom.5, overlay = TRUE, collapse = TRUE)

# Trying to run an Ordered Logistic Regression; not convinced this is actually sensible, though!
# https://stats.oarc.ucla.edu/r/dae/ordinal-logistic-regression/
# car::Anova(ord.mod <- MASS::polr(as.factor(RD.Rank)~RD.clear, data = RD.clarity), type = 2)
# summary(ord.mod)
# visreg::visreg(ord.mod)

# anova(MASS::glm.nb(Q4sum~RD.Rank, data = RD.clarity, maxit = 500))
# anova(glm(Q4sum~RD.Rank, data = RD.clarity, poisson, maxit = 500), test = "Chi")


# # Some basic descriptive values for Q4
# with(RD.clarity, table(Q4sum, RD.clear))
# colSums(RD.clarity[,5:9])

# # Does conceptual clarity link to the existence of a widely accepted definition?
# with(RD.clarity, table(Q3_RD.def, RD.clear))/69

# # If people think there's a clear definition, how multifarious is that definition?
# with(RD.clarity, table(Q3_RD.def, Q4sum))

# # Is there any potential decline in the number of responses based on the order in which the options are presented?
# summary(MASS::glm.nb(colSums(RD.clarity[,5:9])~c(1:5), maxit = 100))

#########################################
# New naming approach:
#########################################

RD.dataset <- read.csv('2023.12_Final data/FINAL_Survey_S1S3_Rfriendly.csv', stringsAsFactors = TRUE)
# RD.dataset$Q4sum <- rowSums(RD.dataset[,5:9])

# Q1 = "How familiar are you with work on the following topics"
# Q1_1 = Familiarity with Ecological Stability
# Q1_2 = Familiarity with Functional Diversity
# Q1_3 = Familiarity with Response Diversity
# Q2 = "Do you think that response diversity is a clear concept?"

mnom.1 <- multinom(Q2~Q1_1, data = RD.dataset)
mnom.2 <- multinom(Q2~Q1_1*Q1_2, data = RD.dataset)
mnom.3 <- multinom(Q2~Q1_1*Q1_2*Q1_3, data = RD.dataset)
mnom.4 <- multinom(Q2~Q1_2, data = RD.dataset)
mnom.5 <- multinom(Q2~Q1_3, data = RD.dataset)
mnom.6 <- multinom(Q2~Q1_3*Q1_1, data = RD.dataset)
mnom.7 <- multinom(Q2~Q1_3*Q1_2, data = RD.dataset)

AICcmodavg::aictab(list(mnom.1,mnom.2,mnom.3,mnom.4,mnom.5, mnom.6, mnom.7))
car::Anova(mnom.7,type = 3)
summary(mnom.7)
emmeans::emtrends()
visreg::visreg(mnom.5, overlay = TRUE, collapse = TRUE)

#########################################
# Demographic info

# Career Stage
career.stages <- with(RD.dataset,colSums(cbind(Q17_1,Q17_2,Q17_3,Q17_4), na.rm = TRUE))

# Check only one stage is chosen per participant
which(with(RD.dataset,rowSums(cbind(Q17_1,Q17_2,Q17_3,Q17_4), na.rm = TRUE))==0)
# Dammit! ID 17 has not provided a career stage. Maybe some non-academic practitioner?
# At least no-one has ID'd with >1 stage

# Set up a new vector that combines all career stages into a single column
# Do we want to make these non-numeric?
RD.dataset$Q17_combined <- vector('numeric', length(RD.dataset$Q17_1))
RD.dataset$Q17_combined[which(RD.dataset$Q17_1==1)] <- 1	# 'ECR'
RD.dataset$Q17_combined[which(RD.dataset$Q17_2==1)] <- 2	# 'Postdoc'
RD.dataset$Q17_combined[which(RD.dataset$Q17_3==1)] <- 3	# 'PD.5plus'
RD.dataset$Q17_combined[which(RD.dataset$Q17_4==1)] <- 4	# 'PD.10plus'

# Where are respondents based?
table(RD.dataset$Q18)

# Subject area
with(RD.dataset,colSums(cbind(Q19_1,Q19_2,Q19_3,Q19_4,Q19_5,Q19_6,Q19_7), na.rm = TRUE))

# Who IDs as multiple subject areas
hist(with(RD.dataset,rowSums(cbind(Q19_1,Q19_2,Q19_3,Q19_4,Q19_5,Q19_6,Q19_7), na.rm = TRUE)))

RD.dataset$subject.breadth <- with(RD.dataset,rowSums(cbind(Q19_1,Q19_2,Q19_3,Q19_4,Q19_5,Q19_6,Q19_7), na.rm = TRUE))

car::Anova(mnom.subject.breadth <- multinom(Q2~subject.breadth, data = RD.dataset),type = 3)
visreg::visreg(mnom.subject.breadth, overlay = TRUE, collapse = TRUE)

#########################################
# Outcome of RD Network

with(RD.dataset,colSums(cbind(Q16_1,Q16_2,Q16_3,Q16_4,Q16_5,Q16_6,Q16_7,Q16_8,Q16_9), na.rm = TRUE))

plot(with(subset(RD.dataset, Q17_1 == 1), colMeans(cbind(Q16_1,Q16_2,Q16_3,Q16_4,Q16_5,Q16_6,Q16_7,Q16_8,Q16_9), na.rm = TRUE)), pch = 16, col = "black", ylim = c(0,9), ylab = "Assigned rank")
points(with(subset(RD.dataset, Q17_2 == 1), colMeans(cbind(Q16_1,Q16_2,Q16_3,Q16_4,Q16_5,Q16_6,Q16_7,Q16_8,Q16_9), na.rm = TRUE)), pch = 16, col = "red")
points(with(subset(RD.dataset, Q17_3 == 1), colMeans(cbind(Q16_1,Q16_2,Q16_3,Q16_4,Q16_5,Q16_6,Q16_7,Q16_8,Q16_9), na.rm = TRUE)), pch = 16, col = "blue")
points(with(subset(RD.dataset, Q17_4 == 1), colMeans(cbind(Q16_1,Q16_2,Q16_3,Q16_4,Q16_5,Q16_6,Q16_7,Q16_8,Q16_9), na.rm = TRUE)), pch = 16, col = "purple")

career.1.95CI <- with(subset(RD.dataset, Q17_1 == 1), apply(cbind(Q16_1,Q16_2,Q16_3,Q16_4,Q16_5,Q16_6,Q16_7,Q16_8,Q16_9), 2, sd, na.rm = TRUE))/sqrt(career.stages[1])
career.2.95CI <- with(subset(RD.dataset, Q17_2 == 1), apply(cbind(Q16_1,Q16_2,Q16_3,Q16_4,Q16_5,Q16_6,Q16_7,Q16_8,Q16_9), 2, sd, na.rm = TRUE))/sqrt(career.stages[2])
career.3.95CI <- with(subset(RD.dataset, Q17_2 == 1), apply(cbind(Q16_1,Q16_2,Q16_3,Q16_4,Q16_5,Q16_6,Q16_7,Q16_8,Q16_9), 2, sd, na.rm = TRUE))/sqrt(career.stages[3])
career.4.95CI <- with(subset(RD.dataset, Q17_2 == 1), apply(cbind(Q16_1,Q16_2,Q16_3,Q16_4,Q16_5,Q16_6,Q16_7,Q16_8,Q16_9), 2, sd, na.rm = TRUE))/sqrt(career.stages[4])


##########################################
# Predictor variables: familiarity with concepts (EcoStab, FD, RD); Demographic variables: (career stage, continent, subject area)

# Descriptive stats: Standardised Methodology (Q15)

# Response variables: how are predictors associated with...
# Q7_12...21 which dimension of stabilty
# Q13_9...16 challenges 
# Q9 check Isbell's for ideas on presenting weighted CIs, for Q9_1, Q9_1b, etc
# Q10 & Q11, as Q9 above


with(subset(RD.dataset, Q17_1 == 1), plot(Q1_3, Q10b_1, pch = 16, ylim = c(min(RD.dataset$Q10_1, na.rm = TRUE), max(RD.dataset$Q10b_1, na.rm = TRUE))))
with(subset(RD.dataset, Q17_2 == 1), points(Q1_3, Q10b_1, pch = 16, col = 'red'))


plot(RD.dataset$Q10_1, RD.dataset$Q10b_1, pch = 16)
cor.test(RD.dataset$Q10_1, RD.dataset$Q10b_1, test = "Spearman")

cor.test(RD.dataset$Q9_1, RD.dataset$Q9b_1, test = "Spearman")