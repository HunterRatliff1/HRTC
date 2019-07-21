require(tidyverse)
require(survival)
require(survminer)
require(lubridate)
require(GGally)


# ------------------------ #
#  Survival with our data  #
# ------------------------ #

users <- read_csv("~/Downloads/utmb_secondary_bullying_relationships_profiles_20190111_130142.csv") %>%
  select(userID=id, phone_number, status.user=status, 
         date.join=created_at, date.exit=opted_out_at,
         zipcode=postal_code, lat=latitude, lon=longitude,
         gender=aaGender, grade=aaGradeLevel, race=aaRace) %>%
  mutate(
    date.join = as.Date(mdy_hm(date.join)),
    date.exit = as.Date(mdy_hm(date.exit)),
    gender    = as.factor(gender),
    grade     = as.factor(grade),
    race      = as.factor(race)
  )  %>%
  mutate(Censor = ifelse(is.na(date.exit), 0, 1)) %>% # Add in some rough censored data
  mutate(date.exit = ifelse(is.na(date.exit),ymd("2019-01-11"), date.exit)) %>%
  mutate(date.exit = as.Date(date.exit, origin=lubridate::origin)) %>%
  mutate(SurvEnd = as.numeric(as.duration(interval(date.join, date.exit)), "days"))


survObj <- Surv(time=users$SurvEnd, event=users$Censor) # not including any event data

survfit(survObj ~ 1, data = users) %>%
  ggsurv() + scale_y_continuous(labels = scales::percent)# + scale_x_continuous(limits=c(0,200))
  ggsurvplot(data = users, pval=T, #conf.int=T,
             risk.table = TRUE,
             fun = "pct",
             size = 1,
             legend = "bottom") 



# ------------------------ #
#  Survival with our data  #
# ------------------------ #

df <- masterDf %>%
  # mutate(Censor = ifelse(
    # DateSent==max(DateSent) & Replied>0, 0, 1)) %>%
  filter(!is.na(Response)) %>%
  mutate(
    grade=as.factor(grade),
    Audience=as.factor(Audience)) %>%
  group_by(phone_number, gender, grade, race, Audience) %>%
  arrange(DateSent) %>%
  
  summarise(
    DateEnd = max(DateSent),
    numResponses = sum(Replied)
  ) %>%
  ungroup() %>%
  mutate(
    SurvEnd = as.numeric(as.duration(interval(ymd("2015-10-16"), DateEnd)), "days"),
    Censor = ifelse(SurvEnd==max(SurvEnd), 0, 1) # Add in some rough censored data
  )

survObj <- Surv(time=df$SurvEnd, event=df$Censor) # not including any event data

survfit(survObj ~ 1, data = df) %>%
  ggsurvplot(data = df, pval=T, #conf.int=T,
             risk.table = TRUE,
             fun = "pct",
             size = 1,
             legend = "bottom") 

survfit(survObj ~ 1, data = df) %>%
  ggsurv() +
  labs(x="Days (since first message)") + 
  scale_y_continuous(labels = scales::percent) +
  theme_fivethirtyeight() + theme(axis.title = element_text())
  

rm(survObj, df)

# ------------------ #
#      Tutorial      # https://www.datacamp.com/community/tutorials/survival-analysis-R
# ------------------ #
data(ovarian)

# glimpse(ovarian)
help(ovarian)

# Dichotomize age and change data labels
ovarian$rx <- factor(ovarian$rx, 
                     levels = c("1", "2"), 
                     labels = c("A", "B"))
ovarian$resid.ds <- factor(ovarian$resid.ds, 
                           levels = c("1", "2"), 
                           labels = c("no", "yes"))
ovarian$ecog.ps <- factor(ovarian$ecog.ps, 
                          levels = c("1", "2"), 
                          labels = c("good", "bad"))

# Dichotomize age
hist(ovarian$age) # Data seems to be bimodal
ovarian <- ovarian %>% mutate(age_group = ifelse(age >=50, "old", "young"))
ovarian$age_group <- factor(ovarian$age_group)
qplot(data=ovarian, x=futime, fill=as.factor(fustat))

# Fit survival data using the Kaplan-Meier method
surv_object <- Surv(time = ovarian$futime, event = ovarian$fustat)
surv_object 


# Examine prdictive value of two Tx groups
fit1 <- survfit(surv_object ~ rx, data = ovarian)
summary(fit1)
ggsurvplot(fit1, data = ovarian, pval = TRUE)
rm(fit1)


# Examine prdictive value of residual disease status
fit2 <- survfit(surv_object ~ resid.ds, data = ovarian)
ggsurvplot(fit2, data = ovarian, pval = TRUE)
rm(fit2)


# Fit a Cox proportional hazards model
fit.coxph <- coxph(surv_object ~ rx + resid.ds + age_group + ecog.ps, 
                   data = ovarian)
ggforest(fit.coxph, data = ovarian)


rm(ovarian, surv_object, fit.coxph)