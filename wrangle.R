
require(tidyverse)
require(googlesheets)

# --- + --- + --- #
#    RESPONSES    #
# --- + --- + --- #

# Grab pseudo-response data
responses <- gs_title("HRTC Responses") %>% gs_read_csv()
responses <- responses %>%
  mutate(aaGradeLevel = as.integer(aaGradeLevel)) %>%
  mutate(aaRace = str_replace(aaRace, "2 .+", "2+")) %>%
  mutate(aaRace = str_replace(aaRace, "WH", "H")) %>% 
  select(phone_number, numResponses, starts_with("aa"), starts_with("DATE")) %>%
  select(-DATE_FreeRes, -DATE_Useful, -DATE_Round2, -DATE_Round3) %>%
  rename(gender=aaGender, grade=aaGradeLevel, race=aaRace) %>%
  gather(Question, Response, -phone_number, -numResponses, -gender, -grade, -race) %>%
  mutate(Response = str_replace(Response, "a|A", "1")) %>%
  mutate(Response = str_replace(Response, "b|B", "2")) %>%
  mutate(Response = str_replace(Response, "c|C", "3")) 

# Cheating for now, but make it like the export data had the uids
tempKey <- gs_title("HRTC Responses") %>% gs_read_csv("Key") %>% 
  select(Col, uid, DateSent) %>%
  mutate(uid = ifelse(is.na(uid), Col, uid)) %>% 
  mutate(DateSent = lubridate::mdy(DateSent))
responses <- responses %>%
  left_join(tempKey, by=c("Question"="Col")) %>%
  select(-Question)
rm(tempKey)

# --- + --- + --- + --- #
#    MASTER & MESSAGE   #
# --- + --- + --- + --- #
masterSheet <- readxl::read_excel("~/Dropbox/Text Campaign_Healthy Relationships/Texting Campaign Master 120418.xlsx", 
                             sheet = "Master", skip=1) %>%
  mutate(Category = na_if(Category, "N/A")) %>%
  mutate(Keywords = na_if(Keywords, "N/A")) %>%
  mutate(mid      = na_if(mid, "N/A"))  %>%
  mutate(Category = str_replace(Category, "\r\n", " ")) %>%
  mutate(Keywords = str_replace(Keywords, "\r\n", " ")) %>%
  select(-Campaign, -`Msg\r\n#`, -`Character Count__1`,
         -Author, -`Source (not included in message)`) %>%
  rename(Type=`Msg type`, Message=`Message (160 characters max)`, 
         ChCnt=`Character Count`, ReplyMsg=`Reply Response if applicable`,
         School=`School/District`) %>%
  # If NA, change to unspecified
  mutate(School = ifelse(is.na(School), "Unspecified", School)) %>%
  mutate(Audience = ifelse(is.na(Audience), "Unspecified", Audience))

msgSheet <- readxl::read_excel("~/Dropbox/Text Campaign_Healthy Relationships/Texting Campaign Master 120418.xlsx", 
                   sheet = "Messages") %>%
  select(mid:Ans) %>%
  rename(BaseMsg=`Message (with some varriation on how it's worded)`)

masterSheet <- masterSheet %>%
  select(-Category, -Keywords) %>%
  filter(!is.na(mid)) %>% # Filters out the messages w/o a MID here, because
                          # otherwise is joining abunch of NA's
  left_join(msgSheet) 

rm(msgSheet)

# --- + --- + --- #
#      JOIN       #
# --- + --- + --- #

df <- responses %>%
  left_join(masterSheet) %>%
  mutate(phone_number = percent_rank(phone_number)) # This drops their phone number,
                                                    # but keeps a form of a unique id

write_csv(df, "Data/MergedDf.csv")
rm(df)
rm(masterSheet)
rm(responses)
