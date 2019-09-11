# library(tidyverse)
# 
# outcome <- read_csv("outcome-of-care-measures.csv", na = c("", "Not Available"))
# head(outcome)
# 
# outcome$`Hospital 30-Day Death (Mortality) Rates from Heart Attack` %>% hist()
# outcome$`Hospital 30-Day Death (Mortality) Rates from Pneumonia` %>% hist()

best <- function(state, outcome) {
  require(tidyverse)
  
  # check valid outcome
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) stop("invalid outcome")
  
  outcome_data <- suppressMessages(read_csv("outcome-of-care-measures.csv", na = c("", "Not Available"))) %>% 
    rename(`heart attack` = `Hospital 30-Day Death (Mortality) Rates from Heart Attack`,
           `heart failure` = `Hospital 30-Day Death (Mortality) Rates from Heart Failure`,
           pneumonia = `Hospital 30-Day Death (Mortality) Rates from Pneumonia`)
  
  #check valid state
  if (!state %in% outcome_data$State) stop("invalid state")
  
  outcome_data %>% 
    filter(State == state) %>% 
    arrange(!!ensym(outcome), `Hospital Name`) %>% 
    .[[1, "Hospital Name"]]
  
  
}

#Quiz week 4
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")

rankhospital <- function(state, outcome, num = "best") {
  require(tidyverse)
  
  # check valid outcome
  if (!num %in% c("best", "worst") & !is.numeric(num)) stop("invalid num")
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) stop("invalid outcome")
  
  outcome_data <- suppressMessages(read_csv("outcome-of-care-measures.csv", na = c("", "Not Available"))) %>% 
    rename(`heart attack` = `Hospital 30-Day Death (Mortality) Rates from Heart Attack`,
           `heart failure` = `Hospital 30-Day Death (Mortality) Rates from Heart Failure`,
           pneumonia = `Hospital 30-Day Death (Mortality) Rates from Pneumonia`)
  
  #check valid state
  if (!state %in% outcome_data$State) stop("invalid state")
  
  rank <- case_when(
    num == "best" ~ 1L,
    num == "worst" ~ -1L,
    TRUE ~ suppressWarnings(as.integer(num))
  )
  
  outcome_data %>% 
    filter(State == state) %>% 
    filter(!is.na(!!ensym(outcome))) %>% 
    arrange(!!ensym(outcome), `Hospital Name`) %>% 
    summarise(rank_hospital = nth(`Hospital Name`, rank)) %>% 
    pull(rank_hospital)
    
  
}

## Quiz week 4
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
