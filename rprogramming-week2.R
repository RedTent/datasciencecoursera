get_pollution_data <- function(directory, id) {
  file_paths <- paste0(directory, "/", stringr::str_pad(id, width = 3, side = "left", pad = "0"), ".csv")
  poll_data <- vroom::vroom(file_paths, delim = ",", col_types = "dnnn", progress = FALSE)
}

pollutantmean <- function(directory, pollutant, id = c(1:332)){
  poll_data <- get_pollution_data(directory = directory, id = id)
  mean(poll_data[[pollutant]], na.rm = TRUE)
  
  
}

complete <- function(directory, id) {
  require(dplyr)
  
  poll_data <- get_pollution_data(directory = directory, id = id)
  
  poll_data %>% 
    rename(id = ID) %>% 
    filter(complete.cases(.)) %>% 
    count(id, name = "nobs") %>% 
    mutate(nobs = ifelse(is.na(nobs), 0, nobs)) %>% 
    right_join(tibble::tibble(id = id), by = "id")
  
  
}

corr <- function(directory, threshold = 0){
  require(dplyr)
  locs_above_threshold <- complete(directory, id = c(1:332)) %>% 
    filter(nobs >= threshold) %>% 
    dplyr::pull(id)
  
  poll_data <- get_pollution_data(directory = directory, id = c(1:332))
  
  poll_data %>% 
    filter(ID %in% locs_above_threshold) %>% 
    group_by(ID) %>% 
    summarise(corr = cor(sulfate, nitrate, use = "complete.obs")) %>% 
    pull(corr)
  
  
}

# Quiz questions
pollutantmean("specdata", "nitrate", id = 1)
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")



# quiz questions
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])


#quiz questions

cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
