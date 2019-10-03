# parse schowe residents file
library(tidyverse)

# change periods in dates to dashes
make_yeardate <- Vectorize(function(orig_str){
  test_str <- "[0-9]{1,2}(\\.|:) {0,2}[0-9]{0,2}\\. {0,2}([0-9]{2,4}|i[0-9]{2,4})"
  if (str_detect(orig_str,test_str)){
    temp_str <- str_extract(orig_str,test_str)
    temp_str <- str_replace_all(temp_str,"\\. ","-")
    orig_str <- str_replace(orig_str,test_str,temp_str)
  }
  return(orig_str)  
}

)  

# house in alt-schowe did not have house numbers but this file places an index number in front.  put in after street.
# add whether in old or new town
house_number_swap <- Vectorize(function(orig_str){
  if (str_detect(orig_str,"^[0-9]+")){
    tokens <- strsplit(orig_str,split = " -- ") %>% unlist()
    orig_str <- paste0("Alt -- ", tokens[2]," ",tokens[1])
#    orig_str <- paste0("Alt -- ", tokens[2]," ",tokens[1]," -- ",tokens[3])
  } else   {
    orig_str <- paste0("Neu -- ",orig_str)
  }
  return(orig_str)
}
)

# extract anything that looks like an address and move it to the address column
# move_address <- function()

raw_data <- read_lines("data/Schowe_Residents.txt",skip_empty_rows = TRUE) %>%
  enframe(name=NULL)

#expand abbreviations
raw_data_1 <- raw_data %>% 
  mutate(value=str_remove(value,"\n")) %>% 
  mutate(value = str_replace_all(value,"geb\\.","nee")) %>% 
  mutate(value = str_replace_all(value,"ev\\.","Evangelical")) %>% 
  mutate(value = str_replace_all(value,"ref\\.","Reformed")) %>%
  mutate(value = str_replace_all(value,"gef\\.","died")) %>%
  mutate(value = str_replace_all(value,"Jug\\.","Jugoslavia")) %>%
  mutate(value = str_replace_all(value,"†","died")) %>% 
  mutate(value = str_replace_all(value,", {0,2}sen\\."," Senior")) %>% 
  mutate(value = str_replace_all(value,", {0,2}jun\\."," Junior")) %>% 
  mutate(value = str_replace_all(value,"r\\. {0,2}k\\.","Catholic")) %>% 
  {.}  

raw_data_2 <- raw_data_1 %>% 
  mutate(value=make_yeardate(value)) %>% 
  separate(value,
           into=c("value","address"),
           sep = ";",
           extra = "merge") %>% 
  separate(value,
           into=c("last_name","first_name","birth_date","faith","outcome"),
           sep = ",",
           extra = "merge")

raw_data_3 <- raw_data_2 %>% 
  mutate(first_name=ifelse(is.na(first_name),last_name,first_name)) %>% 
  mutate(birth_date=ifelse(is.na(birth_date),first_name,birth_date)) %>% 
  mutate(faith=ifelse(is.na(faith),birth_date,faith)) %>% 
  mutate(outcome=ifelse(is.na(outcome),faith,outcome)) %>% 
  mutate(address=ifelse(is.na(address),outcome,address)) %>%
  mutate(address=str_remove(address,".+(,|:|;|\\.)")) %>% 
  mutate(address=house_number_swap(trimws(address))) %>% 
  separate(address,into=c("alt_oder_neu","street"),sep=" -- ") %>% 
  mutate(house=str_extract(street," [0-9]{1,2}$")) %>% 
  mutate(street=str_remove(street," [0-9]{1,2}$")) %>% 
  {.}
  