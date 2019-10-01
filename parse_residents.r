# parse schowe residents file
library(tidyverse)
raw_data <- read_lines("data/Schowe_Residents.txt",skip_empty_rows = TRUE) %>%
  enframe(name=NULL) %>% 
  mutate(value=str_remove(value,"\n")) %>% 
  mutate(value = str_replace_all(value,"geb\\.","nee")) %>% 
  mutate(value = str_replace_all(value,"ev\\.","evacutated")) %>% 
  mutate(value = str_replace_all(value,"ref\\.","refugee")) %>%
  mutate(value = str_replace_all(value,"†","died"))
  

make_yeardate <- Vectorize(function(orig_str){
  if (str_detect(orig_str,"[0-9]{1,2}\\.  [0-9]{1,2}\\.  [0-9]{4}")){
    temp_str <- str_extract(orig_str,"[0-9]{1,2}\\.  [0-9]{1,2}\\.  [0-9]{4}")
    temp_str <- str_replace_all(temp_str,"\\.","-")
    orig_str <- str_replace(orig_str,"[0-9]{1,2}\\.  [0-9]{1,2}\\.  [0-9]{4}",temp_str)
  }
  return(orig_str)  
}

)  

# house in alt-schowe did not have house numbers but this file places an index number in front.  put in after street.
# add whether in old or new town
house_number_swap <- Vectorize(function(orig_str){
  if (str_detect(orig_str,"^[0-9]+")){
    tokens <- strsplit(orig_str,split = " -- ") %>% unlist()
    orig_str <- paste0("Alt -- ", tokens[2]," ",tokens[1]," -- ",tokens[3])
  } else   {
    orig_str <- paste0("Neu -- ",orig_str)
  }
  
  return(orig_str)
}
)

raw_data_2 <- raw_data %>% mutate(value=make_yeardate(value))
