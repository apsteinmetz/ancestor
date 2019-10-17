# guess gender for each first name
# start from US SS baby names database
# if there is a maiden name we automatically tag as female
# then manually assign remaining genders
load("data/schowe_residents_1944.rdata")

names_90 <- read_csv("data/yob1990.txt",
                     col_names=c("first_name","gender","count"),
                     col_types = list(col_character(),col_character(),col_number()))

names_90 <- names_90 %>% 
  select(first_name,gender,count) %>% 
  spread(gender,count) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(prob_female=F/(F+M))

cutoff = 0.75 # threshhold probability for calling gender
names_90 <- names_90 %>% mutate(gender="Uncertain")
names_90 <- names_90 %>% mutate(gender=if_else(prob_female>cutoff,"Female",gender))
names_90 <- names_90 %>% mutate(gender=if_else(prob_female<(1-cutoff),"Male",gender))
names_90_subset <- names_90 %>% select(first_name,gender)

genders <- schowe_residents_1944 %>% 
  left_join(names_90_subset,by="first_name") %>%
  mutate(gender=ifelse(!is.na(maiden_name),"Female",gender)) %>% 
  select(first_name,gender) %>% 
  distinct() %>% 
  arrange(gender,first_name) %>% 
  edit()
  {.}

genders_schowe <- genders %>% 
  mutate(gender = ifelse(gender=="f","Female",gender)) %>% 
  mutate(gender = ifelse(gender=="m","Male",gender)) %>% 
  mutate(gender = ifelse(!str_detect(gender,"ale"),"unknown",gender)) %>% 
  mutate(gender = ifelse(is.na(gender),"unknown",gender)) %>% 
  mutate(gender = as.factor(gender)) %>% 
  {.}

save(genders_schowe,file="data/genders_schowe.rdata")

