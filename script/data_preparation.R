#### ENV PREPARATION ####

library(haven)
library(naniar)
library(dplyr)
library(here)

## User-defined functions ##
turn_function <- function (x) {(max(x,na.rm=T)) -x}
# useful if SEM (std latent variables) is not used
range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
# useful to be able to compare effet of dummies with continous variables 
grand_mean_twoSD <- function (x) {
  (x - mean(x, na.rm = TRUE))/(2 * sd(x, na.rm = TRUE))
}

## session options ##
options(scipen=999)

# read data 

base_data <-  haven::read_sav("data/data.sav")

### Datacleaning ###

# list of indices to clean 

# Dogmatism 
# “q77_1 Il y a une ligne de démarcation claire entre ce qui est bien et ce qui est mal”
# “q77_2 Il n’y a qu’une seule façon correcte d’aborder la plupart des choses”
# “q77_3 Les gens qui ne sont pas d’accord avec moi ont généralement tort”
dogma <- names(base_data)[grep("q77_", names(base_data))]
# Anti-System
# q76_1 Seul un changement radical peut résoudre les problèmes actuels de notre société
# q76_2 Ce n’est pas seulement le gouvernement, mais le système tout entier qu’il faut remplacer
anti_sys <- names(base_data)[grep("q76_", names(base_data))]
# Populism
# Q67_1	People and not the politicians should take decisions
# Q67_2	People would be better represented by ordinary citizens
# Q67_3	Power should be returned to the people
# Q67_4	Better if politicians just followed the will of the people
# Q67_5	Ordinary people know better than politicians
pop <- names(base_data)[grep("q67_", names(base_data))]
# Political Interest 
# q37_1 How often do you follow the political news in the media?
# q37_2 How often do you discuss politics with your friends?
pol_int <- names(base_data)[grep("q37_", names(base_data))]
# Trust  
# Q66-1	Trust in the legal system
# Q66-2	Trust in the national police
# Q66-3	Trust in the press	
# Q66-4	Trust in political parties
# Q66-5	Trust in parliament	
# Q66-6	Trust in the king	
# Q66-7	Trust in the government	
# Q66-8	Trust in the trade unions 
# Q66-9	Trust in science	
trust <- names(base_data)[grep("q66_", names(base_data))]

# recode NAs 
selection_cleaned <- base_data %>% 
  replace_with_na_at(trust, ~.x >5) %>%
  replace_with_na_at("q81", ~.x >2) %>%  # member pol party
  replace_with_na_at("q3", ~.x >2) %>%   # paid occupation 
  replace_with_na_at("q13", ~.x >10) %>% # education 
  replace_with_na_at("q18", ~.x >4) %>%  # subjective social class 
  replace_with_na_at("q15", ~.x >11) %>% # income
  replace_with_na_at("q19", ~.x >3) %>%  # father country of birth
  replace_with_na_at("q20", ~.x >3) %>%  # mother country of birth 
  replace_with_na_at("q57", ~.x >10) %>%  # left-right self-placement
  replace_with_na_at(c("q36",   # interest in politics
                       "q37_1", # frequency follow news
                       "q37_2", # discuss politics 
                       pop,
                       dogma,
                       anti_sys,
                       pol_int), ~.x >5) %>%
  replace_with_na_at("q24", ~.x >52 ) %>% # vote choice if election day
  replace_with_na_at("q21", ~.x >10) %>%  # religious choice 
  replace_with_na_at("q64", ~.x >5)       # disregarded or abandoned by politics
  

# reverse coded variables to flip
# member party 
selection_cleaned$q81 <- turn_function(selection_cleaned$q81)
#q37_1 – How often do you follow the political news in the media?
selection_cleaned$q37_1 <- turn_function(selection_cleaned$q37_1) + 1
#q37_2 – How often do you discuss politics with your friends? 
selection_cleaned$q37_2 <- turn_function(selection_cleaned$q37_2) + 1

# sum indices 
selection_cleaned$trust <- selection_cleaned %>% 
  dplyr::select(starts_with("q66_")) %>%  
  rowwise() %>% 
  dplyr::mutate(sum = sum(cur_data(),na.rm=F)/ncol(.)) %>%  
  select(sum) %>% 
  unlist() %>% 
  as.vector()

selection_cleaned$pol_int <- selection_cleaned %>% 
  dplyr::select("q37_1","q37_2","q36") %>%  
  rowwise() %>% 
  dplyr::mutate(sum = sum(cur_data(), na.rm=F)/ncol(.)) %>%  
  select(sum) %>% 
  unlist() %>% 
  as.vector()


# recoding
selection_cleaned$e_back <- factor(ifelse(selection_cleaned$q19==1 & selection_cleaned$q20==1,
                                          "Belgian", "Other" ))
base_data$region <- as.factor(ifelse(base_data$region==2, "Wallonia", "Flanders"))

selection_cleaned$voted <- factor(selection_cleaned$q24, 
                                  levels=c(1,2,3,4,5,6,7,8,9,12,13,14,15,16,17,18,19,50,51,52), 
                                  labels = c("cdv",
                                  "nva",
                                  "vld",
                                  "spa",
                                  "vb",
                                  "gronen",
                                  "pvda",
                                  "fp",
                                  "autre",
                                  "ps",
                                  "mr",
                                  "cdh",
                                  "ecolo",
                                  "ptb",
                                  "defi",
                                  "pp",
                                  "autre",
                                  "invalid",
                                  "invalid",
                                  "novote")
)


selection_cleaned$v_pop_ed <- factor(ifelse(selection_cleaned$voted=="ptb","Populist Left",
                                            ifelse(selection_cleaned$voted=="pp", "Populist Right", 
                                                   ifelse(selection_cleaned$voted=="pvda", "Populist Left", 
                                                          ifelse(selection_cleaned$voted=="vb", "Populist Right",
                                                                 ifelse(selection_cleaned$voted=="invalid", "Invalid",
                                                                        ifelse(selection_cleaned$voted=="novote", "novote",
                                                                               "Mainstream")))))))

# Prepare data for MPLUS 

# subjective social class 
# merge together to Middle and Upper due to low sample size
selection_cleaned$q18 <- factor(ifelse(selection_cleaned$q18==4, 3, selection_cleaned$q18))

selection_cleaned <- fastDummies::dummy_cols(selection_cleaned,
                                             "q18",
                                             remove_first_dummy = TRUE,
                                             ignore_na = TRUE)


names(selection_cleaned)[grep("q18",names(selection_cleaned))][-1] <- c(
  "lmid","hupp"
)


selection_cleaned$reli4 <- factor(ifelse(selection_cleaned$q21==1 | selection_cleaned$q21==10 ,"None", 
                                         ifelse(selection_cleaned$q21==2, "Free-thinker", 
                                                ifelse(selection_cleaned$q21==3 | selection_cleaned$q21==4 , "Christian", "Others"))))

selection_cleaned$reli4 <- relevel(selection_cleaned$reli4, ref="None")

selection_cleaned <- fastDummies::dummy_cols(selection_cleaned,
                                             "reli4",
                                             remove_first_dummy = TRUE,
                                             ignore_na = TRUE)


names(selection_cleaned)[grep("reli4_",names(selection_cleaned))] <- c("CHRIS",
                                                                       "FREE",
                                                                       "OTHER"
                                                                       
)

