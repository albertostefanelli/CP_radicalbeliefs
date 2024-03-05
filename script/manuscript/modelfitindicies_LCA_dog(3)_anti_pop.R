##### --------------------------------------------------------------- ######
##### EXTRACT MODEL FIT FOR ALL THE ESTIMATED MODEL OF THE SAME CLASS ######
##### --------------------------------------------------------------- ######

##-------- Structure of the script --------##
# 1. read all the results 
# 2. model check (convergence, stability)
# 3. sample and LO-VO LRT 
# 3. tabulate and VIZ  

library(here)
library(flextable)
library(MplusAutomation)
library(dplyr)
library(ggplot2)

#### 1. read all the results  #####
# set name_class_model to get the extract the right model results from the folder 
name_class_moel <- "DOG_3_ANTI_POP"
files <- data.frame(names = list.files(path = here("results_mplus"), full.names = FALSE))
name_folder <- unlist(files %>% dplyr::filter(., grepl(name_class_moel, names)))[[1]]

# read all the output for the different number of classes (in this case 2:9)
all_output <- readModels(here("results_mplus",name_folder), what="all", quiet = TRUE)
summarymix <- map(all_output,function(x) x[["summaries"]])
summarymix[[1]] <- cbind(summarymix[[1]], T11_LMR_PValue= NA,T11_VLMR_PValue=NA, Entropy=NA )
summarymix <- map(summarymix, function(x) x[c("Title", "LL", "Parameters", "AIC", "BIC", "Entropy", "T11_LMR_PValue","T11_VLMR_PValue")])
summarymix <- as_tibble(do.call(rbind, summarymix))
summarymix$Classes <- seq(1:9)

#### 2. model check #####

# check for convergence 
for (i in seq(all_output)){if(length(all_output[[i]]$errors)==0){
  summarymix[i,paste0("Convergence")] <- "Converged"
}else{summarymix[i,paste0("Convergence")] <-"Non Convergence"} 
}

# check for stability of the solution based on the replication of the best LL
for (i in seq(all_output)){
  if(length(grep("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",all_output[[i]]$warnings))>0){
    summarymix[i,paste0("Rep LL")] <- "No"
  }else{
    summarymix[i,paste0("Rep LL")] <- "Yes"
  }
  
  
}

#### 3. Additional info to extract #####

# extract sample size 
for (i in seq(all_output)){
  summarymix[i,paste0("N")] <- sum(all_output[[i]]$class_counts$posteriorProb$count)
  # get row (class) that has the lowest N
  class_min <- which.min(all_output[[i]]$class_counts$posteriorProb$count)
  # round the N for the selected class
  summarymix[i,paste0("min_N")] <- round(all_output[[i]]$class_counts$posteriorProb$count[class_min])
}


# calculate R-LO LRT
# this is different from the one calculated by MPLUS !
# pvalue is always 0 meaning that next model is always better
# this is likely to be wrong

for (i in  1:(nrow(summarymix)-1)){
   
  summarymix[i+1,paste0("LRT p.value")]  <-  round(tidyLPA::calc_lrt(n = array(unlist(summarymix[i,"N"])),
                                                                     null_ll = array(unlist(summarymix[i,"LL"])) ,
                                                                     null_param = array(unlist(summarymix[i,"Parameters"])),
                                                                     null_classes = array(unlist(summarymix[i,"Classes"])),
                                                                     alt_ll = array(unlist(summarymix[i+1,"LL"])),
                                                                     alt_param = array(unlist(summarymix[i+1,"Parameters"])),
                                                                     alt_classes = array(unlist(summarymix[i+1,"Classes"])))["lmr_p"],2)
}

for (i in  1:(nrow(summarymix)-1)){
  
  summarymix[i+1,paste0("Delta BIC")]  <-  round(summarymix[i,"BIC"]- summarymix[i+1,"BIC"],2)
}


#### 4. Tabulation and MODEL FIT VIZ #####


names(summarymix)[grep("min_N",names(summarymix))] <- "Lowest N"

table_article <- summarymix %>% 
  select(Classes,Parameters,AIC, BIC, `Delta BIC`, Entropy,`T11_VLMR_PValue`,"Lowest N") %>%
  mutate_at(vars(Entropy),round,2)

names(table_article)[grep("T11_VLMR_PValue",names(table_article))] <- "VLRT p.value"

table_article <- table_article %>% flextable()

saveRDS(table_article, here("tables", "table2_fit.rds"))
