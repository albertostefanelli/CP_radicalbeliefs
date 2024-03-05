##### --------------------------------------------------------------------- ######
##### EXTRACT CLASS MOST LIKELY CLASS ASSIGNMENT (FOR MODEL WITH 6 CLASSES) ######
##### --------------------------------------------------------------------- ######

##-------- Structure of the script --------##
# 1. read out_file with data frame data
# 2. read out_file with order of the variable in the dataframe data
# 3. merge with original data.frame
# 4. calculate manifest means for each class [optional]

library(here)
library(flextable)
source(here("script", "data_preparation.R"))

##### 1. read out_file with dataframe data ######
# using 3-step regression model results but also baseline model can be used 
# the folder needs to be in the main here() directory

# outfile_data_export contains latent means (prefix C_), factor scores, class assignment 
outfile_data_export <- data.frame(names = list.files(path = here("results_mplus",
                                                                 "DOG_3_ANTI_POP_var_lat=0_covariance=0_residuals=fixed"), full.names = FALSE)) %>% 
  dplyr::filter(., grepl('.dat', names)) %>% 
  dplyr::filter(., grepl('data_export_', names))

##### 2. read out_file with order of the variable in the dataframe data ######

# outfile_model contains order of the exported variable extracted in outfile_data_export
outfile_model <- data.frame(names = list.files(path = here("results_mplus",
                                                           "DOG_3_ANTI_POP_var_lat=0_covariance=0_residuals=fixed"), full.names = FALSE)) %>% 
  dplyr::filter(., grepl('.out', names)) %>% 
  # !!!!!!! NB: set the correct class solution !!!!!!!
  dplyr::filter(., grepl('classes=', names))

data_selected <-c()
for (f in seq(outfile_data_export$names) ) {
  # read outfile to extract variable order 
  
  out <- readLines(paste0( here("results_mplus",
                                "DOG_3_ANTI_POP_var_lat=0_covariance=0_residuals=fixed"),"/",outfile_model[f,]) )
  # start order 
  start_out_order <- grep("Order and format of variables",out)
  # end order 
  end_out_order <- grep("Save file format",out)
  # adjust to remove white space 
  order_session <- out[c((start_out_order+2):(end_out_order-2))]
  # remove white space and unlist  
  order_session_c <-  strsplit(order_session, "[[:space:]]+") 
  list_order <- unlist(map(order_session_c,~.[2]))
  
  # get index latent mean pop for each latent class  (prefix C_)
  mean_pop_index <- grep("C_POP",list_order)
  mean_anti_index <- grep("C_ANTI",list_order)
  mean_dog_index <- grep("C_DOG",list_order)
  
  # get index estimated factor scores not adjust for class assignment 
  factor_pop_index <- grep("^POP$",list_order)
  factor_anti_index <- grep("^ANTI$",list_order)
  factor_dog_index <- grep("^DOG$",list_order)
  
  # get index most likely class 
  class_index <- grep("^C1$",list_order)
  
  # read the data export 
  out <- read.table(paste0(here("results_mplus",
                                "DOG_3_ANTI_POP_var_lat=0_covariance=0_residuals=fixed"),"/",outfile_data_export[f,]),header = FALSE)
  
  # rid is always the last column 
  rid <- out[,ncol(out)]
  
  # extract class, means,  factor 
  class_most_likely <- out[,class_index]
  
  # class mean adjusted for assignment (latent mean)
  # sample mean is zero but should differ by classes
  pop_mean <- out[,mean_pop_index]
  anti_mean <- out[,mean_anti_index]
  dog_mean <- out[,mean_dog_index]
  
  # non_adjusted factor scores
  # meaning level of the latent estimated for each individaul in the sample
  # NB: not sure what this can be used for 
  pop_factor <- out[,factor_pop_index]
  anti_factor <-  out[,factor_anti_index]
  dog_factor <-  out[,factor_dog_index]
  
  model <- f
  data_selected[[f]] <- data.frame(cbind(rid,class_most_likely,pop_mean,anti_mean,dog_mean,pop_factor,anti_factor,dog_factor,model))
}


df_assign <- do.call("rbind", data_selected) %>% data.frame(row.names = NULL)

##### 3. merge with original dataframe ######

class_membership_df <- df_assign %>% mutate_all(as.double) %>% select(rid,pop_mean,anti_mean,dog_mean,class_most_likely,model) %>% 
  ## !!!!!!!!NB: SELECT RIGHT CLASS SOLUTIONS (6 in this case) !!!!!!!!!!!!
  filter(model==6) %>%
  select(-model)

selection_cleaned <- selection_cleaned[!is.na(selection_cleaned$w_age_bel),]

binded_dataset_class <- left_join(selection_cleaned,class_membership_df, by="rid")
