##### ------------------------------------------------------------------------------- ####
##### 3-STEP REGRESSION MODELS PREDICTING CLASS MEMBERSHIP (FOR MODEL WITH 6 CLASSES) #### 
##### ------------------------------------------------------------------------------- ####

# this is a rather flexible approach to run a 3-STEP REGRESSIONS that automatically 
# extract the class predictors so that you can run batch regression once you have estimated 
# the LCA model

##-------- Structure of the script --------##
# 1. Run the models in MPlus
# 2. Extract results 
# 3. Tabulate results 
##-------- Structure of the script --------##

##### 1. RUN THE MODELS ####
# 1. specify the model that has been fitted 
# 2. set seed to the best LL solution (in MPLUS OPTSEED)
# 3. set random start to [Mplus does it automatically if you specify an OPTSEED] 

library(here)
library(data.table)
library(officer)

# get data 
source(here("script", "data_preparation.R"))
source(here("script", "toolbox",  "function_mplus_syntax_lpa.R"))

model <- "
 %OVERALL%
  !!! dogmatism !!!
  DOG BY Q77_1 q77_2 Q77_3;
  !!! antagonism !!!
  ANTI BY q76_1 q76_2;
  !!! populism !!!
  POP BY Q67_1 Q67_2 Q67_3 Q67_4 Q67_5;

  !!!!!!!!
  %c1#1%
  !!!!!!!!

  !!!! Means LV !!!!
  [POP](MPOP1);
  [ANTI](MANTI1);
  [DOG](MDOG1);
  !!!! Variances LV !!!!
  POP@0;
  ANTI@0;
  DOG@0;
  !!!! Covariances LV !!!!
  POP WITH ANTI@0;
  POP WITH DOG@0;
  ANTI WITH DOG@0;
  !!!! Residuals LV !!!!
  Q77_1(VQ77_1);
  Q77_2(VQ77_2);
  Q77_3(VQ77_3);
  Q76_1(VQ76_1);
  Q76_2(VQ76_2);
  Q67_1(VQ67_1);
  Q67_2(VQ67_2);
  Q67_3(VQ67_3);
  Q67_4(VQ67_4);
  Q67_5(VQ67_5);
  !!!! Intercepts manifest !!!!
  [Q77_1@0](intQ77101);
  [Q77_2@0](intQ77201);
  [Q77_3@0](intQ77301);
  [Q76_1@0](intQ76101);
  [Q76_2@0](intQ76201);
  [Q67_1@0](intQ67101);
  [Q67_2@0](intQ67201);
  [Q67_3@0](intQ67301);
  [Q67_4@0](intQ67401);
  [Q67_5@0](intQ67501);

  !!!!!!!!
  %c1#2%
  !!!!!!!!

  !!!! Means LV !!!!
  [POP](MPOP2);
  [ANTI](MANTI2);
  [DOG](MDOG2);
  !!!! Variances LV !!!!
  POP@0;
  ANTI@0;
  DOG@0;
  !!!! Covariances LV !!!!
  POP WITH ANTI@0;
  POP WITH DOG@0;
  ANTI WITH DOG@0;
  !!!! Residuals LV !!!!
  Q77_1(VQ77_1);
  Q77_2(VQ77_2);
  Q77_3(VQ77_3);
  Q76_1(VQ76_1);
  Q76_2(VQ76_2);
  Q67_1(VQ67_1);
  Q67_2(VQ67_2);
  Q67_3(VQ67_3);
  Q67_4(VQ67_4);
  Q67_5(VQ67_5);
  !!!! Intercepts manifest !!!!
  [Q77_1@0](intQ77102);
  [Q77_2@0](intQ77202);
  [Q77_3@0](intQ77302);
  [Q76_1@0](intQ76102);
  [Q76_2@0](intQ76202);
  [Q67_1@0](intQ67102);
  [Q67_2@0](intQ67202);
  [Q67_3@0](intQ67302);
  [Q67_4@0](intQ67402);
  [Q67_5@0](intQ67502);

  !!!!!!!!
  %c1#3%
  !!!!!!!!

  !!!! Means LV !!!!
  [POP](MPOP3);
  [ANTI](MANTI3);
  [DOG](MDOG3);
  !!!! Variances LV !!!!
  POP@0;
  ANTI@0;
  DOG@0;
  !!!! Covariances LV !!!!
  POP WITH ANTI@0;
  POP WITH DOG@0;
  ANTI WITH DOG@0;
  !!!! Residuals LV !!!!
  Q77_1(VQ77_1);
  Q77_2(VQ77_2);
  Q77_3(VQ77_3);
  Q76_1(VQ76_1);
  Q76_2(VQ76_2);
  Q67_1(VQ67_1);
  Q67_2(VQ67_2);
  Q67_3(VQ67_3);
  Q67_4(VQ67_4);
  Q67_5(VQ67_5);
  !!!! Intercepts manifest !!!!
  [Q77_1@0](intQ77103);
  [Q77_2@0](intQ77203);
  [Q77_3@0](intQ77303);
  [Q76_1@0](intQ76103);
  [Q76_2@0](intQ76203);
  [Q67_1@0](intQ67103);
  [Q67_2@0](intQ67203);
  [Q67_3@0](intQ67303);
  [Q67_4@0](intQ67403);
  [Q67_5@0](intQ67503);

  !!!!!!!!
  %c1#4%
  !!!!!!!!

  !!!! Means LV !!!!
  [POP](MPOP4);
  [ANTI](MANTI4);
  [DOG](MDOG4);
  !!!! Variances LV !!!!
  POP@0;
  ANTI@0;
  DOG@0;
  !!!! Covariances LV !!!!
  POP WITH ANTI@0;
  POP WITH DOG@0;
  ANTI WITH DOG@0;
  !!!! Residuals LV !!!!
  Q77_1(VQ77_1);
  Q77_2(VQ77_2);
  Q77_3(VQ77_3);
  Q76_1(VQ76_1);
  Q76_2(VQ76_2);
  Q67_1(VQ67_1);
  Q67_2(VQ67_2);
  Q67_3(VQ67_3);
  Q67_4(VQ67_4);
  Q67_5(VQ67_5);
  !!!! Intercepts manifest !!!!
  [Q77_1@0](intQ77104);
  [Q77_2@0](intQ77204);
  [Q77_3@0](intQ77304);
  [Q76_1@0](intQ76104);
  [Q76_2@0](intQ76204);
  [Q67_1@0](intQ67104);
  [Q67_2@0](intQ67204);
  [Q67_3@0](intQ67304);
  [Q67_4@0](intQ67404);
  [Q67_5@0](intQ67504);

  !!!!!!!!
  %c1#5%
  !!!!!!!!

  !!!! Means LV !!!!
  [POP](MPOP5);
  [ANTI](MANTI5);
  [DOG](MDOG5);
  !!!! Variances LV !!!!
  POP@0;
  ANTI@0;
  DOG@0;
  !!!! Covariances LV !!!!
  POP WITH ANTI@0;
  POP WITH DOG@0;
  ANTI WITH DOG@0;
  !!!! Residuals LV !!!!
  Q77_1(VQ77_1);
  Q77_2(VQ77_2);
  Q77_3(VQ77_3);
  Q76_1(VQ76_1);
  Q76_2(VQ76_2);
  Q67_1(VQ67_1);
  Q67_2(VQ67_2);
  Q67_3(VQ67_3);
  Q67_4(VQ67_4);
  Q67_5(VQ67_5);
  !!!! Intercepts manifest !!!!
  [Q77_1@0](intQ77105);
  [Q77_2@0](intQ77205);
  [Q77_3@0](intQ77305);
  [Q76_1@0](intQ76105);
  [Q76_2@0](intQ76205);
  [Q67_1@0](intQ67105);
  [Q67_2@0](intQ67205);
  [Q67_3@0](intQ67305);
  [Q67_4@0](intQ67405);
  [Q67_5@0](intQ67505);

  !!!!!!!!
  %c1#6%
  !!!!!!!!

  !!!! Means LV !!!!
  [POP](MPOP6);
  [ANTI](MANTI6);
  [DOG](MDOG6);
  !!!! Variances LV !!!!
  POP@0;
  ANTI@0;
  DOG@0;
  !!!! Covariances LV !!!!
  POP WITH ANTI@0;
  POP WITH DOG@0;
  ANTI WITH DOG@0;
  !!!! Residuals LV !!!!
  Q77_1(VQ77_1);
  Q77_2(VQ77_2);
  Q77_3(VQ77_3);
  Q76_1(VQ76_1);
  Q76_2(VQ76_2);
  Q67_1(VQ67_1);
  Q67_2(VQ67_2);
  Q67_3(VQ67_3);
  Q67_4(VQ67_4);
  Q67_5(VQ67_5);
  !!!! Intercepts manifest !!!!
  [Q77_1@0](intQ77106);
  [Q77_2@0](intQ77206);
  [Q77_3@0](intQ77306);
  [Q76_1@0](intQ76106);
  [Q76_2@0](intQ76206);
  [Q67_1@0](intQ67106);
  [Q67_2@0](intQ67206);
  [Q67_3@0](intQ67306);
  [Q67_4@0](intQ67406);
  [Q67_5@0](intQ67506);
"

#### VOTE CHOICE + DEMO + POLITICAL VARIABLES

mplus_selected <- selection_cleaned[!is.na(selection_cleaned$w_age_bel),]


mplus_selected <- mplus_selected %>%select(
  rid,
  age6,
  q2,      # gender
  q13,     # education 
  region,  
  e_back, 
  lmid,    # self-perceived social class: low-middle (ref: low)
  hupp,    # self-perceived social class: middle-upper (ref: low)
  CHRIS,   # religion: Christian (Ref: None)
  FREE,    # religion: Free Masons (Ref: None)
  OTHER,   # religion: Other (Ref: None)
  pol_int, 
  trust,
  q64,	#Disregarded or abandoned by politics
  w_age_bel,
  q76_1,  # Antagonism 
  q76_2,  # Antagonism 
  q77_1,  # Dogmatism 
  q77_2,  # Dogmatism 
  q77_3,  # Dogmatism 
  all_of(pop)) %>%
  mutate_all(as.numeric) %>% 
  mutate_at(vars(
    q13,
    age6,
    pol_int,
    trust,
    q64, 
    q76_1,
    q76_2,
    all_of(pop),
    q77_1,
    q77_2,
    q77_3
  ), list(~scale(.,center=TRUE, scale = TRUE)))%>% 
  mutate_all(as.numeric)%>%
  mutate(region=region-1)%>%
  data.frame() 


model_syntax <- mplusObject(
  TITLE = "BASE 3;",
  VARIABLE = paste(paste(strwrap(paste("USEVARIABLES = ",paste(names(mplus_selected), collapse=" "),
                                       ";"),30),collapse="\n"),
                   "!CATEGORICAL = ;
                   IDVARIABLE IS rid;
                   AUXILIARY = (R3STEP) 
                   age6 q13 q2 region q64
                   pol_int trust e_back
                   lmid hupp CHRIS FREE OTHER;",
                   "
                   class = c1 (6);",
                   
                   "
    WEIGHT=w_age_bel;",
    sep="\n"
  ),
  # the OPTSEED is taken from the LPA model estimation 
  # this allows to avoid to estimate the mixture iterative process twice
  ANALYSIS = "TYPE = MIXTURE;
             ESTIMATOR IS MLR;
             OPTSEED= 529496; 
             PROCESSORS = 1;
       ",
  MODEL = model,
  MODELCONSTRAINT= "
  ",
  OUTPUT = "TECH3; TECH8;CINTERVAL;FSCOEFFICIENT; STANDARDIZED;",
  SAVEDATA="",
  PLOT="",
  rdata=mplus_selected
  
)

model_out <- "regression_model_6_class_continuous.inp"

pop_full_sem <- mplusModeler(model_syntax, 
                             modelout = model_out, 
                             Mplus_command = "/Applications/Mplus_mac/mplus",
                             run=1L,
                             hashfilename = FALSE)

#### 2. EXTACT MODEL parameters ####

# move all the estimated regression output files in new folder 
files <- data.frame(names = list.files(path = here(), full.names = FALSE))
f_rad <- files %>% dplyr::filter(., grepl("regression_model_", names))
for (i in 1:length(f_rad)) { file.move(glue(here("{f_rad[,i]}")), here("results_mplus","results_3-step_regression_6_class"),overwrite = TRUE) }
# filter out the .out files 
out_files_regression <- f_rad %>% dplyr::filter(., grepl(".out", names))

### loop the .out files to extract the parameters 
# read all the output to get number of classes to extract 
all_output <- readModels(here("results_mplus","results_3-step_regression_6_class"), quiet = TRUE)

######## EXTRACT MEANS to select reference group (lowest means######## 
# number of classes to extract [in this case we can get it from the all_output since 
# we are just running the 3-step for the selected class solution]

class_to_extract <- array(unlist(all_output$summaries["NLatentClasses"]))

temp <- all_output$parameters$unstandardized %>%                                            #            
  filter(LatentClass!="Categorical.Latent.Variables")%>% 
  mutate_at(vars("LatentClass"), as.numeric)

temp_p <- as.data.frame(all_output$class_counts$modelEstimated$proportion)
colnames(temp_p) <- paste0("classp")

temp_p <- temp_p %>%  
  mutate(classp = round(classp * 100, 2)) %>%
  mutate("LatentClass" = 1:nrow(temp_p)) %>% mutate_all(as.numeric)

int_results <- left_join(temp, temp_p, by="LatentClass")
int_results$"Latent Class Proportions" <- paste0("Class ", int_results$LatentClass, " (",int_results$classp, "%",")")
model_results <- int_results

model_results <- model_results %>%                                                                 #
  filter(paramHeader == "Means") %>%    
  select(-classp, -LatentClass) %>%    
  filter(param %in% c("DOG","ANTI","POP") ) %>%
  select(param, est, pval, "Latent Class Proportions") %>%
  mutate_at(vars("Latent Class Proportions"), as.character) %>%
  mutate_at(vars(est), as.numeric)

# in this case, the Pro-System class with the lower estimated latent means 
pro_sys_cl <- c()
for (cl in unique(model_results$"Latent Class Proportions")){
  cl_results <- model_results %>% filter(`Latent Class Proportions`==cl)
  
  if (sum(cl_results$est <0) == nrow(cl_results)){
    pro_sys_cl[[cl]] <- cl
  }
  
}

name_pro_system_class <- array(unlist(pro_sys_cl))
nclass_pro_system <- gsub(".*s (.+) \\(.*", "\\1", name_pro_system_class)


######## EXTRACT REGRESSION COEFFICENTS ######## 
out <- readLines(paste0(here("results_mplus","results_3-step_regression_6_class",
                             "regression_model_6_class_continuous.out"
)))
  
# get 3-step logit section in the output 
start_logit <- grep('TESTS OF CATEGORICAL LATENT VARIABLE',out)[1]
end_logit <- grep('TESTS OF CATEGORICAL LATENT VARIABLE',out)[2]
  
logit_section <- out[start_logit:end_logit]
  
  # use different end point depending on the reference class 
  # if the reference category is the last one, end line is 
  # "Parameterization using Reference Class" meaning first section of the output 
  
if(as.numeric(nclass_pro_system)==class_to_extract-1 ){
    end_pro_sys_n <- "ODDS RATIOS FOR TESTS OF CATEGORICAL LATENT VARIABLE MULTINOMIAL LOGISTIC REGRESSIONS"
}else{
    
  end_pro_sys_n <- paste("Parameterization using Reference Class",as.numeric(nclass_pro_system)+1)
}
  
# start logit section for the correct reference class
start_pro_system_param <- grep(paste("Parameterization using Reference Class",nclass_pro_system),logit_section)
end_pro_system_param <- grep(end_pro_sys_n,logit_section)
# extract logit section for the correct reference class
logit_pro_system_class <- logit_section[(start_pro_system_param+2):(end_pro_system_param-2-class_to_extract)]
# clean coefficients from white space
c_on <- logit_pro_system_class[grep("ON$",logit_pro_system_class)] %>% 
    gsub("ON","",.) %>% 
    strsplit(., "[[:space:]]+")
  # unlist coefficents 
c_row <- unlist(purrr::map(c_on, ~.[2]))
  
list_c <- c()
for (p in seq(c_on)){list_c[[p]] <- c_on[[p]][-1]}
# rename classes with correct reference 
list_c <- gsub("#"," ",list_c)
list_c  <-   paste0(gsub("C1","Class",list_c)," (Ref: Class ",nclass_pro_system, ")")
  
# extract coefficients for each class 
out_list <- c()
  for (c in seq(c_row)) {
    name_class <- list_c[c]
    start_index <- grep(c_row[c] ,logit_pro_system_class)+1
    end_index <- grep("^$" ,logit_pro_system_class)[c]-1
    
    coefs <- logit_pro_system_class[start_index:end_index]
    coef_split <- strsplit(coefs, "[[:space:]]+")
    
    list_to_bind <- c()
    for (p in seq(coef_split)){list_to_bind[[p]] <- coef_split[[p]][-1]}
    
    df_int_R <- do.call("rbind", list_to_bind) %>% data.frame
    df_int_R$calss <- name_class
    out_list[[name_class]] <- df_int_R
    
}
# merge all together 
merged_logit <- do.call("rbind", out_list) %>% data.frame(row.names = NULL)
# rename columns 
names(merged_logit) <- c("Predictors","logit","se","t-value","p-value","class")
  
# order based on class (1 to n)
merged_logit <- merged_logit[order(merged_logit$class),]


single_model_logit <- merged_logit %>%
  mutate_at(vars(logit), as.numeric) %>%
  mutate_at(vars(logit), exp)

# rename all predictors 

single_model_logit$Predictors <- recode(single_model_logit$Predictors, 
                                  AGE6 = "Age",
                                  Q13 = "Education",
                                  Q2 = "Female (Ref: Male)",
                                  REGION = "French-speaking Belgium (Ref: Flanders)",
                                  E_BACK = "Non-native (Ref: Belgian)",
                                  Q22 = "Religious attendance",
                                  CHRIS = "Christian (Ref: None)",
                                  OTHER = "Other Religion (Ref: None)",
                                  FREE = "Free-Thinker (Ref: None)",
                                  KNOW = "Political knowledge",
                                  POL_INT = "Political interest",
                                  TRUST = "Institutional trust",
                                  LMID = "PSC: Low Middle (Ref: Working Class)",
                                  HUPP = "PSC: Higher Middle/Upper (Ref: Working Class)", 
                                  Q64 = "Powerlessness"
)

# drop se and p.value
single_model_logit <- single_model_logit %>% select(-`t-value`)

# round numeric values 
single_model_logit <- single_model_logit %>% mutate_at(vars(logit),list(~ as.numeric(.)))  %>% mutate_at(vars(logit),list(~ round(.,2)))  %>% mutate_at(vars(logit),list(~ format(.,2)))


single_model_logit$logit <- ifelse(single_model_logit$logit >0, paste0(" ",single_model_logit$logit), single_model_logit$logit)
single_model_logit$logit <- paste0(single_model_logit$logit, " (",single_model_logit$se, ")")

single_model_logit <- single_model_logit %>% select(-se)

# reshape such that we have a column of coefficients and z.value for each nested model 
single_model_logit_reshaped <- dcast(melt(single_model_logit, id.vars=c("class", "Predictors")), Predictors~variable+class)

string_order <- c("Age",
                  "Education",
                  "Female (Ref: Male)",
                  "Non-native (Ref: Belgian)",
                  "French-speaking Belgium (Ref: Flanders)",

                  "PSC: Low Middle (Ref: Working Class)",
                  "PSC: Higher Middle/Upper (Ref: Working Class)",
                  
                  "Christian (Ref: None)",
                  "Free-Thinker (Ref: None)", 
                  "Other Religion (Ref: None)",
                  
                  "Political interest",
                  "Institutional trust",
                  "Powerlessness"
)

single_model_logit_reshaped <- single_model_logit_reshaped[match(string_order, single_model_logit_reshaped$Predictors),]

# re-order based on nested model progression 
# e.g. demo first, then model with progressively increasing number of predictors 

single_model_logit_reshaped <- single_model_logit_reshaped %>% select(Predictors, 
                                                          "logit_Class 1 (Ref: Class 4)",
                                                          "p-value_Class 1 (Ref: Class 4)",
                                                          
                                                          "logit_Class 5 (Ref: Class 4)",
                                                          "p-value_Class 5 (Ref: Class 4)",
                                                          
                                                          "logit_Class 2 (Ref: Class 4)",
                                                          "p-value_Class 2 (Ref: Class 4)",

                                                          "logit_Class 3 (Ref: Class 4)",
                                                          "p-value_Class 3 (Ref: Class 4)",
                                                          everything())


# page setting for setting to landscape
sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 8.3, height = 11.7),
  type = "continuous",
  page_margins = page_mar()
)

### 1. Radicals                  [1]
### 2. Non-populists Radical     [3]
### 3. Pluralist Antagonist      [4]
### 4. Pro-system                [REFERENCE]    
### 5. Non-dogmatic radical      [2] 
### 6. Disaffected moderates     [5]

library(flextable)
data_3_step <- single_model_logit_reshaped %>% 
  #select(-se, -`t-value`) %>%
  flextable() %>%
  add_header_row(values = c("", "Radical", "Non-dogmatic Radical", "Non-populist Radical", "Pluralist Antagonist","Disaffected moderate"),
  colwidths = c(1, 2, 2, 2, 2, 2)) %>%
  align(i = 1, part = "header", align = "center") %>%
  flextable::compose(i=2, j=c(2,4,6,8,10),
    part = "header",
    value = as_paragraph(
      "Odds Ratio"
    )
  ) %>%
  flextable::compose(i=2, j=c(3,5,7,9,11),
                     part = "header",
                     value = as_paragraph(
                       "p-value"
                     )
  )

saveRDS(data_3_step, here("tables", 
                          "table3_3step.rds"))
