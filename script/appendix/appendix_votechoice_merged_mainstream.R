##### --------------------------------------------------- ######
##### NESTED VOTE CHOICE MODELS WITH MERGED CLASS 4 AND 6 ######
##### --------------------------------------------------- ######
# GOAL: Robustness of the finding due to a lower number of individuals classified as
# Pro-System in Wallonia. Results are more or less the same. 
# Main difference is that we now have a negative coefficient for the non-populist radical (before was insignificant).
# This is because the Disaffected Moderate profile has populism above the sample mean so merging the two profile makes the contrast is stronger. 
# NOT INCLUDED

#### ENV PREPARATION ####

library(here)
library(dplyr)
library(labelled)
library(lavaan)
library(MplusAutomation)
library(ggplot2)
library(officer)
library(data.table)
library(flextable)

source(here("script_paper", "extractclassassignment_LCA_dog(3)_anti_pop.R"))

v_pop_ed_vote <- binded_dataset_class %>% filter(v_pop_ed %in% c("Mainstream", "Populist Left", "Populist Right")) %>%
  mutate_at(vars(v_pop_ed), factor)

### 1. Radicals                  [1]
### 2. Non-populists Radical     [3]
### 3. Pluralist Antagonist      [4]
### 4. Pro-system                [REFERENCE]    
### 5. Non-dogmatic radical      [2] 
### 6. Disaffected moderates     [5]  


# #### NEW MODEL FOR THE DRAFT #####
v_pop_ed_vote$class_most_likely <- ifelse(v_pop_ed_vote$class_most_likely==6, 4, v_pop_ed_vote$class_most_likely)
v_pop_ed_vote$class_most_likely <- as.factor(v_pop_ed_vote$class_most_likely)
v_pop_ed_vote$class_most_likely <- relevel(v_pop_ed_vote$class_most_likely, ref=4)

v_pop_ed_vote <- v_pop_ed_vote %>% filter(v_pop_ed %in% c("Mainstream", "Populist Left", "Populist Right")) %>%
  mutate_at(vars(v_pop_ed), factor)

######## NESTED MODELS ########

####  2. run multinomial models in nnet
# To run:
# 1. 
# 2. 
# 3. 
# 4. 
# 5. 

# VOTE CHOICE BASELINE WITH PROFILES # 

vote_choice_class <- nnet::multinom(v_pop_ed ~ class_most_likely,
                                    weights=w_agev_bel,
                                    data = v_pop_ed_vote)


coef <- summary(vote_choice_class)$coefficients
zvalues <- summary(vote_choice_class)$coefficients / summary(vote_choice_class)$standard.errors
labels <- sjlabelled::get_labels(v_pop_ed_vote$v_pop_ed)

list_to_bind <- c()
for (r in 1:ncol(coef)){
  name_predictor <- colnames(coef)[r]  
  list_to_bind[[r]] <- cbind(logit=coef[,r], zvalues=zvalues[,r], dv_levels=rownames(coef), name_predictor)
  
}

vote_profiles <- data.frame(do.call('rbind',list_to_bind), row.names = NULL)
vote_profiles$model <- "profiles"

# VOTE CHOICE DEMO # 

vote_choice_class <- nnet::multinom(v_pop_ed ~  class_most_likely + q2 +  # gender
                                      age6 +  # age
                                      edu5 + # edu
                                      e_back,  
                                    weights=w_agev_bel,
                                    data = v_pop_ed_vote)


coef <- summary(vote_choice_class)$coefficients
zvalues <- summary(vote_choice_class)$coefficients / summary(vote_choice_class)$standard.errors
labels <- sjlabelled::get_labels(v_pop_ed_vote$v_pop_ed)

list_to_bind <- c()
for (r in 1:ncol(coef)){
  name_predictor <- colnames(coef)[r]  
  list_to_bind[[r]] <- cbind(logit=coef[,r], zvalues=zvalues[,r], dv_levels=rownames(coef), name_predictor)
  
}

vote_demo <- data.frame(do.call('rbind',list_to_bind), row.names = NULL)
vote_demo$model <- "Demo"


# VOTE CHOICE DEMO + RELIGION # 

vote_choice_class <- nnet::multinom(v_pop_ed ~  class_most_likely + q2 +  # gender
                                      age6 +  # age
                                      edu5 + # edu
                                      e_back +
                                      reli4 +# importance religion ,  
                                      q22,  
                                    weights=w_agev_bel,
                                    data = v_pop_ed_vote)



coef <- summary(vote_choice_class)$coefficients
zvalues <- summary(vote_choice_class)$coefficients / summary(vote_choice_class)$standard.errors
labels <- sjlabelled::get_labels(v_pop_ed_vote$v_pop_ed)

list_to_bind <- c()
for (r in 1:ncol(coef)){
  name_predictor <- colnames(coef)[r]  
  list_to_bind[[r]] <- cbind(logit=coef[,r], zvalues=zvalues[,r], dv_levels=rownames(coef), name_predictor)
  
}

vote_rel <- data.frame(do.call('rbind',list_to_bind), row.names = NULL)
vote_rel$model <- "religion"

# VOTE CHOICE DEMO + RELIGION + POLITICS # 

vote_choice_class <- nnet::multinom(v_pop_ed ~  class_most_likely + q2 +  # gender
                                      age6 +  # age
                                      edu5 + # edu
                                      e_back +
                                      reli4 +# importance religion ,  
                                      q22 +
                                      pol_int,
                                      #know,  
                                    weights=w_agev_bel,
                                    data = v_pop_ed_vote)



coef <- summary(vote_choice_class)$coefficients
zvalues <- summary(vote_choice_class)$coefficients / summary(vote_choice_class)$standard.errors
labels <- sjlabelled::get_labels(v_pop_ed_vote$v_pop_ed)

list_to_bind <- c()
for (r in 1:ncol(coef)){
  name_predictor <- colnames(coef)[r]  
  list_to_bind[[r]] <- cbind(logit=coef[,r], zvalues=zvalues[,r], dv_levels=rownames(coef), name_predictor)
  
}

vote_pol <- data.frame(do.call('rbind',list_to_bind), row.names = NULL)
vote_pol$model <- "politics"

# VOTE CHOICE DEMO + RELIGION + POLITICS + L-R #

vote_choice_class <- nnet::multinom(v_pop_ed ~  class_most_likely + q2 +  # gender
                                      age6 +  # age
                                      edu5 + # edu
                                      e_back +
                                      reli4 +# importance religion ,  
                                      q22 +
                                      pol_int +
                                      #know +
                                      q57,
                                      weights=w_agev_bel,
                                      data = v_pop_ed_vote)


coef <- summary(vote_choice_class)$coefficients
zvalues <- summary(vote_choice_class)$coefficients / summary(vote_choice_class)$standard.errors
labels <- sjlabelled::get_labels(v_pop_ed_vote$v_pop_ed)

list_to_bind <- c()
for (r in 1:ncol(coef)){
  name_predictor <- colnames(coef)[r]  
  list_to_bind[[r]] <- cbind(logit=coef[,r], zvalues=zvalues[,r], dv_levels=rownames(coef), name_predictor)
  
}

vote_lr <- data.frame(do.call('rbind',list_to_bind), row.names = NULL)
vote_lr$model <- "leftright"


# VOTE CHOICE DEMO + RELIGION + POLITICS + L-R + TRUST IN INSTITUTION #

vote_choice_class <- nnet::multinom(v_pop_ed ~  class_most_likely + q2 +  # gender
                                      age6 +  # age
                                      edu5 + # edu
                                      e_back +
                                      reli4 +# importance religion ,  
                                      q22 +
                                      pol_int +
                                      #know +
                                      q57 +
                                      trust,
                                    weights=w_agev_bel,
                                    data = v_pop_ed_vote)



coef <- summary(vote_choice_class)$coefficients
zvalues <- summary(vote_choice_class)$coefficients / summary(vote_choice_class)$standard.errors
labels <- sjlabelled::get_labels(v_pop_ed_vote$v_pop_ed)

list_to_bind <- c()
for (r in 1:ncol(coef)){
  name_predictor <- colnames(coef)[r]  
  list_to_bind[[r]] <- cbind(logit=coef[,r], zvalues=zvalues[,r], dv_levels=rownames(coef), name_predictor)
  
}

vote_trust <- data.frame(do.call('rbind',list_to_bind), row.names = NULL)
vote_trust$model <- "trust"


#####  3. Tabulate results 
# bind all models in a table to display nested models
vote_multi <- rbind(vote_profiles, vote_demo, vote_rel, vote_pol, vote_lr, vote_trust)

# Rename predictors. Use number to order the predictors #

vote_multi$dv_levels <- recode(vote_multi$dv_levels, 
                               `Invalid` = "Invalid (Ref: Mainstream)", 
                               `novote` = "No vote (Ref: Mainstream)",
                               `Populist Left`= "Populist Left (Ref: Mainstream)",
                               `Populist Right`= "Populist Right (Ref: Mainstream)"
                               
                               
)


vote_multi <- vote_multi %>% mutate_at(vars(logit,zvalues),list(~ as.numeric(.)))  %>% mutate_at(vars(logit,zvalues),list(~ round(.,2)))
vote_multi <- vote_multi[order(vote_multi$dv_levels),]

vote_multi_reshaped <- dcast(melt(vote_multi, id.vars=c("dv_levels", "name_predictor","model")), dv_levels+name_predictor~variable+model)

vote_multi_reshaped <- vote_multi_reshaped %>% select(dv_levels, name_predictor, logit_profiles, zvalues_profiles, logit_Demo, zvalues_Demo,logit_religion,zvalues_religion,logit_politics,zvalues_politics, logit_leftright, zvalues_leftright,logit_trust, everything())


vote_multi_reshaped$name_predictor <- recode(vote_multi_reshaped$name_predictor, 
                                             `(Intercept)` = "Intercept", 
                                             class_most_likely2 = "Non-populist Radical (Ref: Pro-system + Disaf.)",
                                             class_most_likely3 = "Pluralist Antagonist (Ref: Pro-system + Disaf.)",
                                             class_most_likely1 = "Radical (Ref: Pro-system + Disaf.)",
                                             class_most_likely5 = "Non-dogmatic Radical (Ref: Pro-system + Disaf.)",
                                             #class_most_likely6 = "Disaffected Moderate (Ref: Pro-system)",
                                             q2 = "Female (Ref: Male)",
                                             age6 = "Age",
                                             edu5 = "Education",
                                             e_backOther = "Non-belgian (Ref: Belgian)",
                                             regionWallonia = "Wallonia (Ref: Flanders)",
                                             know = "Political knowledge",
                                             pol_int = "Political interest",
                                             trust = "Institutional Trust",
                                             q57 = "L-R self-placement",
                                             q22 = "Religious attendance",
                                             reli4Christian = "Christian (Ref: None)",
                                             `reli4Free-thinker` = "Free-thinker (Ref: None)",
                                             reli4Others = "Other religions (Ref: None)"
                                             
                                             
)

vote_multi_left <- vote_multi_reshaped %>% filter(dv_levels == 'Populist Left (Ref: Mainstream)') %>% filter(!name_predictor %in% c("Intercept"))


string_order <- c(
                  "Radical (Ref: Pro-system + Disaf.)",
                  "Non-dogmatic Radical (Ref: Pro-system + Disaf.)",
                  "Non-populist Radical (Ref: Pro-system + Disaf.)",
                  "Pluralist Antagonist (Ref: Pro-system + Disaf.)",
                  #"Disaffected Moderate (Ref: Pro-system)",
                  "Female (Ref: Male)",
                  "Age",
                  "Education",
                  "Non-belgian (Ref: Belgian)",
                  "Political knowledge",
                  "Political interest",
                  "Institutional Trust",
                  "L-R self-placement",
                  "Religious attendance",
                  "Christian (Ref: None)",
                  "Free-thinker (Ref: None)",
                  "Other religions (Ref: None)"
)

vote_multi_left <- vote_multi_left[match(string_order, vote_multi_left$name_predictor),]




sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 8.3, height = 11.7),
  type = "continuous",
  page_margins = page_mar()
)



tab_left <- vote_multi_left %>% 
  select(-dv_levels) %>%
  #as_grouped_data(x = ., groups = c("dv_levels"))  %>%   
  flextable() %>%
  #bold(j = 1, i = ~ !is.na(dv_levels)) %>%
  color(j = grep("logit_Demo",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_Demo)>=1.96, color = "red") %>%
  color(j = grep("logit_religion",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_religion)>=1.96, color = "red") %>%
  color(j = grep("logit_politics",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_politics)>=1.96, color = "red") %>%
  color(j = grep("logit_profiles",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_profiles)>=1.96, color = "red") %>%
  color(j = grep("logit_leftright",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_leftright)>=1.96, color = "red") %>%
  color(j = grep("logit_trust",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_trust)>=1.96, color = "red") %>%
  
  color(j = grep("logit_Demo",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_Demo)<1.96 & abs(zvalues_Demo)>=1.65, color = "blue") %>%
  color(j = grep("logit_religion",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_religion)<1.96 & abs(zvalues_religion)>=1.65, color = "blue") %>%
  color(j = grep("logit_politics",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_politics)<1.96 & abs(zvalues_politics)>=1.65, color = "blue") %>%
  color(j = grep("logit_profiles",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_profiles)<1.96 & abs(zvalues_profiles)>=1.65, color = "blue") %>%
  color(j = grep("logit_leftright",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_leftright)<1.96 & abs(zvalues_leftright)>=1.65, color = "blue") %>%
  color(j = grep("logit_trust",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_trust)<1.96 & abs(zvalues_trust)>=1.65, color = "blue") %>%
  
  add_header_row(values = c("", "Ideol. Profiles", "+ Demo", "+ Religion", "+ Politics", "+ L-R", "+ Inst. Trust" ),
                 colwidths = c(1, 2, 2, 2, 2, 2, 2)) %>%
  align(i = 1, part = "header", align = "center") %>%
  align(j = 6, part = "all", align = "right") %>%
  flextable::compose(i=2, j=c(2,4,6,8,10,12),
                     part = "header",
                     value = as_paragraph(
                       "Logit"
                     )
  ) %>%
  flextable::compose(i=2, j=c(3,5,7,9,11,13),
                     part = "header",
                     value = as_paragraph(
                       "z-value"
                     )
                     
  )  %>%
  flextable::compose(i=2, j=c(1),
                     part = "header",
                     value = as_paragraph(
                       ""
                     )
  )

# autofit() %>%
#   fit_to_width(max_width = 11)  %>%
#   flextable_to_rmd(bookdown=TRUE)
# 
# save_as_docx(
#   "my table 1" = ft1, "my table 2" = ft2, 
#   path = "/path/to/file.docx")



vote_multi_left <- vote_multi_reshaped %>% filter(dv_levels == 'Populist Right (Ref: Mainstream)')  %>% filter(!name_predictor %in% c("Intercept"))


string_order <- c(
                  "Radical (Ref: Pro-system + Disaf.)",
                  "Non-dogmatic Radical (Ref: Pro-system + Disaf.)",
                  "Non-populist Radical (Ref: Pro-system + Disaf.)",
                  "Pluralist Antagonist (Ref: Pro-system + Disaf.)",
                  #"Disaffected Moderate (Ref: Pro-system)",
                  "Female (Ref: Male)",
                  "Age",
                  "Education",
                  "Non-belgian (Ref: Belgian)",
                  "Political knowledge",
                  "Political interest",
                  "Institutional Trust",
                  "L-R self-placement",
                  "Religious attendance",
                  "Christian (Ref: None)",
                  "Free-thinker (Ref: None)",
                  "Other religions (Ref: None)"
)

vote_multi_left <- vote_multi_left[match(string_order, vote_multi_left$name_predictor),]




sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 8.3, height = 11.7),
  type = "continuous",
  page_margins = page_mar()
)



tab_right <- vote_multi_left %>% 
  select(-dv_levels) %>%
  #as_grouped_data(groups = c("dv_levels"))  %>%   
  flextable() %>%
  #bold(j = 1, i = ~ !is.na(dv_levels)) %>%
  color(j = grep("logit_Demo",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_Demo)>=1.96, color = "red") %>%
  color(j = grep("logit_religion",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_religion)>=1.96, color = "red") %>%
  color(j = grep("logit_politics",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_politics)>=1.96, color = "red") %>%
  color(j = grep("logit_profiles",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_profiles)>=1.96, color = "red") %>%
  color(j = grep("logit_leftright",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_leftright)>=1.96, color = "red") %>%
  color(j = grep("logit_trust",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_trust)>=1.96, color = "red") %>%
  
  color(j = grep("logit_Demo",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_Demo)<1.96 & abs(zvalues_Demo)>=1.65, color = "blue") %>%
  color(j = grep("logit_religion",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_religion)<1.96 & abs(zvalues_religion)>=1.65, color = "blue") %>%
  color(j = grep("logit_politics",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_politics)<1.96 & abs(zvalues_politics)>=1.65, color = "blue") %>%
  color(j = grep("logit_profiles",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_profiles)<1.96 & abs(zvalues_profiles)>=1.65, color = "blue") %>%
  color(j = grep("logit_leftright",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_leftright)<1.96 & abs(zvalues_leftright)>=1.65, color = "blue") %>%
  color(j = grep("logit_trust",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_trust)<1.96 & abs(zvalues_trust)>=1.65, color = "blue") %>%
  
  add_header_row(values = c("", "Ideol. Profiles", "+ Demo", "+ Religion", "+ Politics", "+ L-R", "+ Inst. Trust" ),
                 colwidths = c(1, 2, 2, 2, 2, 2, 2)) %>%
  align(i = 1, part = "header", align = "center") %>%
  align(j = 6, part = "all", align = "right") %>%
  flextable::compose(i=2, j=c(2,4,6,8,10,12),
                     part = "header",
                     value = as_paragraph(
                       "Logit"
                     )
  ) %>%
  flextable::compose(i=2, j=c(3,5,7,9,11,13),
                     part = "header",
                     value = as_paragraph(
                       "z-value"
                     )
                     
  )  %>%
  flextable::compose(i=2, j=c(1),
                     part = "header",
                     value = as_paragraph(
                       ""
                     )
  )


flextable::save_as_docx(
  pr_section = sect_properties,
  "Radical Right" = tab_right , 
  "Radical Left" = tab_left,
  path=here("manuscript","appendix_multinomial_vote_choice_agev_merged.docx"))




######## NESTED MODELS [NOT INCLUDED IN THE APPENDIX]########

####  2. run multinomial models in nnet
# To run:
# 1. 
# 2. 
# 3. 
# 4. 
# 5. 

# VOTE CHOICE BASELINE WITH PROFILES # 

vote_choice_class <- nnet::multinom(v_pop_ed ~ class_most_likely,
                                    weights=w_agev_bel,
                                    data = v_pop_ed_vote)


coef <- summary(vote_choice_class)$coefficients
zvalues <- summary(vote_choice_class)$coefficients / summary(vote_choice_class)$standard.errors
labels <- sjlabelled::get_labels(v_pop_ed_vote$v_pop_ed)

list_to_bind <- c()
for (r in 1:ncol(coef)){
  name_predictor <- colnames(coef)[r]  
  list_to_bind[[r]] <- cbind(logit=coef[,r], zvalues=zvalues[,r], dv_levels=rownames(coef), name_predictor)
  
}

vote_profiles <- data.frame(do.call('rbind',list_to_bind), row.names = NULL)
vote_profiles$model <- "profiles"

# VOTE CHOICE DEMO # 

vote_choice_class <- nnet::multinom(v_pop_ed ~  class_most_likely + q2 +  # gender
                                      age6 +  # age
                                      edu5 + # edu
                                      e_back,  
                                    weights=w_agev_bel,
                                    data = v_pop_ed_vote)


coef <- summary(vote_choice_class)$coefficients
zvalues <- summary(vote_choice_class)$coefficients / summary(vote_choice_class)$standard.errors
labels <- sjlabelled::get_labels(v_pop_ed_vote$v_pop_ed)

list_to_bind <- c()
for (r in 1:ncol(coef)){
  name_predictor <- colnames(coef)[r]  
  list_to_bind[[r]] <- cbind(logit=coef[,r], zvalues=zvalues[,r], dv_levels=rownames(coef), name_predictor)
  
}

vote_demo <- data.frame(do.call('rbind',list_to_bind), row.names = NULL)
vote_demo$model <- "Demo"


# VOTE CHOICE DEMO + RELIGION # 

vote_choice_class <- nnet::multinom(v_pop_ed ~  class_most_likely + q2 +  # gender
                                      age6 +  # age
                                      edu5 + # edu
                                      e_back +
                                      reli4 +# importance religion ,  
                                      q22,  
                                    weights=w_agev_bel,
                                    data = v_pop_ed_vote)



coef <- summary(vote_choice_class)$coefficients
zvalues <- summary(vote_choice_class)$coefficients / summary(vote_choice_class)$standard.errors
labels <- sjlabelled::get_labels(v_pop_ed_vote$v_pop_ed)

list_to_bind <- c()
for (r in 1:ncol(coef)){
  name_predictor <- colnames(coef)[r]  
  list_to_bind[[r]] <- cbind(logit=coef[,r], zvalues=zvalues[,r], dv_levels=rownames(coef), name_predictor)
  
}

vote_rel <- data.frame(do.call('rbind',list_to_bind), row.names = NULL)
vote_rel$model <- "religion"

# VOTE CHOICE DEMO + RELIGION + POLITICS # 

vote_choice_class <- nnet::multinom(v_pop_ed ~  class_most_likely + q2 +  # gender
                                      age6 +  # age
                                      edu5 + # edu
                                      e_back +
                                      reli4 +# importance religion ,  
                                      q22 +
                                      pol_int,
                                    #know,  
                                    weights=w_agev_bel,
                                    data = v_pop_ed_vote)



coef <- summary(vote_choice_class)$coefficients
zvalues <- summary(vote_choice_class)$coefficients / summary(vote_choice_class)$standard.errors
labels <- sjlabelled::get_labels(v_pop_ed_vote$v_pop_ed)

list_to_bind <- c()
for (r in 1:ncol(coef)){
  name_predictor <- colnames(coef)[r]  
  list_to_bind[[r]] <- cbind(logit=coef[,r], zvalues=zvalues[,r], dv_levels=rownames(coef), name_predictor)
  
}

vote_pol <- data.frame(do.call('rbind',list_to_bind), row.names = NULL)
vote_pol$model <- "politics"

# VOTE CHOICE DEMO + RELIGION + POLITICS + L-R #

vote_choice_class <- nnet::multinom(v_pop_ed ~  class_most_likely + q2 +  # gender
                                      age6 +  # age
                                      edu5 + # edu
                                      e_back +
                                      reli4 +# importance religion ,  
                                      q22 +
                                      pol_int +
                                      #know +
                                      q57,
                                    weights=w_agev_bel,
                                    data = v_pop_ed_vote)



coef <- summary(vote_choice_class)$coefficients
zvalues <- summary(vote_choice_class)$coefficients / summary(vote_choice_class)$standard.errors
labels <- sjlabelled::get_labels(v_pop_ed_vote$v_pop_ed)

list_to_bind <- c()
for (r in 1:ncol(coef)){
  name_predictor <- colnames(coef)[r]  
  list_to_bind[[r]] <- cbind(logit=coef[,r], zvalues=zvalues[,r], dv_levels=rownames(coef), name_predictor)
  
}

vote_lr <- data.frame(do.call('rbind',list_to_bind), row.names = NULL)
vote_lr$model <- "leftright"


# VOTE CHOICE DEMO + RELIGION + POLITICS + L-R + TRUST IN INSTITUTION #

vote_choice_class <- nnet::multinom(v_pop_ed ~  class_most_likely + q2 +  # gender
                                      age6 +  # age
                                      edu5 + # edu
                                      e_back +
                                      reli4 +# importance religion ,  
                                      q22 +
                                      pol_int +
                                      #know +
                                      q57 +
                                      trust,
                                    weights=w_agev_bel,
                                    data = v_pop_ed_vote)



coef <- summary(vote_choice_class)$coefficients
zvalues <- summary(vote_choice_class)$coefficients / summary(vote_choice_class)$standard.errors
labels <- sjlabelled::get_labels(v_pop_ed_vote$v_pop_ed)

list_to_bind <- c()
for (r in 1:ncol(coef)){
  name_predictor <- colnames(coef)[r]  
  list_to_bind[[r]] <- cbind(logit=coef[,r], zvalues=zvalues[,r], dv_levels=rownames(coef), name_predictor)
  
}

vote_trust <- data.frame(do.call('rbind',list_to_bind), row.names = NULL)
vote_trust$model <- "trust"


#####  3. Tabulate results 
# bind all models in a table to display nested models
vote_multi <- rbind(vote_profiles, vote_demo, vote_rel, vote_pol, vote_lr, vote_trust)

# Rename predictors. Use number to order the predictors #

vote_multi$dv_levels <- recode(vote_multi$dv_levels, 
                               `Invalid` = "Invalid (Ref: Mainstream)", 
                               `novote` = "No vote (Ref: Mainstream)",
                               `Populist Left`= "Populist Left (Ref: Mainstream)",
                               `Populist Right`= "Populist Right (Ref: Mainstream)"
                               
                               
)


vote_multi <- vote_multi %>% mutate_at(vars(logit,zvalues),list(~ as.numeric(.)))  %>% mutate_at(vars(logit,zvalues),list(~ round(.,2)))
vote_multi <- vote_multi[order(vote_multi$dv_levels),]

vote_multi_reshaped <- dcast(melt(vote_multi, id.vars=c("dv_levels", "name_predictor","model")), dv_levels+name_predictor~variable+model)

vote_multi_reshaped <- vote_multi_reshaped %>% select(dv_levels, name_predictor, logit_profiles, zvalues_profiles, logit_Demo, zvalues_Demo,logit_religion,zvalues_religion,logit_politics,zvalues_politics, logit_leftright, zvalues_leftright,logit_trust, everything())


vote_multi_reshaped$name_predictor <- recode(vote_multi_reshaped$name_predictor, 
                                             `(Intercept)` = "Intercept", 
                                             class_most_likely2 = "Non-populist Radical (Ref: Pro-system)",
                                             class_most_likely3 = "Pluralist Antagonist (Ref: Pro-system)",
                                             class_most_likely1 = "Radical (Ref: Pro-system)",
                                             class_most_likely5 = "Non-dogmatic Radical (Ref: Pro-system)",
                                             class_most_likely6 = "Disaffected Moderate (Ref: Pro-system)",
                                             q2 = "Female (Ref: Male)",
                                             age6 = "Age",
                                             edu5 = "Education",
                                             e_backOther = "Non-belgian (Ref: Belgian)",
                                             regionWallonia = "Wallonia (Ref: Flanders)",
                                             know = "Political knowledge",
                                             pol_int = "Political interest",
                                             trust = "Institutional Trust",
                                             q57 = "L-R self-placement",
                                             q22 = "Religious attendance",
                                             reli4Christian = "Christian (Ref: None)",
                                             `reli4Free-thinker` = "Free-thinker (Ref: None)",
                                             reli4Others = "Other religions (Ref: None)"
                                             
                                             
)

vote_multi_left <- vote_multi_reshaped %>% filter(dv_levels == 'Populist Left (Ref: Mainstream)') %>% filter(!name_predictor %in% c("Intercept"))



string_order <- c("Intercept",
                  "Radical (Ref: Pro-system)",
                  "Non-dogmatic Radical (Ref: Pro-system)",
                  "Non-populist Radical (Ref: Pro-system)",
                  "Pluralist Antagonist (Ref: Pro-system)",
                  "Disaffected Moderate (Ref: Pro-system)",
                  "Female (Ref: Male)",
                  "Age",
                  "Education",
                  "Non-belgian (Ref: Belgian)",
                  "Political interest",
                  "Institutional Trust",
                  "L-R self-placement",
                  "Religious attendance",
                  "Christian (Ref: None)",
                  "Free-thinker (Ref: None)",
                  "Other religions (Ref: None)"
)

vote_multi_left <- vote_multi_left[match(string_order, vote_multi_left$name_predictor),]



sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 8.3, height = 11.7),
  type = "continuous",
  page_margins = page_mar()
)



tab_left <- vote_multi_left %>% 
  select(-dv_levels) %>%
  #as_grouped_data(x = ., groups = c("dv_levels"))  %>%   
  flextable() %>%
  #bold(j = 1, i = ~ !is.na(dv_levels)) %>%
  color(j = grep("logit_Demo",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_Demo)>=1.96, color = "red") %>%
  color(j = grep("logit_religion",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_religion)>=1.96, color = "red") %>%
  color(j = grep("logit_politics",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_politics)>=1.96, color = "red") %>%
  color(j = grep("logit_profiles",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_profiles)>=1.96, color = "red") %>%
  color(j = grep("logit_leftright",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_leftright)>=1.96, color = "red") %>%
  color(j = grep("logit_trust",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_trust)>=1.96, color = "red") %>%
  
  color(j = grep("logit_Demo",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_Demo)<1.96 & abs(zvalues_Demo)>=1.65, color = "blue") %>%
  color(j = grep("logit_religion",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_religion)<1.96 & abs(zvalues_religion)>=1.65, color = "blue") %>%
  color(j = grep("logit_politics",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_politics)<1.96 & abs(zvalues_politics)>=1.65, color = "blue") %>%
  color(j = grep("logit_profiles",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_profiles)<1.96 & abs(zvalues_profiles)>=1.65, color = "blue") %>%
  color(j = grep("logit_leftright",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_leftright)<1.96 & abs(zvalues_leftright)>=1.65, color = "blue") %>%
  color(j = grep("logit_trust",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_trust)<1.96 & abs(zvalues_trust)>=1.65, color = "blue") %>%
  
  add_header_row(values = c("", "Ideol. Profiles", "+ Demo", "+ Religion", "+ Politics", "+ L-R", "+ Inst. Trust" ),
                 colwidths = c(1, 2, 2, 2, 2, 2, 2)) %>%
  align(i = 1, part = "header", align = "center") %>%
  align(j = 6, part = "all", align = "right") %>%
  flextable::compose(i=2, j=c(2,4,6,8,10,12),
                     part = "header",
                     value = as_paragraph(
                       "Logit"
                     )
  ) %>%
  flextable::compose(i=2, j=c(3,5,7,9,11,13),
                     part = "header",
                     value = as_paragraph(
                       "z-value"
                     )
                     
  )  %>%
  flextable::compose(i=2, j=c(1),
                     part = "header",
                     value = as_paragraph(
                       ""
                     )
  )

# autofit() %>%
#   fit_to_width(max_width = 11)  %>%
#   flextable_to_rmd(bookdown=TRUE)
# 
# save_as_docx(
#   "my table 1" = ft1, "my table 2" = ft2, 
#   path = "/path/to/file.docx")



vote_multi_left <- vote_multi_reshaped %>% filter(dv_levels == 'Populist Right (Ref: Mainstream)')  %>% filter(!name_predictor %in% c("Intercept"))


string_order <- c("Intercept",
                  "Radical (Ref: Pro-system)",
                  "Non-dogmatic Radical (Ref: Pro-system)",
                  "Non-populist Radical (Ref: Pro-system)",
                  "Pluralist Antagonist (Ref: Pro-system)",
                  "Disaffected Moderate (Ref: Pro-system)",
                  "Female (Ref: Male)",
                  "Age",
                  "Education",
                  "Non-belgian (Ref: Belgian)",
                  "Political knowledge",
                  "Political interest",
                  "Institutional Trust",
                  "L-R self-placement",
                  "Religious attendance",
                  "Christian (Ref: None)",
                  "Free-thinker (Ref: None)",
                  "Other religions (Ref: None)"
)

vote_multi_left <- vote_multi_left[match(string_order, vote_multi_left$name_predictor),]




sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 8.3, height = 11.7),
  type = "continuous",
  page_margins = page_mar()
)


tab_right <- vote_multi_left %>% 
  select(-dv_levels) %>%
  #as_grouped_data(x = ., groups = c("dv_levels"))  %>%   
  flextable() %>%
  #bold(j = 1, i = ~ !is.na(dv_levels)) %>%
  color(j = grep("logit_Demo",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_Demo)>=1.96, color = "red") %>%
  color(j = grep("logit_religion",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_religion)>=1.96, color = "red") %>%
  color(j = grep("logit_politics",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_politics)>=1.96, color = "red") %>%
  color(j = grep("logit_profiles",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_profiles)>=1.96, color = "red") %>%
  color(j = grep("logit_leftright",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_leftright)>=1.96, color = "red") %>%
  color(j = grep("logit_trust",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_trust)>=1.96, color = "red") %>%
  
  color(j = grep("logit_Demo",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_Demo)<1.96 & abs(zvalues_Demo)>=1.65, color = "blue") %>%
  color(j = grep("logit_religion",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_religion)<1.96 & abs(zvalues_religion)>=1.65, color = "blue") %>%
  color(j = grep("logit_politics",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_politics)<1.96 & abs(zvalues_politics)>=1.65, color = "blue") %>%
  color(j = grep("logit_profiles",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_profiles)<1.96 & abs(zvalues_profiles)>=1.65, color = "blue") %>%
  color(j = grep("logit_leftright",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_leftright)<1.96 & abs(zvalues_leftright)>=1.65, color = "blue") %>%
  color(j = grep("logit_trust",names(vote_multi_reshaped))-1, i = ~ abs(zvalues_trust)<1.96 & abs(zvalues_trust)>=1.65, color = "blue") %>%
  
  add_header_row(values = c("", "Ideol. Profiles", "+ Demo", "+ Religion", "+ Politics", "+ L-R", "+ Inst. Trust" ),
                 colwidths = c(1, 2, 2, 2, 2, 2, 2)) %>%
  align(i = 1, part = "header", align = "center") %>%
  align(j = 6, part = "all", align = "right") %>%
  flextable::compose(i=2, j=c(2,4,6,8,10,12),
                     part = "header",
                     value = as_paragraph(
                       "Logit"
                     )
  ) %>%
  flextable::compose(i=2, j=c(3,5,7,9,11,13),
                     part = "header",
                     value = as_paragraph(
                       "z-value"
                     )
                     
  )  %>%
  flextable::compose(i=2, j=c(1),
                     part = "header",
                     value = as_paragraph(
                       ""
                     )
  )



sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 8.3, height = 11.7),
  type = "continuous",
  page_margins = page_mar()
)

# flextable::save_as_docx(
#   pr_section = sect_properties,
#   "Radical Right" = tab_right, 
#   "Radical Left" = tab_left,
#   path=here("manuscript","appendix_multinomial_vote_choice_agev_no_know.docx"))





