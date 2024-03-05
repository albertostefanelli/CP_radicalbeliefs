##### -------------------------------------------------- ######
##### APPENDIX FOR BNES ONTOLOGICAL RADICAL BELIEF PAPER ######
##### -------------------------------------------------- ######

##-------- Structure of the script --------##
# 1. Descriptive 
# 2. CFA
# 3. Screeplot 
# 4. Viz multiple class solutions 
# 5. Vote choice: full model table with controls 

#### ENV PREPARATION ####

library(MplusAutomation)
library(ggplot2)
library(purrr)
library(flextable)
library(ggrepel)
library(labelled)
library(sjlabelled)
library(reshape2)
library(here)
library(gtsummary)

source(here("script", "data_preparation.R"))

# sel <- selection_cleaned %>% dplyr::select(
#   rid,
#   age6,
#   q2,  
#   q13,
#   region, 
#   e_back,
#   lmid,
#   hupp,
#   reli4,
#   q64,                          
#   trust,
#   pol_int,
#   q57, 
#   w_age_bel,
#   q76_1, 
#   q76_2, 
#   q77_1, 
#   q77_2, 
#   q77_3,
#   v_pop_ed,
#   all_of(pop))
# 
# summary(sel)

#### 1. Descriptive Statistics ####

v_pop_ed_vote <- selection_cleaned %>% filter(v_pop_ed %in% c("Mainstream", "Populist Left", "Populist Right")) %>%
  mutate_at(vars(v_pop_ed), factor)

descriptive_selected <- v_pop_ed_vote %>% dplyr::select(
  rid,
  age6,
  q2,  
  q13,
  region, 
  e_back,
  lmid,
  hupp,
  reli4,
  q64,                          
  trust,
  pol_int,
  q57, 
  w_age_bel,
  q76_1, 
  q76_2, 
  q77_1, 
  q77_2, 
  q77_3,
  v_pop_ed,
  all_of(pop)) %>%
  mutate_at(vars(q2,region,e_back,reli4,v_pop_ed), labelled::to_factor) %>%
  mutate_at(vars(age6, q13, q57, q76_1, q76_2, q77_1, q77_2, q77_3, q67_1, q67_2, q67_3, q67_4, q67_5), unclass) %>%
  mutate_at(vars(age6, q13, q57, q76_1, q76_2, q77_1, q77_2, q77_3, q67_1, q67_2, q67_3, q67_4, q67_5), as.integer) %>%
  mutate_at(vars(rid), as.integer)

descriptive_selected <- descriptive_selected[!is.na(descriptive_selected$w_age_bel),]
var_label(descriptive_selected$q2) <- "Assigned Sex at Birth"
descriptive_selected$q2 <- factor(descriptive_selected$q2)

data_weighted <- survey::svydesign(ids=~rid, weights=~w_age_bel, data=descriptive_selected)

table_desc <- 
  tbl_summary(
    descriptive_selected,
    include= c("age6", "q13", "q57", "q76_1", "q76_2", "q77_1", "q77_2", "q77_3", "q67_1", "q67_2", "q67_3", "q67_4", "q67_5","q2","pol_int", "region","e_back","reli4","v_pop_ed",
               "pol_int",
               "trust",
               "q64"
               ),
    type = list(c("age6", "q13", "q57", "q76_1", "q76_2", "q77_1", "q77_2", "q77_3", "q67_1", "q67_2", "q67_3", "q67_4", "q67_5","pol_int","trust", "q64") ~ 'continuous2'),
    missing = "no", # don't list missing data separately
    statistic = list(c("age6", "q13", "q57", "q76_1", "q76_2", "q77_1", "q77_2", "q77_3", "q67_1", "q67_2", "q67_3", "q67_4", "q67_5","pol_int","trust", "q64") ~ c("{mean} ({sd})","{median} ({p25}, {p75})", "{min} - {max}"),
                     c("q2","region","e_back","reli4","v_pop_ed") ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 2,
    label = c(age6 ~ "Age (age6)",
              q13 ~ "Education (q13)",
              q57 ~ "Left-Right Orientation (q57)",
              q2 ~ "Sex at birth (q2)",
              pol_int ~ "Political Interest (q36, q37_1, q37_2)",
              trust ~ "Institutional Trust (q66_x)",
              region ~ "Place of Residence (region)",
              e_back ~ "Ethnic background (q18, q19)",
              reli4 ~ "Religious Denomination (q21)",
              q64 ~ "Powerlessness (q64)",
              q76_1 ~"Antagonism (q76_1)", 
              q76_2 ~"Antagonism (q76_2)",
              q77_1 ~"Dogmatism (q77_1)",
              q77_2 ~"Dogmatism (q77_2)",
              q77_3 ~"Dogmatism (q77_3)",
              q67_1 ~"Populism (q67_1)",
              q67_2 ~"Populism (q67_2)",
              q67_3 ~"Populism (q67_3)",
              q67_4 ~"Populism (q67_4)",
              q67_5 ~"Populism (q67_5)",
              v_pop_ed ~ "Radical Vote Choice (q24)"
    )
  ) %>%
  add_n() %>% # add column with total number of non-missing observations
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() 

saveRDS(table_desc, here("tables", "APPENDIX_descriptives.rds"))

#### LPA SCREE PLOT ####

# read all the output for the different number of classes (in this case 1:9)
name_class_moel <- "DOG_3_ANTI_POP"
files <- data.frame(names = list.files(path = here("results_mplus"), full.names = FALSE))
name_folder <- unlist(files %>% dplyr::filter(., grepl(name_class_moel, names)))[[1]]

all_output <- readModels(here("results_mplus",name_folder), what="all", quiet = TRUE)
summarymix <- map(all_output,function(x) x[["summaries"]])
summarymix[[1]] <- cbind(summarymix[[1]], T11_LMR_PValue= NA,T11_VLMR_PValue=NA, Entropy=NA )
summarymix <- map(summarymix, function(x) x[c("Title", "LL", "Parameters", "AIC", "BIC","aBIC", "Entropy", "T11_LMR_PValue","T11_VLMR_PValue")])
summarymix <- as_tibble(do.call(rbind, summarymix))
summarymix$Classes <- seq(1:9)

# plot FIT 
summarymix_w <- summarymix %>% select(Classes, AIC, BIC, aBIC)

long <- summarymix_w %>% tidyr::gather(fit, value, -c(Classes))
  
screenplot <- long %>%
  #filter(!Classes %in% c(7, 9, 8)) %>%
  ggplot(aes(x=Classes, y= value, lty =fit)) +
  geom_line()+
  geom_point() + 
  scale_x_continuous(breaks=min(summarymix$Classes): max(summarymix$Classes))+
  guides(lty=guide_legend(title="Fit Index"))+
  ylab("") +
  theme_classic()

ggsave(here("figures", paste0("APPENDIX_scree",".png")), device="png", width=30, height=20, units = "cm")

#### LPA VIZUALIZATION DIFFERENT CLASSES ####

# set name_class_model to get the extract the right model results from the folder 
name_class_moel <- "DOG_3_ANTI_POP_"
files <- data.frame(names = list.files(path = here("results_mplus"), full.names = FALSE))
name_folder <- unlist(files %>% dplyr::filter(., grepl(name_class_moel, names)))[[1]]

# read all the output for the different number of classes (in this case 2:9)
all_output <- readModels(here("results_mplus",name_folder), quiet = TRUE)


model_results_loop <- data.frame()

for (i in 2:(length(all_output))){
  
  n_classes <- nrow(all_output[[i]]$class_counts$mostLikely)
  temp <- all_output[[i]]$parameters$unstandardized %>%                                            #            
    mutate(model = paste(n_classes, "-Class Model")) %>% 
    filter(LatentClass!="Categorical.Latent.Variables")%>% 
    mutate_at(vars("LatentClass"), as.numeric)
  
  
  temp_p <- as.data.frame(all_output[[i]]$class_counts$modelEstimated$proportion)
  colnames(temp_p) <- paste0("classp")
  
  temp_p <- temp_p %>%  
    mutate(classp = round(classp * 100, 2)) %>%
    mutate("LatentClass" = 1:nrow(temp_p)) %>% mutate_all(as.numeric)
  
  int_results <- left_join(temp, temp_p, by="LatentClass")
  int_results$"Latent Class Proportions" <- paste0("Class ", int_results$LatentClass, " (",int_results$classp, "%",")")
  model_results_loop <- rbind(model_results_loop, int_results)
  
}

model_results <- model_results_loop %>%                                                                 #
  filter(paramHeader == "Means") %>%    
  select(-classp, -LatentClass) %>%    
  filter(param %in% c("DOG","ANTI","POP") ) %>%
  #filter(model %in% paste0(class_to_extract," -Class Model"))   %>%
  select(param, est, se, pval, "Latent Class Proportions", model) %>%
  mutate_at(vars("Latent Class Proportions"), as.character) %>%
  mutate_at(vars(est), as.numeric)

model_results$ci_low_95 <- model_results$est - qt(0.975,df=1619-1)*model_results$se
model_results$ci_upper_95 <- model_results$est + qt(0.975,df=1619-1)*model_results$se

pd <- position_dodge(0.1) # move them .05 to the left and right

model_results %>% 
  # remove the () to ensure that the visualization makes sense
  mutate(`Latent Class Proportions` = gsub("\\s*\\([^\\)]+\\)", "", `Latent Class Proportions`)) %>%
  filter(model %in% paste0(c(4:7)," -Class Model")) %>%
  ggplot(aes(x = param, y = est,    
             color = `Latent Class Proportions`, shape = `Latent Class Proportions`,      
             group = `Latent Class Proportions`, lty = `Latent Class Proportions`)) +  
  geom_point( position=pd) + 
  geom_line(aes(color = `Latent Class Proportions`), position=pd) + 
  geom_errorbar(aes(ymin=ci_low_95, ymax=ci_upper_95), width=.05, position=pd) +
  scale_color_grey(end = .8) + 
  geom_hline(yintercept=0, color="black", linetype = 'dotted') +
  labs(title = "LPA Posterior Means",
       x= "", y = "Latent Mean") +
  facet_grid(. ~ model) +
  theme_classic() + 
  guides(fill=guide_legend(title="Latent Profile"))+
  guides(shape=guide_legend(title="Latent Profile"))+
  guides(color=guide_legend(title="Latent Profile"))+
  guides(lty=guide_legend(title="Latent Profile"))+
  scale_x_discrete(labels =c("Antagonism","Dogmatism", "Populism")) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(here("figures", paste0("APPENDIX_all_classes",".png")), device="png", width=30, height=20, units = "cm")


#### VOTE CHOICE MODEL #####

# extract class assignment and add to dataframe 
source(here("script", "manuscript", "extractclassassignment_LCA_dog(3)_anti_pop.R"))

v_pop_ed_vote <- binded_dataset_class %>% filter(v_pop_ed %in% c("Mainstream", "Populist Left", "Populist Right")) %>%
  mutate_at(vars(v_pop_ed), factor)

v_pop_ed_vote$class_most_likely <- as.factor(v_pop_ed_vote$class_most_likely)
v_pop_ed_vote$class_most_likely <- relevel(v_pop_ed_vote$class_most_likely, ref=4)

### 1. Radicals                  [1]
### 2. Non-populists Radical     [3]
### 3. Anti-system               [4]
### 4. Pro-system                [REFERENCE]    
### 5. Non-dogmatic radical      [2] 
### 6. Disaffected moderates     [5]  

vote_choice_poli <- nnet::multinom(v_pop_ed ~ class_most_likely + 
                                     q2 +  
                                     age6 +  
                                     q13 + 
                                     e_back +
                                     as.factor(q18) +
                                     reli4 +
                                     pol_int +
                                     trust + 
                                     q57, 
                                   weights=w_agev_bel,
                                   data = v_pop_ed_vote)


## Loop to compare whether there are significant differences in the coefficients between the levels of the DV 
# using so called Delta Method

coef <- summary(vote_choice_poli)$coefficients
zvalues <- summary(vote_choice_poli)$coefficients / summary(vote_choice_poli)$standard.errors
se <- summary(vote_choice_poli)$standard.errors
pvalues <- 2*pnorm(q=abs(zvalues), lower.tail=FALSE)


names_predictors <- colnames(coef)
names_dv <- rownames(coef)
mvcov <- vcov(vote_choice_poli)

pvalue_wald <- c()
for (i in 1:length(names_predictors)){
  dv <-1:length(names_dv)
  # min max approach needs to change if more levels of the dv (e.g. >3) are included
  # for now i keep it like this
  beta1 <- paste(min(names_dv[dv]), names_predictors[i],sep=":")
  beta2 <- paste(max(names_dv[dv]), names_predictors[i],sep=":")
  int_vcov <- mvcov[c(beta1, beta2),c(beta1, beta2)]
  # formula is Var(beta1) + var(beta2) -2*cov(beta1, beta2)
  int_se <- sqrt(int_vcov[1,1] + int_vcov[2,2] -2*int_vcov[1,2])
  # formula is (beta1 - beta 2)/se
  wald.z <- (coef[min(names_dv[dv]),  names_predictors[i]] - coef[max(names_dv[dv]), names_predictors[i]])/int_se
  
  pvalue_wald[i]<- 2*pnorm(abs(wald.z), lower.tail=FALSE)
  
  
}

labels <- sjlabelled::get_labels(v_pop_ed_vote$v_pop_ed)


list_to_bind <- c()
for (r in 1:ncol(coef)){
  name_predictor <- colnames(coef)[r]  
  list_to_bind[[r]] <- cbind(logit=coef[,r], se=se[,r], pvalues=pvalues[,r], dv_levels=rownames(coef), name_predictor)
  
}

vote_poli <- data.frame(do.call('rbind',list_to_bind), row.names = NULL)

vote_poli <- vote_poli %>% mutate_at(vars(logit,pvalues),list(~ as.numeric(.)))  %>% mutate_at(vars(logit,pvalues),list(~ round(.,2)))
vote_poli <- vote_poli %>% mutate_at(vars(logit,pvalues),list(~ as.numeric(.)))  %>% mutate_at(vars(logit,pvalues),list(~ format(.,2)))

vote_poli <- vote_poli %>% mutate_at(vars(se),list(~ as.numeric(.)))  %>% mutate_at(vars(se),list(~ round(.,2)))
vote_poli <- vote_poli %>% mutate_at(vars(se),list(~ as.numeric(.)))  %>% mutate_at(vars(se),list(~ format(.,2)))

vote_poli$logit <- ifelse(vote_poli$logit >0, paste0(" ",vote_poli$logit), vote_poli$logit)
vote_poli$logit <- paste0(vote_poli$logit, " (",vote_poli$se, ")")

vote_poli <- vote_poli %>% select(-se)

vote_poli$dv_levels <- recode(vote_poli$dv_levels, 
                              `Invalid` = "Invalid (Ref: Mainstream)", 
                              `novote` = "No vote (Ref: Mainstream)",
                              `Populist Left`= "Populist Left (Ref: Mainstream)",
                              `Populist Right`= "Populist Right (Ref: Mainstream)"
                              
                              
)


vote_poli <- vote_poli[!vote_poli$name_predictor=="00 - Intercept",]

vote_draft_reshaped <- dcast(melt(vote_poli, id.vars=c("dv_levels", "name_predictor")), name_predictor~variable+dv_levels)

vote_draft_reshaped <- vote_draft_reshaped %>% 
  select(name_predictor,"logit_Populist Right (Ref: Mainstream)", 
         "pvalues_Populist Right (Ref: Mainstream)",
         everything())

vote_draft_reshaped$name_predictor <- recode(vote_draft_reshaped$name_predictor, 
                                             `(Intercept)` = "Intercept", 
                                             class_most_likely2 = "Non-populist Radical (Ref: Pro-system)",
                                             class_most_likely3 = "Pluralist Antagonist (Ref: Pro-system)",
                                             class_most_likely1 = "Radical (Ref: Pro-system)",
                                             class_most_likely5 = "Non-dogmatic Radical (Ref: Pro-system)",
                                             class_most_likely6 = "Disaffected moderate (Ref: Pro-system)",
                                             q2 = "Female (Ref: Male)",
                                             age6 = "Age",
                                             q13 = "Education",
                                             e_backOther = "Non-belgian (Ref: Belgian)",
                                             regionWallonia = "Wallonia (Ref: Flanders)",
                                             `as.factor(q18)2` = "PSC: Low Middle (Ref: Working Class)", 
                                             `as.factor(q18)3` = "PSC: Higher Middle/Upper (Ref: Working Class)",
                                             pol_int = "Political interest",
                                             trust = "Institutional Trust",
                                             q57 = "L-R self-placement",
                                             reli4Christian = "Christian (Ref: None)",
                                             `reli4Free-thinker` = "Free-thinker (Ref: None)",
                                             reli4Others = "Other religions (Ref: None)"
                                             
                                             
)


string_order <- c("Intercept",
                  "Radical (Ref: Pro-system)",
                  "Non-dogmatic Radical (Ref: Pro-system)",
                  "Non-populist Radical (Ref: Pro-system)",
                  "Pluralist Antagonist (Ref: Pro-system)",
                  "Disaffected moderate (Ref: Pro-system)",
                  "Female (Ref: Male)",
                  "Age",
                  "Education",
                  "Non-belgian (Ref: Belgian)",
                  "PSC: Low Middle (Ref: Working Class)", 
                  "PSC: Higher Middle/Upper (Ref: Working Class)",
                  "Political interest",
                  "Institutional Trust",
                  "L-R self-placement",
                  "Christian (Ref: None)",
                  "Free-thinker (Ref: None)",
                  "Other religions (Ref: None)"
)

vote_draft_reshaped <- vote_draft_reshaped[match(string_order, vote_draft_reshaped$name_predictor),]

vote_draft_reshaped$delta <- pvalue_wald

vote_draft_reshaped <- vote_draft_reshaped %>% mutate_at(vars(delta),list(~ as.numeric(.)))  %>% 
  mutate_at(vars(delta),list(~ round(.,2)))  %>% 
  mutate_at(vars(delta),list(~ format(.,2))) |>
  select(-delta)


vote_choice_table <- vote_draft_reshaped %>% 
  flextable() %>%
  add_header_row(values = c("", "DV: Populist Right (Ref: Mainstream)", "DV: Populist Left (Ref: Mainstream)"),
                 colwidths = c(1, 2, 2)) %>%
  align(i = 1, part = "header", align = "center") %>%
  align(j = 5, part = "all", align = "right") %>%
  flextable::compose(i=2, j=c(2,4),
                     part = "header",
                     value = as_paragraph(
                       "Logit"
                     )
  ) %>%
  flextable::compose(i=2, j=c(3,5),
                     part = "header",
                     value = as_paragraph(
                       "p-value"
                     )
                     
  ) %>%
  flextable::compose(i=2, j=c(1),
                     part = "header",
                     value = as_paragraph(
                       ""
                     )
  )

saveRDS(vote_choice_table, here("tables", "APPENDIX_vote_choice.rds"))
