##### ------------------------------------------------------------------------------- ######
##### VOTE CHOICE MODELS USING MOST LIKELY CLASS ASSIGMENT (FOR MODEL WITH 6 CLASSES) ######
##### ------------------------------------------------------------------------------- ######

library(here)
library(dplyr)
library(reshape2)
library(officer)
library(ggrepel)
library(ggstatsplot)
library(patchwork)

##-------- Structure of the script --------##
# 1. extract most likely class assignment 
# 2. run multinomial models in nnet 
# 3. visualize results in a dot-wisker

##### 1. extract most likely class assignment  ######

source(here("script","manuscript", "extractclassassignment_LCA_dog(3)_anti_pop.R"))

v_pop_ed_vote <- binded_dataset_class %>% 
  filter(v_pop_ed %in% c("Mainstream", "Populist Left", "Populist Right")) %>%
  mutate_at(vars(v_pop_ed, q18, reli4, e_back, q2), factor) %>% 
  mutate_at(vars(age6, pol_int, trust, q57), list(~scale(.,center=TRUE, scale = TRUE)))
  
# #### NEW MODEL FOR THE DRAFT #####
v_pop_ed_vote$class_most_likely <- as.factor(v_pop_ed_vote$class_most_likely)
v_pop_ed_vote$class_most_likely <- relevel(v_pop_ed_vote$class_most_likely, ref=4)

### 1. Radical \n (Ref: Pro-System)                   [1]
### 2. Non-populists Radical \n (Ref: Pro-System)     [3]
### 3. Pluralist Antagonist \n (Ref: Pro-System)      [4]
### 4. Pro-system                                     [REFERENCE]    
### 5. Non-dogmatic Radical \n (Ref: Pro-System)      [2] 
### 6. Disaffected moderate \n (Ref: Pro-System)s     [5]  

vote_choice_poli <- nnet::multinom(v_pop_ed ~ 
                                     class_most_likely +
                                     q2 +       # gender
                                     age6 +     # age
                                     q13 +      # edu
                                     e_back +   # ethnic background 
                                     q18 +      # social class
                                     reli4 +    # religious denomination 
                                     pol_int +  # polinterest 
                                     trust +    # trust 
                                     q57,       # lr
                                     weights=w_agev_bel,
                                     data = v_pop_ed_vote)


fit_index <- performance::model_performance(vote_choice_poli)
fit_index <- fit_index %>% select(R2_adjusted)
names(fit_index)[1] <- "$R^2$ Adjusted"

sample_size <- paste("N:", length(vote_choice_poli$weights))
fit_index_text <- paste(sample_size, paste(names(fit_index), round(fit_index[1,],3), sep = ": ", collapse = ", "), sep = ", ")


gg_tidy_left <- ggcoefstats(vote_choice_poli, output = 'tidy',exponentiate=TRUE, standardize=NULL, ci_method="normal") %>% 
  filter(str_detect(term, 'class_most_likely')) %>% 
  filter(str_detect(term, 'Left'))

gg_tidy_left$dv <- "Vote for Populist Radical Left (PTB/PVDA) \n (Ref: Mainstream)"

gg_tidy_right <- ggcoefstats(vote_choice_poli, output = 'tidy', exponentiate=TRUE, standardize=NULL,ci_method="normal") %>% 
  filter(str_detect(term, 'class_most_likely')) %>% 
  filter(str_detect(term, 'Right'))


gg_tidy_right$dv <- "Vote for Populist Radical Right (VB, PP) \n (Ref: Mainstream)"

gg_tidy_merged <- rbind(gg_tidy_left,gg_tidy_right)


gg_tidy_merged$term <- recode(gg_tidy_merged$term, 
                             `class_most_likely2_Populist Right` = "Non-populist Radical \n (Ref: Pro-System)",
                             `class_most_likely3_Populist Right` = "Pluralist Antagonist \n (Ref: Pro-System)",
                             `class_most_likely1_Populist Right` = "Radical \n (Ref: Pro-System)",
                             `class_most_likely5_Populist Right` = "Non-dogmatic Radical \n (Ref: Pro-System)",
                             `class_most_likely6_Populist Right` = "Disaffected moderate \n (Ref: Pro-System)",
                             `class_most_likely2_Populist Left` = "Non-populist Radical \n (Ref: Pro-System)",
                             `class_most_likely3_Populist Left` = "Pluralist Antagonist \n (Ref: Pro-System)",
                             `class_most_likely1_Populist Left` = "Radical \n (Ref: Pro-System)",
                             `class_most_likely5_Populist Left` = "Non-dogmatic Radical \n (Ref: Pro-System)",
                             `class_most_likely6_Populist Left` = "Disaffected moderate \n (Ref: Pro-System)"
                             )


gg_tidy_merged$term <- factor(gg_tidy_merged$term, levels=c("Disaffected moderate \n (Ref: Pro-System)",
                                      "Pluralist Antagonist \n (Ref: Pro-System)",
                                      "Non-populist Radical \n (Ref: Pro-System)",
                                      "Non-dogmatic Radical \n (Ref: Pro-System)",
                                      "Radical \n (Ref: Pro-System)"))


gg_tidy_merged$expression <- gsub("beta", "OR", gg_tidy_merged$expression)
gg_tidy_merged$expression <- gsub("== \"8.87e-03\"|== \"5.77e-03\"|== \"8.43e-03\"", "<= \"0.001\"", gg_tidy_merged$expression)

gg_tidy_merged_95 <- gg_tidy_merged


gg_tidy_left <- ggcoefstats(vote_choice_poli, output = 'tidy',exponentiate=TRUE, standardize=NULL, ci_method="normal",  conf.level = 0.90) %>% 
  filter(str_detect(term, 'class_most_likely')) %>% 
  filter(str_detect(term, 'Left'))

gg_tidy_left$dv <- "Vote for Populist Radical Left (PTB/PVDA) \n (Ref: Mainstream)"

gg_tidy_right <- ggcoefstats(vote_choice_poli, output = 'tidy', exponentiate=TRUE, standardize=NULL,ci_method="normal",  conf.level = 0.90) %>% 
  filter(str_detect(term, 'class_most_likely')) %>% 
  filter(str_detect(term, 'Right'))


gg_tidy_right$dv <- "Vote for Populist Radical Right (VB, PP) \n (Ref: Mainstream)"

gg_tidy_merged <- rbind(gg_tidy_left,gg_tidy_right) 


gg_tidy_merged$term <- recode(gg_tidy_merged$term, 
                              `class_most_likely2_Populist Right` = "Non-populist Radical \n (Ref: Pro-System)",
                              `class_most_likely3_Populist Right` = "Pluralist Antagonist \n (Ref: Pro-System)",
                              `class_most_likely1_Populist Right` = "Radical \n (Ref: Pro-System)",
                              `class_most_likely5_Populist Right` = "Non-dogmatic Radical \n (Ref: Pro-System)",
                              `class_most_likely6_Populist Right` = "Disaffected moderate \n (Ref: Pro-System)",
                              `class_most_likely2_Populist Left` = "Non-populist Radical \n (Ref: Pro-System)",
                              `class_most_likely3_Populist Left` = "Pluralist Antagonist \n (Ref: Pro-System)",
                              `class_most_likely1_Populist Left` = "Radical \n (Ref: Pro-System)",
                              `class_most_likely5_Populist Left` = "Non-dogmatic Radical \n (Ref: Pro-System)",
                              `class_most_likely6_Populist Left` = "Disaffected moderate \n (Ref: Pro-System)"
)


gg_tidy_merged$term <- factor(gg_tidy_merged$term, levels=c("Disaffected moderate \n (Ref: Pro-System)",
                                                            "Pluralist Antagonist \n (Ref: Pro-System)",
                                                            "Non-populist Radical \n (Ref: Pro-System)",
                                                            "Non-dogmatic Radical \n (Ref: Pro-System)",
                                                            "Radical \n (Ref: Pro-System)"))


gg_tidy_merged_90 <- gg_tidy_merged |> select(conf.low, conf.high, term, dv) |> dplyr::rename("conf.low.90"=`conf.low`,
                                                                     "conf.high.90"=`conf.high`
                                                                     )


gg_tidy_merged_all <- left_join(gg_tidy_merged_95, gg_tidy_merged_90, by=c("term", "dv"))

pd <- position_dodge(0.1) # move them .05 to the left and right

plot_facet <- ggplot(gg_tidy_merged_all, aes(x = estimate, y=term, label=expression)) + 
  geom_point(aes(estimate)) +
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high), 
                position = pd,
                width=0.07,
                size=0.6,
                colour="grey60" ) +
  geom_errorbar(aes(xmin=conf.low.90, xmax=conf.high.90), 
                position = pd,
                width=0.07,
                size=0.6,
                colour="black") +
  geom_label_repel(parse = TRUE,
                   size = 2, 
                   force_pull   = 0, # do not pull toward data points
                   nudge_y      = 0.05,
                   direction    = "y",
                   angle        = 90,
                   hjust        = 0,
                   segment.size = 0.2,
                   max.iter = 1e4, max.time = 1) +
  theme_classic() +
  geom_vline(xintercept = 1, colour='grey40', linetype="dashed", size=0.5) +
  facet_wrap(~dv, ncol=2) + 
  ggplot2::labs(y = "Latent Profiles") +
  ggplot2::labs(x = "Odds Ratio (OR)") 

plot_facet + plot_annotation(
  title = 'Vote for Radical Parties',
  subtitle = 'Multinomial logistic regression',
  caption = latex2exp::TeX(fit_index_text),
  theme = theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5) )
)

ggsave(here("figures", paste0("figure2_",
                              "votechoice",
                              ".jpeg")), device="jpeg", width=30, height=15, units = "cm", dpi=600)

