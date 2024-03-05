##### --------------------------------------------------------------------- ######
##### VIZUALIZATION OF THE LATENT MEANS FOR EACH CLASS (FOR MODEL WITH 6 CLASSES) ######
##### --------------------------------------------------------------------- ######

##-------- Structure of the script --------##
# 1. Extract model results of all estimated model 
# 2. Select best class solutions 
# 3. Visualize 
##-------- Structure of the script --------##

library(reshape2)
library(here)
library(patchwork)
source(here("script", "data_preparation.R"))

#### 1. Extract model results of all estimated model  ####
# set name_class_model to get the extract the right model results from the folder 
name_class_model <- "DOG_3_ANTI_POP_"
files <- data.frame(names = list.files(path = here("results_mplus"), full.names = FALSE))
name_folder <- unlist(files %>% dplyr::filter(., grepl(name_class_model, names)))[[1]]

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

##### 2. Select best class solutions  ####
# meaning picking the best class solution (BIC, LRT, ...)

class_to_extract <- 6

model_results <- model_results_loop %>%                                                                 #
  filter(paramHeader == "Means") %>%    
  select(-classp, -LatentClass) %>%    
  filter(param %in% c("DOG","ANTI","POP") ) %>%
  filter(model %in% paste0(class_to_extract," -Class Model"))   %>%
  select(param, est, se, pval, "Latent Class Proportions", model) %>%
  mutate_at(vars("Latent Class Proportions"), as.character) %>%
  mutate_at(vars(est), as.numeric)


model_results$ci_low_95 <- model_results$est - qt(0.975,df=1619-1)*model_results$se
model_results$ci_upper_95 <- model_results$est + qt(0.975,df=1619-1)*model_results$se

model_results$ci_low_90 <- model_results$est - qt(0.95,df=1619-1)*model_results$se
model_results$ci_upper_90 <- model_results$est + qt(0.95,df=1619-1)*model_results$se

#model_results$est_graph <- ifelse(model_results$pval>0.10,0,model_results$est)

model_results$`Latent Class Proportions` <- recode(model_results$`Latent Class Proportions`,
                                        "Class 1 (16.98%)" = "Radical (16.98%)",
                                        "Class 2 (14.71%)" = "Non-populist Radical (14.71%)",
                                        "Class 3 (13.67%)" = "Pluralist Antagonist (13.67%)",
                                        "Class 4 (22.31%)" = "Pro-system (22.31%)",
                                        "Class 5 (7.28%)" =  "Non-dogmatic Radical (7.28%)",
                                        "Class 6 (25.04%)" = "Disaffected Moderate (25.04%)"
)

model_results$graph_facet <- recode(model_results$`Latent Class Proportions`,
                                    "Radical (16.98%)" = "Integrated Radical", 
                                    "Non-populist Radical (14.71%)" = "Alternative non-mainstream",
                                    "Pluralist Antagonist (13.67%)" ="Alternative non-mainstream",
                                    "Pro-system (22.31%)" = "Mainstream",
                                    "Non-dogmatic Radical (7.28%)" =  "Alternative non-mainstream",
                                    "Disaffected Moderate (25.04%)" = "Mainstream"
)

model_results$graph_facet <- factor(model_results$graph_facet, 
                                    levels=c("Integrated Radical", "Alternative non-mainstream","Mainstream")
                                    )


#### 3. VIZ LATENT MEANS FOR THE DIFFERENT CLASSES ####

pd <- position_dodge(0.1) # move them .05 to the left and right
list_g <- c()

for (g in unique(model_results$graph_facet)){
#g <- "1 - Integrated Radical"
#g <-  "2 - Alternative non-mainstream"

x_pos  <- ifelse(g ==  "Mainstream" , 0.30, ifelse(g ==  "Alternative non-mainstream", 0.31, 0.20) )  
y_pos  <- ifelse(g ==  "Mainstream" , 0.8, ifelse(g ==  "Alternative non-mainstream", 0.15, 0.2) )


list_g[[g]] <- model_results %>% filter(graph_facet==g) %>% 
  ggplot(aes(x = param, y = est,    
          shape = `Latent Class Proportions`,      
          group = `Latent Class Proportions`, lty = `Latent Class Proportions`)) +  
  geom_point(position=pd) + 
  geom_errorbar(aes(ymin=ci_low_95, ymax=ci_upper_95), 
                width=0.07,
                size=0.6,
                position=pd,
                colour="grey60" ) +
  geom_errorbar(aes(ymin=ci_low_90, ymax=ci_upper_90), position=pd,
                width=0.07,
                size=0.6,
                colour="black") +
  geom_line(aes(lty = `Latent Class Proportions`),position=pd) + 
  facet_wrap(~graph_facet,  ncol=2, strip.position = "top") + 
  geom_hline(yintercept=0, color="black", linetype = 'dotted') +
  labs(title = "",
       x= "", y = "") + 
  theme_minimal() + 
  guides(fill=guide_legend(title="Latent Profile"))+
  guides(shape=guide_legend(title="Latent Profile"))+
  guides(color=guide_legend(title="Latent Profile"))+
  guides(lty=guide_legend(title="Latent Profile"))+
  scale_x_discrete(labels =c("Antagonism","Dogmatism", "Populism")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(legend.position = c(x_pos, y_pos)) +
  ylim(-1.2, 1.7)
}


list_g[[2]] <- list_g[[2]] +   labs(title = "",
                     x= "", y = "") 
list_g[[3]] <- list_g[[3]] +   labs(title = "",
                     x= "", y = "") 

(list_g[[1]] | list_g[[2]] | list_g[[3]])  +  plot_layout(guides = 'keep') +
    plot_annotation(
    title = 'Extracted Latent Ideological Profiles',
    subtitle = 'Latent Profile-Confirmatory Factor Analysis (LP-CFA)',
    caption = 'Entropy: 0.79, Lowest N = 110',
    theme = theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5) )
  )

ggsave(here("figures", paste0("figure1_",
                              "latentmeans",
                              ".jpeg")), device="jpeg", width=37*0.90, height=15*0.90, units = "cm", dpi=600)

