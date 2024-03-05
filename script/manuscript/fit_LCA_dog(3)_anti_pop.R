##### -------------------------------------------- ######
##### GENERATES THE IMPUT FILES AND RUN THE MODELS ######
##### -------------------------------------------- ######

##-------- Structure of the script --------##
# 1. Data preparation. 
# 2. Run the LPA models.

library(here)
source(here("script", "data_preparation.R"))
source(here("script","toolbox", "function_mplus_syntax_lpa.R"))

# select only the variable that we are going to use for Mplus

mplus_selected <- selection_cleaned[!is.na(selection_cleaned$w_age_bel),]

mplus_selected <- mplus_selected %>%select(
  # respondent ID
  rid,
  # sampling weights
  w_age_bel,
  # antagonism
  q76_1, 
  q76_2, 
  # dogmatism
  q77_1, 
  q77_2, 
  q77_3,
  # populism
  all_of(pop)) %>%
  mutate_all(as.numeric) %>% 
  mutate_at(vars(q76_1,
                 q76_2,
                 all_of(pop),
                 q77_1,
                 q77_2,
                 q77_3
  ), list(~scale(.,center=TRUE, scale = TRUE)))%>% 
  mutate_all(as.numeric)%>%
  data.frame() 


### Run-batch the LPA models ###

##### MODEL SPECIFICATIONS #####
overall_model <- "
%OVERALL%
!!! dogmatism !!!
DOG BY Q77_1 q77_2 Q77_3;
!!! antagonism !!!
ANTI BY q76_1 q76_2;
!!! populism !!!
POP BY Q67_1 Q67_2 Q67_3 Q67_4 Q67_5;
"

# LPA model specification 
covariances <- c(0)              # no covariance between latent indicators 
residuals_latent <- c("fixed")   # fixed residual across classes 
var_lat <-c(0)                   # variance set to 0 for non-parametric modelling
classes <- c(1:9)
grid <- expand.grid(var_lat,covariances,residuals_latent)
names(grid) <- c("var_lat","covariances","residuals_latent")

# generate input and RUN
name_class_model <- "DOG_3_ANTI_POP_"

run_LCA(grid_combination=grid,
        name_class_model=name_class_model,
        overall_model=overall_model,
        run=FALSE,
        n_classes = classes,
        variables=c("POP", "ANTI", "DOG"),
        latent=c(TRUE, TRUE, TRUE),
        manifests = c(
          "Q77_1",
          "Q77_2",
          "Q77_3",
          "Q76_1",
          "Q76_2",
          "Q67_1",
          "Q67_2",
          "Q67_3",
          "Q67_4",
          "Q67_5" 
        ),
        s_weights = "w_age_bel;",
        starts="STARTS = 500 125 0;"
)
