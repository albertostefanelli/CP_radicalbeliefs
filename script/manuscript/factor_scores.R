library(lavaan)
library(here)
source(here("script", "data_preparation.R"))

########### ---------------------------------------------------- ###############
#################  Calculate Factor Scores for several indices #################
########### ---------------------------------------------------- ###############
# Individual fit for each of the factor included in the model

############################  TABLE 1 - CFA ONTOLOGICAL ################################
# the table resulting from this script is hard-wired in the rmarkdown document 
# results have been copied and pasted manually 

selection_cleaned_cfa <- selection_cleaned %>% 
  haven::zap_labels()

model_ontological='
Populism =~ q67_1 + q67_2 + q67_3 + q67_4 + q67_5
Dogmatism =~  q77_1 + q77_2 + q77_3
Antagonism =~ q76_1 + q76_2
'

fit_model_ontological <- cfa(model_ontological, 
                             data=selection_cleaned_cfa,
                             #sampling.weights = "w_age_bel",
                             missing="ML",
                             estimator="MLR",
                             meanstructure = TRUE)

summary(fit_model_ontological,standardized=TRUE)

loadings <- semoutput::sem_factorloadings(
  fit_model_ontological,
  standardized = TRUE,
  ci = "standardized",
  digits = 3,
  print = FALSE
)

loadings <- loadings %>% select(`Latent Factor`,`Indicator`, Loadings, sig, SE)

loadings$Loadings <- paste0(sprintf("%.2f",round(loadings$Loadings,2)), " (", sprintf("%.2f",round(loadings$SE,3)), ")", loadings$sig)
loadings <- loadings %>% select(-sig, -SE)

fitm_fit_model_ontological <- fitMeasures(fit_model_ontological, c(
  "cfi.robust",
  "rmsea.robust",
  "srmr"), output = "matrix")



correlation <- semoutput::sem_factorcor(fit_model_ontological, print = FALSE)
correlation <- correlation %>% select(`Factor 1`,`Factor 2`, r, sig, SE)
correlation$r <- paste0(sprintf("%.2f",round(correlation$r,2)), " (", sprintf("%.2f",round(correlation$SE,3)), ")", correlation$sig)
correlation <- correlation %>% select(-sig, -SE)
