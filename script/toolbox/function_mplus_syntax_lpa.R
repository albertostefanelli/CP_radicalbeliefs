require(gt)
require(glue)
require(purrr)
require(ggplot2)
require(filesstrings)

###### ------------------------------------------------------------------- #######
###### mplus_syntax_lpa: generate syntax for different types of LPA models #######
###### ------------------------------------------------------------------- #######

# # input mplus_syntax_lpa
# variances = c(0, 1, free, fixed)
# covariances = c(0,free,fixed)
# residuals_latent = c(free, fixed)
# intercepts = c(free, fixed, zero)
# classes = c(x:y)


mplus_syntax_lpa <- function(variables = variables,
                             latent = latent,
                             manifests=manifests,
                             variances = variances,
                             covariances = covariances,
                             residuals_latent = residuals_latent,
                             intercepts=intercepts,
                             classes=classes
){
  
  #debugging
  # variables<- c("POP", "ANTI", "DOG")
  # latent <- c(TRUE, TRUE, TRUE)
  # manifests <- c(
  #   "Q77_1",
  #   "Q77_2",
  #   "Q77_3",
  #   "Q76_1",
  #   "Q76_2",
  #   "Q67_1",
  #   "Q67_2",
  #   "Q67_3",
  #   "Q67_4",
  #   "Q67_5"
  # )
  # variances <- c(0)
  # covariances<-c("free")
  # residuals_latent<-c("fixed")
  # classes<-c(9)
  # intercepts<-c("zero")
  
  
  manifests <- toupper(manifests)
  variables <- toupper(variables)
  classes <- as.numeric(classes)
  
  # VARIANCE of the latent (and manifest) variables
  vars <- c()
  if (variances==1){
    # this takes into account the fact that single indicator have the variance of the manifest set to 1 but the variance of the latent is estimated.
    # this might not be true so i just skip it copying the same code over the if condition
    for (i in seq(variables)){if(latent[i]){
      vars[[i]] <- paste0(variables[i],"@1",";\n", collapse="")
    }else{
      vars[[i]] <- paste0(variables[i], "(V",variables[i],")",";\n", collapse="")
    }}
    vars <- paste(array(unlist(vars)), collapse = "")
  }else if (variances==0){
    for (i in seq(variables)){if(latent[i]){
      vars[[i]] <- paste0(variables[i],"@0",";\n", collapse="")
    }else{
      vars[[i]] <- paste0(variables[i], "(V",variables[i],")",";\n", collapse="")
    }}
    vars <- paste(array(unlist(vars)), collapse = "")
  }else if (variances=="fixed"){
    vars <- paste0(variables, "(LV",variables,")",";\n", collapse="")
  }else if (variances=="free"){
    vars <- paste0(variables, "(LV",variables,"{C}",")",";\n", collapse="")
  }else {
    message("Error, Check your variance input")
  }
  
  # COVARIANCE between latent factors 
  vectors_comb <- combn(variables, 2)
  covars <- c()
  if (covariances==0){
    covars <- paste(paste0(vectors_comb[1,]," WITH ", vectors_comb[2,],"@0;\n"), collapse = "")
  }else if (covariances=="fixed"){
    covars <- paste(paste(vectors_comb[1,],"WITH", vectors_comb[2,],paste0("(","C",vectors_comb[1,],vectors_comb[2,],");\n")), collapse="")
  }else if (covariances=="free"){
    covars <- paste(paste(vectors_comb[1,],"WITH", vectors_comb[2,],paste0("(","C",vectors_comb[1,],vectors_comb[2,],"{C}",");\n")), collapse="")
  }else {
    message("Error, Check your covariance input")
  }
  
  # residual of the indicator of the latent variable 
  # usually set fixed across classes to decrease model complexity
  
  resi <- c()
  if (residuals_latent=="fixed"){
    resi <- paste0(manifests, "(V",manifests,")",";\n", collapse="")
  }else if (residuals_latent=="free"){
    resi <- paste0(manifests, "(LV",manifests,"{C}",")",";\n", collapse="")   
  }else {
    message("Error, Check your residuals input")
  }
  
  
  unique <- gsub("(.+?)(\\_.*)", "\\1", manifests)
  n_var_each_counstruct <- table(unique)
  
  # get the last indicator of the latent and set it to 0
  # this allows us to estimate the mean of each class and not to have a 0 class 
  var_to_set_to_zero <- c()
  for (i in names(n_var_each_counstruct)){
    if (length(grep(i, manifests))==1){
    }else{
      var_to_set_to_zero[[i]] <- manifests[grep(i, manifests)[length(grep(i, manifests))]]
    }
  }
  
  # in case of manifest variable (instead of LV) do not set the intercept (in this case their mean) to zero
  # FIX ME: need to test this with the new code


  # INTERCEPTS manifest indicators 
  
  int <-c()
  if (intercepts=="fixed"){
    
    # in case of manifest variable (instead of LV) do not set the intercept (in this case their mean) to zero
    manifest_interecepts <- manifests
    for (i in array(unlist(var_to_set_to_zero))){
      manifest_interecepts[grep(i,manifests)] <-  paste0(manifest_interecepts[grep(i,manifests)],"@0")
      
    }
    
    labs <- gsub("[[:punct:]]", "", manifest_interecepts)
    int <-  paste0("[",manifest_interecepts,"]","(int", labs, ")", ";\n", collapse="")
  }else if (intercepts=="free"){
    
    # in case of manifest variable (instead of LV) do not set the intercept (in this case their mean) to zero
    manifest_interecepts <- manifests
    for (i in array(unlist(var_to_set_to_zero))){
      manifest_interecepts[grep(i,manifests)] <-  paste0(manifest_interecepts[grep(i,manifests)],"@0")
      
    }
    
    labs <- gsub("[[:punct:]]", "", manifest_interecepts)
    int <- paste0("[",manifests,"]", "(int",labs,"{C}",")",";\n", collapse="")
  }else if (intercepts=="zero"){
    labs <- gsub("[[:punct:]]", "", manifests)
    labs <- paste0(labs, "0")
    int <- paste0("[",manifests,"@0]", "(int",labs,"{C}",")",";\n", collapse="")
  }else {
    message("Error, Check your interecept input")
  }
  
  # means are freely estimated without any starting values
  means <- c()
  means <- paste0("[",variables,"]", "(M",variables,"{C}",")",";\n", collapse="")
  
  # bind the the class invariant output together
  output <- paste0("!!!! Means LV !!!!\n", means,"!!!! Variances LV !!!!\n", vars,"!!!! Covariances LV !!!!\n", covars,"!!!! Residuals LV !!!!\n", resi, "!!!! Intercepts manifest !!!!\n", int, collapse = "\n")
  
  output_list <- c()
  
  # add the number of each class. 
  # glue substitutes {C} with a number
  for (C in 1:classes){
    output_list[C] <- glue(output)
  }
  
  # intercepts set to 0 for all class apart the last one
  # this allows to estimate the mean for each class 
  # not implemented at this time
  
  #1. Set each latent variable's marker item (1 of the manifest indicator) to zero in all G groups. 
  #2. The latent means in one of the groups (which serves the role of a baseline) are set to zero and all the intercepts of the marker items are declared invariant across groups.
  
  # we go for the first approach and set the marker items to zero
  # for (i in  1:(length(seq(classes))-1)){
  #   for (v in seq(unlist(var_to_set_to_zero))) {
  #     
  #     to_get <- paste0(unlist(var_to_set_to_zero)[v],"@0")
  #     to_replace <- unlist(var_to_set_to_zero)[v]
  #     output_list[i] <- sub(to_get, to_replace,output_list[i])
  #     to_get <- paste0("int", sub("_", "", unlist(var_to_set_to_zero)[v]),"0")
  #     to_replace <- paste0("int", sub("_", "", unlist(var_to_set_to_zero)[v]))
  #     output_list[i] <- sub(to_get, to_replace,output_list[i])
  #     
  #   }
  #   
  # }
  
  # add class labels to the mplus syntax 
  names_classes <- c()
  for (C in 1:classes){
    names_classes[C] <- glue("%c1#{C}%")
  }
  
  # make the mplus syntax more readable by separating the various class specific sections 
  for (C in 1:classes){
    names_classes[C] <- paste("\n\n!!!!!!!!",names_classes[C], "!!!!!!!!",sep="\n")
    output_list[C] <- paste(names_classes[C],output_list[C],sep = "\n\n")
  }
  
  # collapse the output of each class all together
  output_list <- paste(output_list, collapse="")
  return(output_list)
}

#### TESTING FUNCTION 

# class_specific_model <- mplus_syntax_lpa(variables=c("POP", "ANTI", "DOG"),
#                                          latent=c(TRUE,TRUE,TRUE),
#                                          classes=3,
#                                          manifests = c(
#                                            "Q77_1",
#                                            "Q77_2",
#                                            "Q77_3",
#                                            "Q76_1",
#                                            "Q76_2",
#                                            "Q67_1",
#                                            "Q67_2",
#                                            "Q67_3",
#                                            "Q67_4",
#                                            "Q67_5"
#                                          ),
#                                          variances=c("0"),
#                                          covariances=c("0"),
#                                          residuals_latent=c("fixed"),
#                                          intercepts=c("zero") # hard coded
# )
# 
# 
# cat(class_specific_model)




###### -------------------------------------------------- #######
###### run_LCA: run the models using the generated syntax #######
###### -------------------------------------------------- #######


## debugging
# source(here("script_paper", "main_data_preparation.R"))
# covariances <- c(0)
# residuals_latent <- c("fixed")
# var_lat <-c(0)
# n_classes <- c(1:2)
# grid <- expand.grid(var_lat,covariances,residuals_latent)
# names(grid) <- c("var_lat","covariances","residuals_latent")
# grid_combination <- grid
# 
# 
# name_class_model <- "TEST_"
# 
# overall_model <- "
# %OVERALL%
# !!! dogmatism !!!
# DOG BY Q77_1 q77_2 Q77_3;
# !!! antagonism !!!
# ANTI BY q76_1 q76_2;
# !!! populism !!!
# POP BY Q67_1 Q67_2 Q67_3 Q67_4 Q67_5;
# "
# 
# variables=c("POP", "ANTI", "DOG")
# latent=c(TRUE,TRUE,TRUE)
# manifests = c(
#    "Q77_1",
#    "Q77_2",
#    "Q77_3",
#    "Q76_1",
#    "Q76_2",
#    "Q67_1",
#    "Q67_2",
#    "Q67_3",
#    "Q67_4",
#    "Q67_5"
# )
# 
# starts <- "STARTS = 500 125 0;"
# s_weights <- "w_age_bel;"
# run=TRUE

run_LCA <- function(grid_combination=grid_combination,
                    name_class_model=name_class_model,
                    overall_model=overall_model,
                    n_classes=n_classes,
                    variables=variables,
                    latent=latent,
                    manifests=manifests,
                    starts=starts,
                    s_weights=s_weights, #(NULL, weights_variable_name)
                    run=TRUE
                    
){
  
  
  for (r in 1:nrow(grid_combination)){
    #r <- 1
    var <-  as.character(grid_combination[r,"var_lat"])
    cov <- as.character(grid_combination[r,"covariances"])
    res <- as.character(grid_combination[r,"residuals_latent"])
    print("------------")
    print(paste0("Progression grid combinations: ",round(r/nrow(grid_combination),2)*100,"%"))
   
    
     if(is.null(s_weights)){s_weights <- "!WEIGHT=NULL;"}else{
      s_weights <- paste0("WEIGHT=",s_weights)
    }
    
    for (cl in n_classes){
      #cl <- 1 
      name <- paste0(name_class_model,"var_lat=",var, "_covariance=",cov,"_residuals=",res,"_classes=",cl)
      print(paste0("Writing: ",name," (Classes: ",cl,")"))
      
      
      class_specific_model <- mplus_syntax_lpa(variables=variables,
                                               latent=latent,
                                               classes=cl,
                                               manifests = manifests,
                                               variances=var,
                                               covariances=cov,
                                               residuals_latent=res,
                                               intercepts=c("zero") # hard coded
      )
      


      
      
      model_out <- mplusObject(
        TITLE = paste0(name,";"),
        VARIABLE = paste(paste(strwrap(paste("USEVARIABLES = ",paste(names(mplus_selected), collapse=" "),
                                             ";"), 30), collapse = "\n"),
                         s_weights,
                         "IDVARIABLE IS rid;
                          CLASSES=c1(",cl,");",
                   sep = "\n"
        ),
        ANALYSIS = paste(
             "TYPE = MIXTURE;",
             starts,
             "!lrtstarts = 0 0 600 125;",
             "LRTBOOTSTRAP = 10;",
             "STITERATIONS=15;
             !K-1STARTS = 1000 300;
             !STSEED = 134830;
             !H1STARTS = 1000 300;
             ESTIMATOR = MLR;
             PROCESSORS = 8;",
             sep="\n"), 
        MODEL = paste(overall_model, class_specific_model, sep="\n"),
        OUTPUT = "TECH11;", 
        # the savedata is set to use only the number of classes otherwise the filename is too long
        SAVEDATA = paste("SAVE=fscores CPROB;\n", paste0("FILE IS data_export_",cl,".dat;")),
        rdata = mplus_selected
      )
      
      

      model_out_file <- paste0(name,".inp")

      model_out_write_out <- mplusModeler(object = model_out,
                                          modelout = model_out_file,
                                          Mplus_command = "/Applications/Mplus_mac/mplus",
                                          run = 0,
                                          hashfilename = FALSE
      )
      
      
    }
    
    name_base <- sub("_classes*.." ,"" ,name)
    files <- data.frame(names = list.files(path = here(), full.names = FALSE))
    f_rad <- files %>% dplyr::filter(., grepl(name_base, names))
    for (i in 1:length(f_rad)) { file.move(glue(here("{f_rad[,i]}")), here(name_base),overwrite = TRUE) }
    
    
    if(run){
    
    print(paste0("Running all models in: ", name_base))
    
    count <- 1
    for (cl in n_classes){
      name_run_models <- paste0(name_class_model,"var_lat=",var, "_covariance=",cov,"_residuals=",res,"_classes=",cl,".inp")
      print("------")
      print(paste0("Running Class: ", cl, " Total Classes: ", max(n_classes)))
      runModels(target=here(name_base,name_run_models), recursive = FALSE, replaceOutfile="modifiedDate", showOutput = FALSE, Mplus_command = "/Applications/Mplus_mac/mplus")
      print(paste0("(Progression: ", round(count/length(n_classes)*100,2),"%)"))
      
      count <- count + 1
    }

    all_output <- readModels(here(name_base), quiet = TRUE)

    # read all the output for the different number of classes (in this case 1:9)
    all_output <- readModels(here(name_base), what="all", quiet = TRUE)
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
    
    table_gt<-gt(summarymix)
    
    screenplot <- ggplot(data=summarymix, aes(x=Classes, y=BIC)) +
      geom_line(linetype = "dashed")+
      geom_point() + 
      theme_classic()
    
    list_out <- c()
    list_out[["Screen plot"]] <- screenplot
    list_out[["Fit table"]] <- summarymix
    
    ggsave(filename=paste0("screenplot_",name_class_model,".pdf"), plot=screenplot, path=here(name_base))
    gtsave(table_gt, paste0("fit_",name_class_model,".html"), path = here(name_base))
    
    return(list_out)
    }
    else{message("Input files have only been written \nSet run to TRUE to also run the models")}
    
  }
  
}
