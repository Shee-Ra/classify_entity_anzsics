

train_model = function(prepare_raw_data = T
                      , n_anzsics_to_sample_ = 2
                      , n_rows_to_sample_per_anzsic_ = 100
                      , feature_type = 'bow'  # c('bow','2-gram','word-embeddings')
                      , model = 'rf' #c('rf','nn')
                      , test = TRUE
                      , save_test_results = F
                      , save_model = FALSE
                      , ...
                      ){
  
  # browser()
  library(here)
  source(here("common_libraries.r"))
  # load helper functions
  
  # access arguments in ...
  arguments = list(...)
  
  #####################################################
  # prepare raw data
  #####################################################
  if(prepare_raw_data){
    source(here('src','data','0_clean_data.R'))
    clean_data()
  }
  
  #####################################################
  # data sampling
  #####################################################
  source(here('src','data','1_sample_data.R'))
  data = sample_data(n_anzsics_to_sample = n_anzsics_to_sample_
                     , n_rows_to_sample_per_anzsic = n_rows_to_sample_per_anzsic_)
  
  #####################################################
  # generate features
  #####################################################
  source(here('src','features','helper_functions.R')) # load helper functions
  
  if (feature_type == 'bow'){
    # bag of words
    source(here('src','features','make_features_bow.R'))
    feats = make_features_bow(tr = data)
    feats$tr_modified = feats$tr %>% prepare_for_modeling()
    feats$test_modified = feats$test %>% prepare_for_modeling()
    
  } else if (feature_type == '2-gram'){
    # n-gram for n = 2
    source(here('src','features','make_features_bow.R'))
    feats = make_features_bow(tr = data
                              , input_ngram_len = 2L)
    feats$tr_modified = feats$tr %>% prepare_for_modeling()
    feats$test_modified = feats$test %>% prepare_for_modeling()
    
  } else {
    # word embeddings
    source(here('src','features','make_features_we.R'))
    feats = make_features_we(in_tr = data)
    feats$tr_modified = feats$tr %>% prepare_for_modeling_we()
    feats$test_modified = feats$test %>% prepare_for_modeling_we()
    
   
  } 

  #####################################################
  # train, test &/or evaluate model
  #####################################################
  if(model == 'rf'){
    #
    # random forest
    #
    
    # set defaults for model if not input
    if(is.null(arguments$num.trees)){ 
      arguments$num.trees = 500
    }
    if(is.null(arguments$mtry)){
      arguments$num.tree = NULL
    }
    
    # train random forest model
    rf = ranger(anzsic_code_lv1 ~ .
                , data = feats$tr_modified 
                , probability = T
                , num.trees = arguments$num.trees  # input from ...
                , mtry = arguments$mtry           # input from ...
                )
    
    # save model
    if(save_model){
      file_name = here("models",paste0(model,"_",feature_type,"_",lubridate::today(),".RDS")) # file name
      saveRDS(rf, file_name)
      
      # model metadata
      model_metadata = list("rf_num.trees" = arguments$num.tree
                            , "rf_mtry" = arguments$mtry
                            , "n_anzsics_sampled" = n_anzsics_to_sample_
                            , "n_rows_sampled_per_anzsic" = n_rows_to_sample_per_anzsic_
                            , "included_anzsics" = cols
                            , "features_used" = feature_type
                            , "model_used" = model
                            , "model_saved" = save_model
                            , "model_tested" = test
                            , "model_test_saved" = save_test_results) 
      file_name_meta = here("models",paste0(model,"_",feature_type,"_",lubridate::today(),"_metadata.RDS")) # file name
      saveRDS(model_metadata , file_name_meta)
      
      
    }
    
    # predict on test set
    if(test){
      test_preds = predict(rf, data = feats$test_modified %>%
                             select(-anzsic_code_lv1)
                           )$predictions
    }
    
  } else {
    #
    # Neural Network
    #
    
    # set defaults for model if not input
    if(is.null(arguments$num.tree)){ 
      arguments$hidden = 3
    }

    # train neural network model
    nn = neuralnet(anzsic_code_lv1 ~ .
                   , data = feats$tr_modified
                   , hidden = arguments$hidden # vector defining layers and nodes    c(7,7,2)
                   , act.fct = "logistic"
                   , linear.output = FALSE     # for classification
                   )
    
    # save model
    if(save_model){
      # model
      file_name = here("models",paste0(model,"_",feature_type,"_",lubridate::today(),".RDS")) # file name
      saveRDS(rf, file_name)
      
      # model metadata
      model_metadata = list("nn_nodes_by_layer" = arguments$hidden
                            , "n_anzsics_sampled" = n_anzsics_to_sample_
                            , "n_rows_sampled_per_anzsic" = n_rows_to_sample_per_anzsic_
                            , "included_anzsics" = cols
                            , "features_used" = feature_type
                            , "model_used" = model
                            , "model_saved" = save_model
                            , "model_tested" = test
                            , "model_test_saved" = save_test_results) 
      file_name_meta = here("models",paste0(model,"_",feature_type,"_",lubridate::today(),"_metadata.RDS")) # file name
      saveRDS(model_metadata , file_name_meta)
      
    }
    
    # predict on test set
    if(test){
      test_preds = predict(nn, feats$test_modified %>% 
                             select(-anzsic_code_lv1)
                           )
      
      # clean predictions
      test_preds %<>% as_tibble()                   # convert matrix to tibble
      cols = table(feats$test_modified$anzsic_code_lv1) %>% 
        names()                                     # get column names
      colnames(test_preds) = cols                   # assign column names
    }

  }
  
  #####################################################
  # evaluate model
  #####################################################
  
  # combine test results with original data & evaluate model
  source(here('src','models','evaluate_model_performance.R'))
  test_results = evaluate_results(test_predictions = test_preds
                                  , test_data = feats$test)  
  
  # save test evaluation results
  if(save_test_results){
    file_name = here("models",paste0(model,"_",feature_type,"_",lubridate::today(),"_evaluation_data.RDS")) # file name
    saveRDS(test_results, file_name)
  }
  
  return(test_results)
}