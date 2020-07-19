

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
  if (feature_type == 'bow'){
    # bag of words
    source(here('src','features','make_features_bow.R'))
    feats = make_features_bow(tr = data)
    
  } else if (feature_type == '2-gram'){
    # n-gram for n = 2
    source(here('src','features','make_features_bow.R'))
    feats = make_features_bow(tr = data
                              , input_ngram_len = 2L)
  } else {
    # word embeddings
    source(here('src','features','make_features_we.R'))
    feats = make_features_we(in_tr = data)
   
  } 

  #####################################################
  # train, test &/or evaluate model
  #####################################################
  if(model == 'rf'){
    # run rf
    rf = ranger(anzsic_code_lv1 ~ .
                , data = feats$tr %>% prepare_for_modeling()
                , probability = T
                , num.trees = arguments$num.tree  # input from ...
                , mtry = arguments$mtry           # input from ...
                )
    
    # save model
    if(save_model){
      file_name = here("models",paste0("rf_BOW_",lubridate::today(),".RDS")) # file name
      saveRDS(rf, file_name)
    }
    
    # predict on test set
    if(test){
      test_preds = predict(rf, data = feats$test %>%
                             prepare_for_modeling() %>% 
                             select(-anzsic_code_lv1)
                           )$predictions
    }
    
    # combine test results with original data & evaluate model
    source(here('src','models','evaluate_model_performance.R'))
    test_results = evaluate_results(test_predictions = test_preds
                        , test_data = feats$test)  
    
    # save test evaluation results
    if(save_test_results){
      file_name = here("models",paste0("rf_BOW_",lubridate::today(),"_evaluation_data.RDS")) # file name
      saveRDS(test_results, file_name)
    }
    
    return(test_results)

  } else {
    print("nn goes here!")
    
    # save model
    
    # test model
    
    # save test
  }
  
  
}