

train_model = function(prepare_raw_data = T
                      , n_anzsics_to_sample_ = 2
                      , n_rows_to_sample_per_anzsic_ = 100
                      , feature_type = c('bow','n-gram','word-embeddings')
                      , model = c('rf','nn')
                      , test = TRUE
                      , save_test_results = TRUE
                      , save_model = FALSE
                      ){
  
  # prepare raw data
  if(prepare_raw_data){
    source(here('src','data','0_clean_data.R'))
    clean_data()
  }
  
  # data sampling
  source(here('src','data','1_sample_data.R'))
  data = sample_data(n_anzsics_to_sample = n_anzsics_to_sample_
                     , n_rows_to_sample_per_anzsic = n_rows_to_sample_per_anzsic_)
  
  # generate features
  
  # train model
  
  # test model
  
  # save test
  
  # save model
  
}