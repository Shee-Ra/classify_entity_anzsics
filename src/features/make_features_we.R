# make features with word embeddings

make_features_we = function(in_tr = input_dataframe
                            , sp_model = starspace_load_model(here("data","starspace","starspace_business_names_n1e+05.ruimtehol"))
                            , feat_spelling_mistakes = F
                            , feat_bow = F  # placeholder for WIP
                            ){
  # libraries
  library(pacman)
  p_load(here
         , ruimtehol
         , tidyverse
         , magrittr
         , hunspell
         , janitor)
  
  # browser()
  
  # features: make word embeddings
  sp = starspace_embedding(object = sp_model
                           , x = in_tr$name) %>% 
    as_tibble %>% 
    clean_names
    
  # features: spelling mistakes
  sm = in_tr %>% 
    rowwise() %>% 
    mutate(n_mistakes = length(hunspell(name
                          , dict = "en_AU")[[1]])
           ) %>%
    select(n_mistakes)
  
  # features: bag of words
  bow = rep(0, dim(in_tr)[1]) # placeholder: fix
  
  # combine
  if(feat_spelling_mistakes & feat_bow){
    xy = in_tr %>%  
      bind_cols(sp) %>% 
      bind_cols(sm) %>% 
      bind_cols(bow)
  } else if (feat_spelling_mistakes){
    xy = in_tr %>%   
      bind_cols(sp) %>% 
      bind_cols(sm)
  } else if (feat_bow) {
    xy = in_tr %>%  
      bind_cols(sp) %>%
      bind_cols(bow)
  } else {
    xy = in_tr %>%  
      bind_cols(sp)
  }
  
  # create test / train sets
  samp = rsample::initial_split(data = xy
                                , prop = .60
                                , strata = anzsic_code_lv1
  )
  
  
  # return test and train set
  list(test = testing(samp)
       , tr = training(samp)
  )

}

