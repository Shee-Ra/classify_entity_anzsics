library(here)
source(here("common_libraries.r"))

# generate features for "bag of words"
make_features_bow = function(tr = input_dataframe
                             , input_ngram_len = 1L
                             , in_stem = F          
                             , in_weights = T 
                             ){
  # browser()
  source(here('src','features','helper_functions.R')) # load helper functions

  # tokenise words
  # read more here: https://www.rdocumentation.org/packages/quanteda/versions/2.1.0/topics/tokens
  tr_tok = tokens(tr$name       # select name column
                  , remove_punct = T
                  , preserve_tags = F        # ignore social media tags
                  , remove_symbols = T 
                  , remove_numbers = T 
                  , split_hyphens = T
                  , ngrams = input_ngram_len # build n-grams
                  ) 
  
  # create a document-feature matrix
  # play with dfms here: https://tutorials.quanteda.io/basic-operations/dfm/dfm/
  tr_dfm <- dfm(tr_tok
                , tolower = T                   # make all words lowercase
                , stem = in_stem                # extract stem words only
                , remove = stopwords("english") # remove stopwords, e.g. common words that don't add meaning
                , remove_punct = TRUE           # remove punctuation
  )
  
  # some useful commands:
  # featnames(tr_dfm)                   # have a look at dfm values
  # topfeatures(tr_dfm)                 # look at top features
  # textstat_frequency(tr_dfm, n = 10)  # get top 10 + frquency count
  
  # weight dfm by tf-idf (https://quanteda.io/reference/dfm_tfidf.html)
  # do this to down-weight more common words, e.g. pty ltd etc.
  if (in_weights){
    tr_dfm =  dfm_tfidf(x = tr_dfm
                        , scheme_tf = "count"
                        , scheme_df = "inverse"
                        , base = 10
                        , force = F)
  }
  
  # convert dfm to data frame (note: this is time consuming on local machine)
  dfm_df = tr_dfm %>% quanteda::convert(to = "data.frame")
  
  # bind dfm to original data
  xy = tr %>% 
    bind_cols(dfm_df) %>% 
    clean_names() %>% 
    prepare_for_modeling()
    
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

# unused code

# # get stats on word frequency
# nfeats = featnames(tr_dfm) %>% length()      # number of columns
# tr_word_features = textstat_frequency(tr_dfm
#                                       , n = min(nfeats, 1000)
#                                       , force = F)
# 
# # Sort by reverse frequency order
# tr_word_features$feature <- with(tr_word_features,
#                                  reorder(feature
#                                          ,-frequency))