
# fix starspace

make_wordembeddings = function(tr = read_fst(path = here("data","processed","name.fst"))
                               , tr_make = T       # make data to train on
                               , tr_size = 1000000 # size of data
                               , tr_file = NULL    # input your own data to train on
                               
                
  
){
  browser()
  
  # libraries
  library(pacman)
  p_load(here
         , ruimtehol
         , tidyverse
         , fst)
  
 # prepare cleaned data & save for starspace
  if(tr_make){
    file_name = paste0("starspace_business_names_n",tr_size)
    
    tr %>% 
      sample_n(tr_size) %>% 
      write_csv(path = here("data","starspace",paste0(file_name,".csv")))
    
    file_path = here("data","starspace",paste0(file_name,".csv"))
  } else {
    file_path = tr_file
  }
  
  # train neural net, see inputs here: https://www.rdocumentation.org/packages/ruimtehol/versions/0.2.3/topics/starspace
  sp = starspace(file = file_path
                 , dim = 500     # size of vector produced
                 , minCount = 10 # min occurance
                 , trainMode = 5 # word embeddings
                 ) 
  
  starspace_save_model(sp
                       , file = here("data","starspace",paste0(file_name,".ruimtehol"))
                       , method = "ruimtehol")
  
}