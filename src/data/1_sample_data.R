library(here)
here("common_libraries.r")

sample_data = function(n_anzsics_to_sample = 2           # max 16
                       , n_rows_to_sample_per_anzsic = 500 # max = 2000
){
  # browser() # for testing
  
  # read in raw data
  input_dat = read_csv(here("data","processed","name_and_industry.csv")
           , col_types = cols(name = col_character()
                             , anzsic_desc_lv4 = col_character()
                             , anzsic_code_lv4 = col_double()
                             , anzsic_code_lv3 = col_character()
                             , anzsic_code_lv2 = col_character()
                             , anzsic_code_lv1 = col_character()
                             )
           )
  
  
  # sample subset of ANZSICs
  samp_anzsics = input_dat %>% 
    group_by(anzsic_code_lv1) %>% 
    tally() %>% 
    filter(n >= min(n_rows_to_sample_per_anzsic,2000) # samples max out at 2000, otherwise too few lvl1 ANZSIC category will satisfy condition
           ) %>%                                      # makes sure we pick ANZSICs which can satisfy n_rows_to_sample_per_anzsic condiiton
    pull(anzsic_code_lv1) %>% 
    sample(min(n_anzsics_to_sample,16)                # there's only 16 level 1 ANZSICs
           , replace = T) %>%                         # sampling with replacement & unqiue handles case... 
    unique()                                          # ...when n_anzsics_to_sample & n_rows_to_sample_per_anzsic conflict
  
  # sample rows in each ANZSIC category
  samp_data <- input_dat %>% 
    filter(anzsic_code_lv1 %in% samp_anzsics) %>%     # filter for n_anzsics_to_sample
    group_by(anzsic_code_lv1) %>% 
    sample_n(min(n_rows_to_sample_per_anzsic,2000)) %>% 
    select(name,anzsic_code_lv1)
  
  print(paste0('sampled ',min(n_rows_to_sample_per_anzsic,2000), ' rows from ANZSICs ', samp_anzsics))
  
  return(samp_data)
  
}