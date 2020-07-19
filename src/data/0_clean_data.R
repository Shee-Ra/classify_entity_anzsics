library(here)
here("common_libraries.r")

clean_data = function(){
  if (file.exists(here("data","processed","name_and_industry.csv"))){
    # check for cleaned data
    print("Cleaned data exists! No need to clean raw data.")
  } else { # try to clean raw data (if raw data file doesn't exist, function will fail)
    
    tryCatch(
      {
        
        # clean labelled data set (for training)
        read.csv(here("data","raw","Business_establishments__with_trading_name_and_industry_classification.csv")) %>% 
          group_by(Trading.name
                   , Industry..ANZSIC4..code
                   , Industry..ANZSIC4..description) %>% 
          tally %>% 
          # clean column names
          rename(anzsic_code = Industry..ANZSIC4..code
                 , anzsic_desc_lv4 = Industry..ANZSIC4..description
                 , name = Trading.name) %>% 
          ungroup %>%
          # clean business names, remove spaces, lower etc.
          mutate(name = str_squish(name)
                 , name = str_to_lower(name)
                 , name = str_remove_all(name, "\"")) %>% 
          group_by(name) %>% 
          filter(n == max(n)                      # select common classifcation
                 , anzsic_desc_lv4 != 'Vacant Space'  # remove nulls
                 , anzsic_code !=0) %>% 
          ungroup %>% 
          # change ANZSIC to string, clean and extract various granularities
          mutate(anzsic_code_lv4 = str_pad(as.character(anzsic_code)
                                           , side = c("left")
                                           , width = 4)
                 , anzsic_code_lv3 = str_extract(anzsic_code_lv4,"\\d{3}$")
                 , anzsic_code_lv2 = str_extract(anzsic_code_lv4,"\\d{2}$")
                 # lvl 1 from wikipedia: https://en.wikipedia.org/wiki/Australian_and_New_Zealand_Standard_Industrial_Classification
                 , anzsic_code_lv1 = case_when(anzsic_code_lv2 %in% c('01','02','03','04','05') ~ 'A'
                                               , anzsic_code_lv2 %in% c('06','07','08','09','10') ~ 'B'
                                               , anzsic_code_lv2 %in% c('26','27','28','29') ~ 'D'
                                               , anzsic_code_lv2 %in% c('30','31','32') ~ 'E'
                                               , anzsic_code_lv2 %in% c('33','34','35','36','37','38') ~ 'F'
                                               , anzsic_code_lv2 %in% c('39','40','41','42','43') ~ 'G'
                                               , anzsic_code_lv2 %in% c('44','45') ~ 'H'
                                               , anzsic_code_lv2 %in% c('46','47','48','49','50','51','52','53') ~ 'I'
                                               , anzsic_code_lv2 %in% c('54','55','56','57','58','59','60') ~ 'J'
                                               , anzsic_code_lv2 %in% c('62','63','64') ~ 'K'
                                               , anzsic_code_lv2 %in% c('66','67') ~ 'L'
                                               , anzsic_code_lv2 %in% c('69','70') ~ 'M'
                                               , anzsic_code_lv2 %in% c('72','73') ~ 'N'
                                               , anzsic_code_lv2 %in% c('75','76','77') ~ 'O'
                                               , anzsic_code_lv2 %in% c('80','81','82') ~ 'P'
                                               , anzsic_code_lv2 %in% c('84','85','86','87') ~ 'Q'
                                               , anzsic_code_lv2 %in% c('89','90','91','92') ~ 'R'
                                               , anzsic_code_lv2 %in% c('94','95','96') ~ 'S'
                                               , T ~ 'C'
                 )
          ) %>%
          select(name
                 , anzsic_desc_lv4
                 , anzsic_code_lv4
                 , anzsic_code_lv3
                 , anzsic_code_lv2
                 , anzsic_code_lv1) %>% 
          # save cleaned data
          write_csv(path = here("data","processed","name_and_industry.csv")
          )
        
        print("Raw data cleaned")
        
      },
      error=function(cond) {
        message("Raw data does not seem to exist:")
        message("Here's the original error message:")
        message(cond)
      }
    ) 
  }
}
  

