
prepare_for_modeling = function(in_data ){
  
  # remove duplicate columns (this is not thorough, one column is just dropped)
  in_data = in_data[, !duplicated(colnames(in_data))]
  
  in_data %>% 
    # remove unneeded columns
    select(-name        
           , -document) %>% 
    # rename any columns which have protected names (see ?make.names)
    rename_at(vars(matches('(^if)|(^else)|(^repeat)|(^while)|(^function)|(^for)|(^in)|(^next)|(^break)|(^true)|(^false)|(^null)'))
              , funs(sprintf('x_%s', .)))
}

prepare_for_modeling_we = function(in_data ){
  
  in_data %>% 
    # remove unneeded columns
    select(-name) %>% 
    # rename any columns which have protected names (see ?make.names)
    rename_at(vars(matches('(^if)|(^else)|(^repeat)|(^while)|(^function)|(^for)|(^in)|(^next)|(^break)|(^true)|(^false)|(^null)'))
              , funs(sprintf('x_%s', .)))
}
