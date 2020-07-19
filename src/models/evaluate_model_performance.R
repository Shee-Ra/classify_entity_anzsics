
# this function requires predictions for each class
evaluate_results = function(test_predictions = rf$predictions
                            , test_data = feats$tr
                            , orig_cols = c("name","anzsic_code_lv1")
){
  # browser()
  
  #####################################################
  # Get raw predictions
  #####################################################
  
  # get max probability from prediction matrix
  max_pred = do.call(pmax,as_tibble(test_predictions))
  
  # get column name for max_pred
  max_anzsic = colnames(test_predictions)[apply(as_tibble(test_predictions)
                                     , 1
                                     , which.max)]
  
  # combine precitions with info from modelled data
  result_tbl = test_data %>% 
    # select useful cols from original data (e.g. target)
    select(eval(orig_cols)) %>% 
    # get predictions, convert to a tibble and round predictions
    bind_cols(test_predictions %>% 
                as_tibble() %>% 
                mutate_all(.funs = funs(round(.,2)))
    ) %>% 
    # bind highest prediction
    bind_cols(pred_value = round(max_pred,2)) %>% 
    # bind anzsic prediction
    bind_cols(pred_anzsic = max_anzsic) %>%
    mutate(correct = get(orig_cols[2]) == pred_anzsic) %>% 
    select(eval(orig_cols)
           , pred_anzsic
           , pred_value
           , correct
           , everything())
  
  #####################################################
  # Summarised results
  #####################################################
  
  # calculate total accuracy to result_tbl
  total_acc = result_tbl %>% group_by(correct) %>% tally %>% ungroup %>% mutate(prop = prop.table(n)) %>% .[[2,3]]
  
  # use cutpointr
  # a. include only anzsics which have both correct and incorrect predicitions - cutpointr freaks out otherwise (i.e. this remove small groups)
  in_set = result_tbl %>% group_by(pred_anzsic,correct) %>% tally %>% mutate(p = prop.table(n)) %>% filter(p!=1) %>% pull(pred_anzsic) %>% unique()
  
  # b. run cutpointr on whole dataset
  cutpoints = cutpointr(data = result_tbl %>% 
                          filter(pred_anzsic %in% in_set)
                        , x = pred_value
                        , class = correct
                        , pos_class = TRUE
                        , method = maximize_metric
                        , metric = accuracy # I use accuracy, but there are other metrics
  )
  
  return(list(result_tbl = result_tbl
              , total_accuracy = total_acc
              , total_accuracy_w_cutpoint = cutpoints$accuracy
              , cutpoint_data = cutpoints))
  
} 








# unused code:
#
# use "subgroup = pred_anzsic" in function "cutpointr" (part b) for subgroup evaluation (i.e. a cutpoint per anzsic)
#
# # calculate accuracy per subgroup (anzsic) when a cutoff is used
# total_acc_with_cutpoint = result_tbl %>% 
#   left_join(cutpoints_pc %>% 
#               select(subgroup
#                      , optimal_cutpoint)
#             , by = c( "pred_anzsic" = "subgroup")) %>% 
#   mutate(apply_cutpoint = case_when(pred_value >= optimal_cutpoint ~ 1 # use cutoff to assign T/F
#                                     , is.na(optimal_cutpoint) ~ 1      # if no cutpoint, assign all T
#                                     , T ~ 0)
#          , correct_cutpoint = case_when(anzsic_code_lv1 == pred_anzsic & apply_cutpoint == 1 ~ 1
#                                         , T ~ 0)) %>% 
#   group_by(correct_cutpoint) %>% 
#   tally %>%
#   mutate(prop_correct = prop.table(n)) %>% .[[2,3]]