


aggregate_binary <- function(binary_data) {
  
  individual <- binary_data %>% 
    dplyr::mutate(Correct = ifelse(
      stringr::str_sub(Response, start = 1L, end = 1L) == Truth, TRUE, FALSE
    )) %>% 
    dplyr::group_by(user, Group) %>% 
    dplyr::summarise(
      Actual = sum(Correct), 
      Predicted = sum(Confidence), 
      Total = dplyr::n(), 
      .groups = "drop"
    )
  
  group <- individual %>% 
    dplyr::group_by(Group) %>% 
    dplyr::summarise(
      Group_Pct_Actual = sum(Actual) / (dplyr::n() * mean(Total)), 
      Group_Pct_Predicted = sum(Predicted) / (dplyr::n() * mean(Total)), 
      Adjustment_Needed = Group_Pct_Actual - Group_Pct_Predicted, 
      .groups = "drop"
    )
  
  list(
    individual = individual, 
    group = group
  )
  
}


calculate_range_adjustment <- function(group_pct) {
  
  inv_norm <- qnorm(
    group_pct / 2 + 0.5, 
    mean = 0, 
    sd = 1
  )
  
  (3.29 / inv_norm) / 2
  
}

aggregate_range <- function(range_data) {
  
  individual <- range_data %>% 
    dplyr::mutate(Bounded = dplyr::case_when(
      Truth >= Lower90 & Truth <= Upper90 ~ TRUE, 
      TRUE ~ FALSE
    )) %>% 
    dplyr::group_by(user, Group) %>% 
    dplyr::summarise(
      Bounded = sum(Bounded), 
      Total = dplyr::n(), 
      .groups = "drop"
    )
  
  group <- individual %>% 
    dplyr::group_by(Group) %>% 
    dplyr::summarise(
      Group_Pct = sum(Bounded) / (dplyr::n() * mean(Total)), 
      Adjustment_Needed = calculate_range_adjustment(group_pct = Group_Pct), 
      .groups = "drop"
    )
  
  list(
    individual = individual, 
    group = group
  )
  
}
