library(tidyverse)

column_overlap <- function(col1, col2){
  result <- as.numeric(length(intersect(col1, col2))/length(union(col1, col2)))
  return(result)
}

get_overlap_scores <- function(col, candidates){
  result <- map_dbl(.x = candidates, .f = column_overlap, col1 =  col) %>% set_names(., names(candidates))
  return(result)
}

name_similarity <- function(col1, col2){
  
  result <- adist(col1, col2) %>% as.vector(.)
  
  return(result)
}

get_name_scores <- function(col, candidates){
  
  result <- map_dbl(.x = candidates, .f = name_similarity, col1 = col) %>% set_names(., candidates)
  return(1/(result + 1))
  
}

infer_join_condition <- function(df1, df2, excluding){
  
  # This function determines a join condition for merging dataframes X and Y. For each column C 
  # in X to be included, we iterate through each column of Y and compute a similarity score defined
  # as the sum SS = CO + LD, where CO is the column overlap score (the ratio of the cardinality of 
  # intersection and the cardinality of union) and LD is the Levenshtein edit distance of the column 
  # names. The column from Y with the maximum satisfaction score is paired with C. The conjunction of 
  # the set of all such pairs becomes the join condition.
  
  join_cols <- df1 %>% select_if(.p = !(names(.) == excluding)) %>% names(.)
  
  join_condition <- vector(mode = "character", length = length(join_cols)) %>% set_names(., join_cols)
  
  for(i in seq_along(join_condition)){
    
    overlap_scores <- get_overlap_scores(as_vector(df1[, i]), df2)
    
    name_scores <- get_name_scores(names(df1)[i], names(df2))
    
    similarity_score <- (overlap_scores + name_scores) %>% `[`(. == max(.)) %>% names(.)
    
    join_condition[i] <- similarity_score
  }
  
  return(join_condition)
  
}

square_distance <- function(col1, col2){
  result <- (col1 - col2)**2
  return(result)
}

get_square_distances <- function(col, candidates){
  
  result <- map_dbl(.x = candidates, .f = square_distance, col1 = col)
  return(result)
  
}

infer_comparisons <- function(df, excluding){
  
  # Let's start by defining similarity in this context as
  # SS = min(SD + 1/(1 + LD)).
  
  column_means <- df %>% select(-excluding) %>% summarise_all(mean)
  
  mappings <- vector(mode = "character", length(column_means)) %>% set_names(., names(column_means))
  
  for(col in names(mappings)){
    
    others <- names(mappings) %>% '['(. != col)
    
    square_distances <- get_square_distances(as_vector(column_means[col]), select(column_means, -c(col)))
    name_scores <- get_name_scores(col, names(select(column_means, -c(col))))
    similarity_score <- (square_distances + name_scores) %>% `[`(. == min(.)) %>% names(.)
    mappings[col] <- similarity_score
  }
  return(mappings)
}

add_diff_cols <- function(df, comparisons){
  ref_cols <- names(comparisons)
  test_cols <- unname(comparisons)
  diff_cols <- paste0(ref_cols, "_diff") %>% syms(.)
  new_cols <- set_names(parse_exprs(paste0(ref_cols, " - ", "`", test_cols, "`")), diff_cols) %>% as_list(.)
  result <- df %>% mutate(!!!new_cols)
  return(result)
}
