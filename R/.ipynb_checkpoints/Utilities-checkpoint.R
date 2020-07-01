library(data.table)
library(mltools)


#' @export
encode_and_bind <- function(frame, feature_to_encode) {
    res <- cbind(iris, one_hot(as.data.table(frame[[feature_to_encode]])))
    return(res)
}

#' @export
remove_features <- function(frame, features) {
  rem_vec <- unlist(strsplit(features, ', '))
  res <- frame[,!(names(frame) %in% rem_vec)]
  return(res)
}

#' @export
apply_function_to_column <- function(frame, list_of_columns, new_col, funct) {
    use_cols <- unlist(strsplit(list_of_columns, ', '))
    new_cols <- unlist(strsplit(new_col, ', '))
    frame[new_cols] <- apply(frame[use_cols], 2, function(x) {eval(parse(text=funct))})
    return(frame)
}

#' @export
get_closest_string <- function(vector_of_strings, search_string) {
    all_dists <- adist(vector_of_strings, search_string)
    closest <- min(all_dists)
    res <- vector_of_strings[which(all_dists == closest)]
    return(res)
}
view rawutilities.R 