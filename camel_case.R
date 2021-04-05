f <- function(char, method){
  char %<>%
    str_split(method) %>%
    unlist()
  return(char)
}

to_camel_case <- function(char){
  if (nchar(char) > 1){
    split_char = f(char, '_')
    split_char = f(split_char, '-')
  } else return(char)
  if (tolower(split_char[1]) == split_char[1]){ 
    split_changed_char <<- sapply(split_char[-1], str_to_title)
    return(paste(c(split_char[1], split_changed_char), collapse = ''))
  } else {
    split_changed_char <<- sapply(split_char, str_to_title)
    return(paste(split_changed_char, collapse = ''))
  }
}

to_camel_case <- function(text){
  gsub('[-_](.)','\\U\\1',text,perl=T)
}