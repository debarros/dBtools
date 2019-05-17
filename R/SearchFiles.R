#' @title Search Files Intersect
#' @description Find filenames that match ALL required regular expressions
#' @param files A character vector with filenames
#' @param include A character vector containing regular expressions
#' @param remove A character vector containing regular expressions
#' @return A character vector that is a subset of \code{files}.  Each element will match all elements of \code{include}, and no
#'   elements of the output will match any elements of \code{remove}
SearchFiles.intersect = function(files,
                                 include = NA_character_,
                                 remove = NA_character_) {
  if (!identical(include, NA_character_)) {
    for (k in include) {
      files = grep(
        pattern = k,
        x = files,
        ignore.case = T,
        value = T
      )
    }
  } # /include

  if (!identical(remove, NA_character_)) {
    for (j in remove) {
      files = grep(
        pattern = j,
        x = files,
        ignore.case = T,
        value = T,
        invert = T
      )
    }
  } # /remove

  return(files)

} # /function



#' @title Search Files Union
#' @description Find filenames that match ANY required regular expressions
#' @param files A character vector with filenames
#' @param include A character vector containing regular expressions
#' @return A character vector that is a subset of \code{files}.  Each element will match at least one element of \code{include}.
SearchFiles.union = function(files, include = NA_character_) {
  if (length(include) == 1) {
    if (is.na(include)) {
      stop("There must be at least one element in the include parameter")
    }
  }

  file.list = vector(mode = "list", length = length(include))
  for (k in 1:length(include)) {
    file.list[[k]] = grep(
      pattern = include[k],
      x = files,
      ignore.case = T,
      value = T
    )
  }

  files = character(0)
  for (i in 1:length(file.list)) {
    files = c(files, file.list[[i]])
  }

  files = unique(files)

  return(files)

} # /function


#' @title Search Files Select
#' @description Find filenames that match some regular expressions and not others
#' @param files A character vector with filenames
#' @param includeAll A character vector containing regular expressions
#' @param includeAny A character vector containing regular expressions
#' @param remove A character vector containing regular expressions
#' @param spaceReplaceAny A character vector containing character strings to use instead of spaces
#' @param andReplaceAny A character vector containing character strings to use instead of the string "and"
#' @return A character vector that is a subset of \code{files}.  Each element will match all elements of \code{includeAll} and at
#'   least one element of \code{includeAny}. No elements of the output will match any elements of \code{remove}.
SearchFiles.select = function(files,
                              includeAll = NA_character_,
                              includeAny = NA_character_,
                              remove = NA_character_,
                              spaceReplaceAny = NA_character_,
                              andReplaceAny = NA_character_) {
  if (length(andReplaceAny) > 0) {
    if (!is.na(andReplaceAny[1])) {
      for (i in 1:length(andReplaceAny)) {
        includeAny = unique(c(
          includeAny,
          gsub(
            pattern = "and",
            replacement = andReplaceAny[i],
            x = includeAny
          )
        ))
      }
    }
  }


  if (length(spaceReplaceAny) > 0) {
    if (!is.na(spaceReplaceAny[1])) {
      for (i in 1:length(spaceReplaceAny)) {
        includeAny = unique(c(
          includeAny,
          gsub(
            pattern = " ",
            replacement = spaceReplaceAny[i],
            x = includeAny
          )
        ))
      }
    }
  }


  if (!identical(includeAny, NA_character_)) {
    files = SearchFiles.union(files, includeAny)
  }

  files = SearchFiles.intersect(files = files,
                                include = includeAll,
                                remove = remove)

  return(files)

} # /function
