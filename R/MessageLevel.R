# MessageLevel.R
# Message Level print




#' @title MLprint
#' @description print the message level
#' @param message The message to be printed (or not).  It's a character vector of length 1.
#' @param ML integer vector length 2.  The first element indicates how deep to print messages.  The second element indicates how
#'   many layers deep already.
#' @param indent integer length 1 - how many spaces to indent at each level.  Defaults to 2.
#' @param indentCharacter character length 1, nchar 1, the character to use for indenting messages
#' @return Nothing gets returned
#' @note This function decides whether or not to print a message, and how much to indent it.  Whenever the ML parameter is passed
#'   between functions, the first element should remain constant, and the second element should be increased by 1.
MLprint = function(message,
                   ML = c(1, 1),
                   indent = 2,
                   indentCharacter = " ") {
  if (ML[2] < 0) {
    # If the indent level is negative
    stop("The 2nd element of ML can't be less than zero.") # throw an error
  } # /error check

  if (ML[1] >= ML[2]) {
    # If it's not too deep yet
    prefixCnt = indent * ML[2]                             # How long should the indent be?
    prefixVec = rep(indentCharacter, times = prefixCnt)    # Make a vector of the indent
    prefix = paste0(prefixVec, collapse = "")              # Concatenate the indent
    newMessage = paste0(prefix, message)                   # add the indent to the message
    print(newMessage)                                      # print the message
  } # /if it's not too deep

} # /function


#' @title Vector MLprint
#' @description print the message level
#' @param messages The messages to be printed (or not).  Character vector where each element is one message to print.
#' @param ML integer vector length 2.  The first element indicates how deep to print messages.  The second element indicates how
#'   many layers deep already.
#' @param indent integer length 1 - how many spaces to indent at each level.  Defaults to 2.
#' @param indentCharacter character length 1, nchar 1, the character to use for indenting messages
#' @return Nothing gets returned
#' @note This function acts as a vectorized version of MLprint
vMLprint = function(messages,
                    ML = c(0, 0),
                    indent = 2, indentCharacter = " ") {
  if (ML[2] < 0) {
    # If the indent level is negative
    stop("The second element of ML cannot be less than zero.") # throw an error
  } # /error check

  if (ML[1] >= ML[2]) {
    # If it's not too deep yet
    for (i in 1:length(messages)) {
      # for each message
      thisMessage = messages[i]                                # select the message
      MLprint(thisMessage, ML, indent, indentCharacter)                         # call MLprint
    } # /for each message
  } # /if it's not too deep

} # /function


#' @title inc
#' @description Increment the message level
#' @param ML integer vector length 2.  The first element indicates how deep to print messages.  The second element indicates how
#'   many layers deep the processing is when the call occurs.
#' @return ML is returned, but with the second element increased by 1
#' @note This function is used when ML is passed as an argment to another function.  That way, in the called function, the depth
#'   is 1 more than in the calling environment.
inc = function(ML) {
  ML[2] = ML[2] + 1
  return(ML)
} # /function

