# Code in this file is adapted from --- 
# https://github.com/r-lib/roxygen2/pull/922#issue-321632922 

#' @export
#' @name Person
#' 
#' @title 
#' R6 class for a person
#' 
#' @description 
#' Description of the class.
#' 
#' @details 
#' Details of the class.
#' 
#' @section Public fields:
#' * `name`: first name.
#' * `hair`: hair color.
Person <- R6::R6Class(
  "Person",
  public = list(
    name = NULL,
    hair = NULL,
    
    #' @details
    #' Create a new `Person` object.
    #' 
    #' @param name First name.
    #' @param hair Hair color, if known.
    #' @return A person object.
    #' 
    #' @examples
    #' Person$new("Ann", "black")
    initialize = function(name = NA, hair = NA) {
      self$name <- name
      self$hair <- hair
      self$greet()
    },
    
    #' @details
    #' Set new hair color.
    #' 
    #' @param val The new hair color.
    #' @return The new color, invisibly.
    #' 
    #' @examples
    #' ann <- Person$new("Ann", "black")
    #' ann$hair
    #' ann$set_hair("red")
    #' ann$hair
    set_hair = function(val) {
      self$hair <- val
    },
    
    #' @details
    #' Make the person greet.
    #' 
    #' @examples
    #' ann <- Person$new("Ann", "black")
    #' ann$greet()
    greet = function() {
      cat(paste0("Hello, my name is ", self$name, ".\n"))
    }
  )
)
