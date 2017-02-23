# R Programming
# Exercise 5: The Sorting Hat
# Feb 14 - 16, 2017
# Author: Jonas Markgraf
# -------------------------------

rm(list = ls())

# 1) students --------------------

funcStudent <- function(name) {
  student <- list(name, courage = sample(x = 1:100, size = 1), ambition = sample(1:100, 1),
                  intelligence = sample(1:100, 1), effort = sample(1:100, 1))
  class(student) <- "student"
  print(student)
}

Harry <- funcStudent("Harry Potter")
Hermine <- funcStudent("Hermine")
Ron <- funcStudent("Ron")

# 2) sorter ----------------------

# generate random 4x4 matrix
my_matrix <- matrix(sample(1:100, 16), nrow = 4, ncol = 4)

# different ways to unlist
a <- unlist(matrix(Harry[2:5], ncol = 1, dimnames = list(names(Harry[2:5]))))
a
b <- unlist(Harry)[2:5]

# function
sort.student <- function(student_name, my_matrix = diag(4)) {
  # Function name: sorter()
  # Purpose: evaluate values of 'student' object, assign students to groups based on values.
  # Function: 
  ##  1) takes values from output in list format, turns them into numerics (matrix), and unlists to vector 'a'
  ##  2) multiplies vector 'a' with 4x4 matrix (by default identity matrix)
  ##  3) identifies maximum value in newly created vector and prints group labels
  ##  User option to only calculate m, d or both statistics
  # Args:
  ##  student_name: object of the class "student"
  ##  my_matrix: must be a 4x4 data frame; by default 4x4 identity matrix
  # Open questions:
  ## - what is the role of "my_matrix"? 
  ### - where does the matrix come from? I set it to the identity matrix by default...
  ### - why do we need to multiply it with "a"?
  ## - we didn't use the "sort" function yet although we were supposed to do that...
  # Author: Jonas Markgraf
  a <- unlist(matrix(student_name[2:5], ncol = 1, dimnames = list(names(student_name[2:5]))))
  vect1 <- a %*% t(my_matrix)
  if(max(vect1) == vect1[1]) {
    print("GRIFFINDOR!")
  } else if(max(vect1) == vect1[2]) {
    print("SLYTHERIN!")
  } else if(max(vect1) == vect1[3]) {
    print("RAVENCLAW!")
  } else {
    print("HUFFLEPUFF!")
  }
}

# alternative solution from JB (shorter!):
sort.student <- function(student_name, my_matrix = diag(4)) {
  a <- unlist(matrix(student_name[2:5], ncol = 1, dimnames = list(names(student_name[2:5]))))
  vect1 <- a %*% t(my_matrix)
  houses <- c("Grif", "Slyth", "Rav", "Huf")
  return(houses[which.max(vect1)])
}
# Examples:

sort.student(Harry)
sort(Harry)

# 3) Modifications -------------------

sort.mod <- function(student_name, my_matrix = diag(4)) {
  a <- unlist(matrix(student_name[2:5], ncol = 1, dimnames = list(names(student_name[2:5]))))
  vect1 <- a %*% t(my_matrix)
  if(max(vect1) == vect1[1]) {
    class(student_name) <- c(class(student_name), "Gryffindor")
  } else if(max(vect1) == vect1[2]) {
    class(student_name) <- c(class(student_name), "Slytherin")
  } else if(max(vect1) == vect1[3]) {
    class(student_name) <- c(class(student_name), "Ravenclaw")
  } else {
    class(student_name) <- c(class(student_name), "Hufflepuff")
  }
  return(student_name)
}

# Examples:
Harry <- sort.mod(Harry)

# alternative solution (JB) - shorter!:

sort.mod <- function(student_name, my_matrix = diag(4)) {
  a <- unlist(matrix(student_name[2:5], ncol = 1, dimnames = list(names(student_name[2:5]))))
  vect1 <- a %*% t(my_matrix)
  houses <- c("Grif", "Slyth", "Rav", "Huf")
  class(student_name) <- c(class(student_name), houses[which.max(vect1)])
  print(paste0[which.max(vect1)], "!")
  return(student_name)
}

# 4) Curfew -----------------------------

Gryffindor_Tower <- new.env()
Black_Lake <- new.env()
Ravenclaw_Tower <- new.env()
Basement <- new.env()

curfew <- function(student_name) {
  if("Gryffindor" %in% class(student_name)) {
    assign(deparse(substitute(student_name)), student_name, envir = Gryffindor_Tower)
  } else if("Slytherin" %in% class(student_name)) {
    assign(deparse(substitute(student_name)), student_name, envir = Black_Lake)
  } else if("Ravenclaw" %in% class(student_name)) {
    assign(deparse(substitute(student_name)), student_name, envir = Ravenclaw_Tower)
  } else {
    assign(deparse(substitute(student_name)), student_name, envir = Basement)
  }
}

# Examples:
curfew(Harry)
ls(Gryffindor_Tower)

# write function method "argus_filch" ---------------------------------------------
## collects all objects of class "student" in global environment
## runs "curfew" function on them
## "curfew" function must run in global environment

# basic structure
eval(parse(text=thisCall), envir = globalenv())

# Jeong's solution
argus_filch <- function(){
  for(i in ls(envir=globalenv())){  # apply to all i in globalenvir
    obj <- get(i, envir=globalenv())  # collect all i in globalenvir
    if("student" %in% class(obj)){  # apply function "curfew" to all object with class "student"
      eval(parse(text=paste0("curfew(", i, ")")), envir=globalenv())  # I don't fully get this part.
    }
  }
}

# failed attempts
ClassElements <- Filter( function(x) "student" %in% class(get(x)), ls())
ClassElements  # only gives character string with names of elements of class "student"...