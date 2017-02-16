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

# 2) sorter ----------------------

# generate random 4x4 matrix
my_matrix <- matrix(sample(1:100, 16), nrow = 4, ncol = 4)

# different ways to unlist
a <- unlist(matrix(Harry[2:5], ncol = 1, dimnames = list(names(Harry[2:5]))))
b <- unlist(Harry)[2:5]

# function
sorter <- function(student_name, my_matrix = diag(4)) {
  # Function name: sorter()
  # Purpose: evaluate values of 'student' object, assign students to groups based on values.
  # Function: 
  ##  1) takes values from output in list format, turns them into numerics (matrix), and unlists to vector 'a'
  ##  2) multiplies vector 'a' with 4x4 matrix (by default identity matrix)
  ##  3) identifies maximum value in newly created vector and prints group labels
  ##  User option to only calculate m, d or both statistics
  # Args:
  ##  student_name: object of the class "student"
  ##  my_matrix: by default 4x4 identity matrix
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

# Examples:

sorter(Harry)

sorter(Harry, my_matrix = my_matrix)

