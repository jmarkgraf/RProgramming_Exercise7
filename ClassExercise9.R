# R Programming
# Exercise 5: The Sorting Hat
# Feb 14, 2017
# -------------------------------


# students
funcStudent <- function(name) {
  student <- list(name, courage = sample(x = 1:100, size = 1), ambition = sample(1:100, 1),
                  intelligence = sample(1:100, 1), effort = sample(1:100, 1))
  class(student) <- "student"
  print(student)
}

Jonas <- unlist(funcStudent("Jonas"))

matrix(Jonas[2:5], nrow = 1, colnames = names(Jonas[2:5]))
# sorter

sorter <- function(student_name, my_matrix) {
  a <- unlist(student_name)[2:5]
  Xt <- t(my_matrix)
  vect <- Xt * t
}

sorter(Jonas)
