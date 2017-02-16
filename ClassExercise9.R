# R Programming
# Exercise 5: The Sorting Hat
# Feb 14, 2017
# -------------------------------


# 1) students --------------------

funcStudent <- function(name) {
  student <- list(name, courage = sample(x = 1:100, size = 1), ambition = sample(1:100, 1),
                  intelligence = sample(1:100, 1), effort = sample(1:100, 1))
  class(student) <- "student"
  print(student)
}

Harry <- funcStudent("Harry Potter")

# 2) sorter ----------------------

my_matrix <- matrix(sample(1:100, 16), nrow = 4, ncol = 4)

# different ways to unlist
a <- unlist(matrix(Harry[2:5], ncol = 1, dimnames = list(names(Harry[2:5]))))
b <- unlist(Harry)[2:5]

# function
sorter <- function(student_name, my_matrix = diag(4)) {
  a <- unlist(matrix(student_name[2:5], ncol = 1, dimnames = list(names(student_name[2:5]))))
  vect1 <- a %*% t(my_matrix)
  print(sort(vect1, decreasing = T))
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

sorter(Harry)

# my questions:
## - what is the role of "my_matrix"? 
### - where does the matrix come from? I set it to the identity matrix by default...
### - why do we need to multiply it with "a"?
## - we didn't use the "sort" function yet although we were supposed to do that...
## - 
