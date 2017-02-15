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

# different ways to unlist
a <- matrix(Harry[2:5], ncol = 1, dimnames = list(names(Harry[2:5])))
b <- unlist(Harry)[2:5]

# function
sorter <- function(student_name, my_matrix) {   # question: how does "my_matrix" help us? what's its role?
  a <- unlist(student_name)[2:5]  # we need to incorporate step 1 here: calculate t(X)*a
  # vect1 <- a %*% t(my_matrix)  # code for multiplying 'a' with transposed matrix
  if(max(a) == a[1]) {
    print("GRIFFINDOR!")
  } else if(max(a) == a[2]) {
    print("SLYTHERIN!")
  } else if(max(a) == a[3]) {
    print("RAVENCLAW!")
  } else {
    print("HUFFLEPUFF!")
  }
}

sorter(Harry)