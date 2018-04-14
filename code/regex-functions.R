#1
split_chars <- function(x){
  element <- str_sub(x, start = c(1:str_count(x)), end = c(1:str_count(x)))
  return(element)
}
split_chars("Go Bears!")
split_chars('Expecto Patronum')

#2
num_vowels <- function(x){
  a <- sum(x == "A" | x == "a")
  e <- sum(x == "E" | x == "e")
  i <- sum(x == "I" | x == "i")
  o <- sum(x == "O" | x == "o")
  u <- sum(x == "U" | x == "u")
  vowel <- c(a,e,i,o,u)
  name <- c("a", "e", "i", "o", "u")
  names(vowel) <- name
  #list <- list(name, vowel)
  return(vowel)
}
num_vowels(vec)
vec <- c("G", "g", "o", "O", 'i', 'a')



#3

count_vowels <- function(x){
  a <- str_replace(x, pattern = " ", replacement = "")
  b <- split_chars(a)
  c <- num_vowels(b)
  return(c)
}

count_vowels("HELLO WORLD, MY NAME IS DANIEL")

#4
reverse_chars <- function(x){
  r <- str_sub(x, start = -c(1:str_count(x)), end = -c(1:str_count(x)))
  reverse <- paste(r, collapse = "")
  return(reverse)
}

reverse_chars("hello world")

#5
reverse_words <- function(x){
  destring <- unlist(str_split(x, pattern = " "))
  reverse <- rev(destring)
  restring <- paste(reverse, collapse = " ")
  return(restring)
}

reverse_words("Hello World")

h <- c("lalala", "hehehe")
k <- paste(h, collapse = " ")


i <- unlist(str_split(k, " "))

rev(i)
paste(rev(i), collapse = " ")






