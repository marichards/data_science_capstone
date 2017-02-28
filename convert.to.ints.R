convert.to.ints <- function(ngram,singlet.hash){
  
  # Do an operation I could do once
  nint <- integer(length = length(ngram))
  for(i in 1:length(ngram)){
    nint[[i]] <- singlet.hash[[ngram[[i]]]]
  }
  
  nint
}