# Turns agreement vector into an ordered factor
# You can supply your own set of strings and it will use those levels

factorize_agreement <- function(x, lvls){
  x <- tolower(x)
  if(missing(lvls)){ lvls <- c("strongly agree", "agree", "somewhat agree", "somewhat disagree", "disagree", "strongly disagree") }
  x <- factor(x, levels = lvls)
  x
}

# Searches a data.frame for vectors where the six common agreement levels make up >50% of values
# Can supply own set of strings; calls factorize_agreement_vec
# Throw warning with values that don't fit the paradigm
# Returns data.frame with factorization completed in place
#  factorize_agreement_df <- function(x, lvls){

# }
