
Softmax_choice <- function(value_1, value_2, temperature){
  
  # Probabilistic model choice with choice probabilities controlled by soft-max function
  # Softmax function depends on temperature parameter (sigmoid steepness)
  choice_prob_1 = (
    exp(value_1/temperature) / (exp(value_1/temperature) + exp(value_2/temperature))
  )
  choice_prob_2 = 1-choice_prob_1
  
  
  # Make choice based on softmax probability
  choice = sample(c(1,2), 1, replace=TRUE, prob=c(choice_prob_1, choice_prob_2))
  # Return probability of choice
  prob = c(choice_prob_1, choice_prob_2)
  choice_prob = prob[choice]
  
  # Returns index and probability of choice if options are ordered in a two-entry vector
  ans = list('choice' = choice,
             'choice_prob' = choice_prob)
  return(ans)
}
