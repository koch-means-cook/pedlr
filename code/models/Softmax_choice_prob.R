
Softmax_choice_prob <- function(value_1,
                                value_2,
                                choice,
                                temperature){
  
  # Get probability of choice given options
  # Softmax function depends on temperature parameter (sigmoid steepness)
  choice_prob_1 = (
    exp(value_1/temperature) / (exp(value_1/temperature) + exp(value_2/temperature))
  )
  choice_prob_2 = 1-choice_prob_1
  choice_prob = c(choice_prob_1, choice_prob_2)
  
  
  # Give probability of specific choice made
  prob = choice_prob[choice]
  
  # Returns index and probability of choice if options are ordered in a two-entry vector
  ans = list('choice' = choice,
             'choice_prob' = prob)
  return(ans)
}
