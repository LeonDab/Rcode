#Input your starting state and number of trials
initial_state <- "Jnk"
num_trials <- 10000

transition_matrix <- matrix(c(99.32, 0.60, 0.08, 17.34, 58.42, 24.24, 0, 0, 100),
                            nrow = 3, byrow = TRUE,
                            dimnames = list(c("Inv", "Jnk", "Def"),
                                            c("Inv", "Jnk", "Def")))

single_transition <- function(transition_matrix, current_state) {
  row_index <- match(current_state, rownames(transition_matrix))
  next_state_probs <- transition_matrix[row_index, ]
  next_state <- sample(rownames(transition_matrix), size = 1, prob = next_state_probs)
  return(next_state)}

multiple_transitions <- function(transition_matrix, current_state, num_trials) 
  {time_to_default <- numeric(num_trials)
  
  for (i in 1:num_trials) 
    {state <- current_state
    for (time_step in 1:1000) 
      {state <- single_transition(transition_matrix, state)
      if (state == "Def") {
        time_to_default[i] <- time_step
        break}}
      if (time_to_default[i] == 0) 
      {time_to_default[i] <- 1000}}
  return(time_to_default)}

time_to_default <- multiple_transitions(transition_matrix, initial_state, num_trials)

average_time_to_default <- mean(time_to_default)

cat("Average Time to Default in Years:", average_time_to_default, "\n")
