##' @title Get static branching values
##'
##' @return
##' @author Shir Dekel
##' @export
get_values <- function() {

  tibble(
    experiment_number = seq_len(2),
    iv = list(
    ## Experiment 1
    c(
      "anecdote",
      "alignment"
    ),
    ## Experiment 2
    c(
      "anecdote_between",
      "similarity",
      "valence"
    )
    ),
    data = list(
      quote(anecdotes1::data),
      quote(anecdotes2::data)
    )
  ) %>%
    rowwise() %>%
    mutate_function_call(
      "plot",
      experiment_number
    ) %>%
    mutate_function_call(
      "results",
      experiment_number
    )
}
