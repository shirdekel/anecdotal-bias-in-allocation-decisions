#' @title Get anecdotes 2 allocation results
#'
#' Excluding the statistics only condition

#' @return
#' @author Shir Dekel
#' @export
#' @param data
get_allocation <- function(data = anecdotes2::data) {
  allocation_omnibus <-
    data %>%
    get_omnibus_anecdotes_2_allocation()

  allocation_omnibus_apa <-
    allocation_omnibus %>%
    apa_print(es = "pes", mse = FALSE) %>%
    pluck("full_result")

  allocation_combined_high <-
    data |>
    filter(
      valence == "negative",
      anecdote_between == "combined",
      similarity == "high"
    ) |>
    pivot_longer(
      c(allocation, statistics_only),
      names_to = "condition", values_to = "allocation"
    ) |>
    aov_ez(
      id = "id",
      dv = "allocation",
      within = "condition",
      data = _,
      type = 2
    ) |>
    apa_print(es = "pes", mse = FALSE) |>
    pluck("table", "estimate")

  allocation_combined_high_eta2 <-
    str_glue("$\\hat{{\\eta}}^2_p = {allocation_combined_high}$")

  anecdotes_only_similarity <-
    c("Anecdoteonly_Negative_Low_high", "Anecdoteonly_Positive_Low_high") %>%
    map(
      ~ allocation_omnibus %>%
        get_anecdotes_only_similarity() %>%
        apa_print() %>%
        pluck("full_result", .x)
    ) %>%
    set_names("valence_negative", "valence_positive")

  similarity_high_anecdote <-
    data %>%
    filter(similarity == "high") %>%
    aov_ez(
      id = "id",
      dv = "allocation_inverse",
      between = "anecdote_between",
      within = c("valence"),
      data = .,
      type = 2
    ) %>%
    apa_print(es = "pes", mse = FALSE) %>%
    pluck("full_result")

  allocation_omnibus_apa %>%
    c(
      lst(
        allocation_combined_high_eta2,
        anecdotes_only_similarity,
        similarity_high_anecdote
      )
    )
}
