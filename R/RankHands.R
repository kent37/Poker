# Rank hands

#' Ordering permutation of a list of hands.
#' This is `order` for a list of hands.
#' @param hands A list of hand objects
#' @param decreasing Sort in decreasing order (default) or not?
#' @return An integer vector of indices such that `hands[order_hands(hands)]`
#'   is in order by rank.
#' @export
order_hands = function(hands, decreasing=TRUE) {
  # This works by ordering the rank_vectors
  # Make sure each hand is categorized
  hands = purrr::map(hands, categorize_hand)

  # Get the rank vectors
  ranks = purrr::map(hands, 'rank_vector')

  # This transposes the list of ranks to lists of first-ranks, second-ranks, etc
  # padding as needed with NA
  # https://stackoverflow.com/questions/15201305/how-to-convert-a-list-consisting-of-vector-of-different-lengths-to-a-usable-data
  n_ranks <- purrr::map_int(ranks, length)
  seq_max <- seq_len(max(n_ranks))
  padded = purrr::map(ranks, `[`, i=seq_max)
  mat = do.call(rbind, padded) %>% as.data.frame()

  # Now ordering `mat` gives us what we want
  do.call(order, c(mat, list(decreasing=decreasing)))
}
