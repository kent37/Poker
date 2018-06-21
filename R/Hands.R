# Find the rank of a hand
library(magrittr)

category_names = c('Royal Flush', 'Straight Flush', 'Four of a Kind',
               'Full House', 'Flush', 'Straight', 'Three of a Kind',
               'Two Pair', 'One Pair', 'High Card')

category_ranks = seq_along(hand_names) %>% set_names(hand_names)

#' Find the category of a hand.
#' Will work with hands of any size up to 5
#' @param hand A hand
#' @return A category object
rank_hand = function(hand) {
  stopifnot(inherits(hand, 'hand'), length(hand) <= 5)

  # We can learn a lot from the differences between ranks. This
  # can identify like cards and runs.
  # Putting the diffs into strings allows regex matching
  ranks = purrr::map_int(hand, 'rank')
  diffs = diff(ranks)

  # Make a useful string containing only 0, 1 and 2
  diffs = -diffs
  diffs[diffs>1] = 2
  diffs = paste0(diffs, collapse='')

  # Find the largest number of cards in any suit
  suits = purrr::map_chr(hand, 'suit')
  max_flush = max(table(suits))

  # Now we can make a first cut at finding the category
  category = dplyr::case_when(
    max_flush==5 && stringr::str_detect(diffs, '1111') ~ 'Straight Flush',
    stringr::str_detect(diffs, '000') ~ 'Four of a Kind',
    stringr::str_detect(diffs, '00.0')
      || stringr::str_detect(diffs, '0.00') ~ 'Full House',
    max_flush==5 ~ 'Flush',
    stringr::str_detect(diffs, '1111') ~ 'Straight',
    stringr::str_detect(diffs, '00') ~ 'Three of a Kind',
    stringr::str_detect(diffs, '0.+0') ~ 'Two Pair',
    stringr::str_detect(diffs, '0') ~ 'One Pair',
    TRUE ~ 'High Card'
  )

  # Look for a royal straight flush
  if (category == 'Straight Flush' && ranks[1] == 14)
    category = 'Royal Straight Flush'

  category
}
