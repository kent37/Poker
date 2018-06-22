# Find the rank of a hand
library(magrittr)

category_names = c('Royal Flush', 'Straight Flush', 'Four of a Kind',
               'Full House', 'Flush', 'Straight', 'Three of a Kind',
               'Two Pair', 'One Pair', 'High Card')

category_ranks = seq_along(category_names) %>% magrittr::set_names(category_names)

#' Find the category of a hand.
#' Will work with hands of any size up to 5
#' @param hand A hand
#' @return A category object
#' @export
#' @importFrom stats "na.omit"
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

  # Now try all the testers and find the one that sticks
  testers = c(is_straight_flush, is_four_of_a_kind, is_full_house,
              is_flush, is_straight, is_three_of_a_kind, is_two_pair,
              is_pair, is_high_card)
  for (tester in testers) {
    category = tester(hand, diffs, max_flush)
    if (!is.null(category))
      return (category)
  }
}

# These functions all take a hand, diffs and max_flush as input and return either a
# category object (a list with a name, high card(s) and kickers)
# or NULL
is_straight_flush = function(hand, diffs, max_flush) {
  if (max_flush!=5 || !stringr::str_detect(diffs, '1111'))
    return (NULL)

  # It's some kind of straight flush
  if (hand[[1]]$rank == 14)
    return (as.category('Royal Straight Flush', NULL, NULL))

  return (as.category('Straight Flush', hand[1], NULL))
}

is_four_of_a_kind = function(hand, diffs, max_flush) {
  m = stringr::str_locate(diffs, '000')
  loc = unname(m[1, 1])
  if (is.na(loc)) return (NULL)
  high = hand[loc]
  kicker = ifelse(loc==1, hand[5], hand[1])
  return (as.category('Four of a Kind', high, kicker))
}

is_full_house = function(hand, diffs, max_flush) {
  if (stringr::str_detect(diffs, '00.0')) # Three then two
    return (as.category('Full House', hand[c(1, 4)], NULL))

  if (stringr::str_detect(diffs, '0.00')) # Two then three
    return (as.category('Full House', hand[c(3, 1)], NULL))

  return (NULL)
}

is_straight = function(hand, diffs, max_flush) {
  if (stringr::str_detect(diffs, '1111'))
    return (as.category('Straight', hand[1], NULL))

  return (NULL)
}

is_flush = function(hand, diffs, max_flush) {
  if (max_flush == 5)
    return (as.category('Flush', hand[1], NULL))

  return (NULL)
}


is_three_of_a_kind = function(hand, diffs, max_flush) {
  m = stringr::str_locate(diffs, '00')
  loc = m[1, 1]
  if (is.na(loc)) return (NULL)

  kickers = setdiff(1:5, loc:(loc+2))
  return (as.category('Three of a Kind', hand[loc], hand[kickers]))
}

is_two_pair = function(hand, diffs, max_flush) {
  m = stringr::str_locate(diffs, '0.+0')
  p1 = m[1, 1]
  if (is.na(p1)) return (NULL)
  p2 = m[1, 2]
  kicker = setdiff(1:5, c(p1, p1+1, p2, p2+1))
  return (as.category('Two Pair', hand[c(p1, p2)], hand[kicker]))
}

is_pair = function(hand, diffs, max_flush) {
  m = stringr::str_locate(diffs, '0')
  loc = m[1, 1]
  if (is.na(loc)) return (NULL)

  kickers = setdiff(1:5, c(loc, loc+1))
  return (as.category('One Pair', hand[loc], hand[kickers]))
}

is_high_card = function(hand, diffs, max_flush) {
  return (as.category('High Card', hand[1], hand[2:5]))
}

as.category = function(name, high, kickers) {
  structure(list(name=name, high=high, kickers=kickers),
            class=c('category', 'list'))
}

#' Construct the string representation of a category
#' @param x A category object
#' @param ... Ignored
#' @return The string representation of the category
#' @export
format.category = function(x, ...) {
  name = x$name
  high = purrr::map_int(x$high, 'rank')
  high = ranks_to_name(high)
  kickers = purrr::map_int(x$kickers, 'rank')
  kickers = ranks_to_name(kickers)

  high_str = switch(length(high) + 1,
    NA_character_, # length == 0
    paste(high, 'High'),
    ifelse(name=='Full House', paste(high[1], 'over', high[2]),
      paste(high[1], 'and', high[2]))
  )

  kicker_str = ifelse(length(kickers)==0, NA_character_,
                      paste('Kickers:', paste(kickers, collapse=', ')))

  paste(na.omit(c(name, high_str, kicker_str)), collapse=', ')
}

#' Print a category
#' @param x A category object
#' @param ... Passed to `print.default`
#' @export
print.category = function(x, ...) print(format(x), ...)
