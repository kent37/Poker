# Find the category of a hand
library(magrittr)

category_names = c('Royal Straight Flush', 'Straight Flush', 'Four of a Kind',
               'Full House', 'Flush', 'Straight', 'Three of a Kind',
               'Two Pair', 'One Pair', 'High Card')

# Numeric ranks, decreasing with the value of the hand
category_ranks = (length(category_names) - seq_along(category_names)) %>%
  magrittr::set_names(category_names)

#' Find the category of a hand.
#' Will work with hands up to 9 cards.
#' @param hand A hand
#' @return The hand with added category and rank_vector fields
#' @export
#' @importFrom stats "na.omit"
categorize_hand = function(hand) {
  # Limiting the hand to <= 9 cards means there can be only one
  # possible straight or flush. We depend on that.
  stopifnot(inherits(hand, 'hand'), length(hand) <= 9)

  # Category is cached in the hand, if it is there just return it
  if (!is.null(hand$category))
    return (hand)

  # Differences between cards, as a string of [012]+
  diffs = make_diffs(hand$cards)

  # Find the largest number of cards in any suit
  suits = purrr::map_chr(hand$cards, 'suit')
  suit_counts = table(suits)
  max_flush = max(suit_counts) # Biggest flush in this hand

  # If there is a possible (straight) flush, get all the cards
  # in the suit with the max count.
  flush_cards = NULL
  if (max_flush >= 5) {
    flush_suit = names(suit_counts)[which.max(suit_counts)]
    flush_cards = hand$cards[suits == flush_suit]
  }

  # Now try all the testers and find the one that sticks
  testers = c(is_straight_flush, is_four_of_a_kind, is_full_house,
              is_flush, is_straight, is_three_of_a_kind, is_two_pair,
              is_pair, is_high_card)
  for (tester in testers) {
    category = tester(hand$cards, diffs, max_flush, flush_cards)
    if (!is.null(category)) {
      hand$category = category
      hand$rank_vector = make_rank_vector(category)
      return (hand)
    }
  }
}

# We can learn a lot from the differences between ranks. This
# can identify like cards and runs.
# Putting the diffs into a string allows regex matching
make_diffs <- function(cards) {
  ranks = purrr::map_int(cards, 'rank')
  diffs = diff(ranks)

  # Make a useful string containing only 0, 1 and 2
  diffs = -diffs
  diffs[diffs>1] = 2
  diffs = paste0(diffs, collapse='')
}

# These functions all take a hand, diffs, max_flush and flush_cards as input and
# return either a category object (a list with a name, used cards, high card(s)
# and kickers) or NULL
is_straight_flush = function(hand, diffs, max_flush, flush_cards) {
  if (max_flush < 5)
    return (NULL)

  # We need to work just with flush_cards, starting with a new diff
  diffs = make_diffs(flush_cards)
  m = stringr::str_locate(diffs, '1111')
  loc = unname(m[1, 1])
  if (is.na(loc)) return (NULL)

  # It's some kind of straight flush
  cards = flush_cards[loc:(loc+4)]
  if (cards[[1]]$rank == 14)
    return (as.category('Royal Straight Flush', cards, NULL, NULL))

  return (as.category('Straight Flush', cards, cards[1], NULL))
}

is_four_of_a_kind = function(hand, diffs, ...) {
  m = stringr::str_locate(diffs, '000')
  loc = unname(m[1, 1])
  if (is.na(loc)) return (NULL)

  high = hand[loc]
  cards = hand[loc:(loc+3)]
  kicker = hand[setdiff(seq_along(hand),loc:(loc+3))[1]]
  return (as.category('Four of a Kind', cards, high, kicker))
}

is_full_house = function(hand, diffs, ...) {
  m = stringr::str_locate(diffs, '00.+0') # Three then two
  start = unname(m[1, 1])
  if (!is.na(start)) {
    end = unname(m[1, 2])
    cards = hand[c(start:(start+2), end, end+1)]
    return (as.category('Full House', cards, cards[c(1, 4)], NULL))
  }

  m = stringr::str_locate(diffs, '0.+00') # Two then three
  start = unname(m[1, 1])
  if (is.na(start))
    return (NULL)

  end = unname(m[1, 2])
  cards = hand[c(start, start+1, (end-1):(end+1))]
  return (as.category('Full House', cards, cards[c(4, 1)], NULL))
}

is_straight = function(hand, diffs, ...) {
  # We have to allow for duplicates within a run, e.g. 566789
  m = stringr::str_locate(diffs, '10*10*10*1')
  start = unname(m[1, 1])
  if (is.na(start)) return (NULL)

  end = unname(m[1, 2])

  # Take out duplicates by looking for 0s in the relevant portion of diffs
  keep = strsplit(diffs, '')[[1]][start:end] != '0'
  keep = c(TRUE, keep) # Diffs are one shorter than cards so pad
  cards = hand[start:(end+1)][keep]

  return (as.category('Straight', cards, cards[1], NULL))

  return (NULL)
}

is_flush = function(hand, diffs, max_flush, flush_cards) {
  if (max_flush == 5)
    return (as.category('Flush', flush_cards[1:5], flush_cards[1], NULL))

  return (NULL)
}


is_three_of_a_kind = function(hand, diffs, ...) {
  m = stringr::str_locate(diffs, '00')
  loc = m[1, 1]
  if (is.na(loc)) return (NULL)

  cards = hand[loc:(loc+2)]

  # The two best cards not included in the three
  kickers = hand[setdiff(seq_along(hand), loc:(loc+2))[1:2]]
  return (as.category('Three of a Kind', cards, cards[1], kickers))
}

is_two_pair = function(hand, diffs, ...) {
  m = stringr::str_locate(diffs, '0.+0')
  p1 = m[1, 1]
  if (is.na(p1)) return (NULL)
  p2 = m[1, 2]
  cards = c(hand[c(p1, p1+1, p2, p2+1)])
  kicker = hand[setdiff(seq_along(hand), c(p1, p1+1, p2, p2+1))[1]]
  return (as.category('Two Pair', cards, cards[c(1, 3)], kicker))
}

is_pair = function(hand, diffs, ...) {
  m = stringr::str_locate(diffs, '0')
  loc = m[1, 1]
  if (is.na(loc)) return (NULL)

  cards = hand[c(loc, loc+1)]
  kickers = hand[setdiff(seq_along(hand), c(loc, loc+1))[1:3]]
  return (as.category('One Pair', cards, cards[1], kickers))
}

is_high_card = function(hand, diffs, ...) {
  return (as.category('High Card', hand[1], hand[1], hand[2:5]))
}

# A category is a list with
# name - the category name, e.g. "One Pair"
# cards - the cards constituting the actual combination, e.g. the pair
# high - the high card(s) in the hand
# kickers - any cards in the hand not used to make `cards`
as.category = function(name, cards, high, kickers) {
  structure(list(name=name, cards=cards, high=high, kickers=kickers),
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

#' Format the (subset of) cards used to make the best hand.
#' @param category A category object
#' @return A string showing the cards used in the hand
#' @export
format_cards = function(category) {
  stopifnot(inherits(category, 'category'))
  used = purrr::map_chr(category$cards, format.card) %>% paste(collapse=', ')
  if (length(category$kickers) > 0) {
    kickers = purrr::map_chr(category$kickers, format.card) %>% paste(collapse=', ')
    paste(used, '+', kickers)
  } else used
}

#' Print a category
#' @param x A category object
#' @param ... Passed to `print.default`
#' @export
print.category = function(x, ...) print(format(x), ...)

#' Make a vector of (integer) category, high card ranks, kicker ranks
#' that can be used to order hands
#' @param category A category object
#' @return An integer vector
#' @export
make_rank_vector = function(category) {
  cat_rank = category_ranks[category$name]
  cards = purrr::map_int(c(category$high, category$kickers), 'rank')
  structure(c(cat_rank, cards), class=c('rank_vector', 'numeric'))
}
