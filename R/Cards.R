# Parse and display cards and lists of cards
library(magrittr)

# Forward and reverse lookup for card rank
card_names = c(as.character(2:10), 'J', 'Q', 'K', 'A')
card_ranks = 2:14 %>% set_names(card_names)

full_deck = paste0(rep(card_names, 4), rep(c('S', 'H', 'D', 'C'), each=13))

#' Parse a single card
#' @param card The text name of a card, e.g. 'JH'
#' @return A two element list containing the numeric card rank
#' and suit
parse_card = function(card) {
  stopifnot(is.character(card), length(card)==1,
            stringr::str_length(card) %in% 2:3)

  # Split to list
  parsed = stringr::str_match(card, '(..?)(.)')[1, 2:3] %>%
    set_names(c('rank', 'suit')) %>% as.list()

  # Look up the alpha rank. This makes a named vector so it preserves the
  # alpha value.
  parsed$rank = card_ranks[parsed$rank]

  structure(parsed, class=c('card', class(parsed)))
}

#' Parse a hand provided as a JSON list of cards
#' @param cards The JSON input
#' @return A list of cards in decreasing order of rank
parse_cards = function(cards) {
  # Parse to cards
  raw_list = jsonlite::fromJSON(cards)
  hand = purrr::map(raw_list, parse_card)

  # Order by rank
  ranks = purrr::map_int(hand, 'rank')
  hand = hand[order(ranks, decreasing=TRUE)]

  structure(hand, class=c('hand', class(parsed)))
}

#' Construct the string representation of a card
#' @param card A card object
#' @return The string representation of the card
format.card = function(card) {
  stopifnot(inherits(card, 'card'))

  paste0(names(card$rank), card$suit)
}

print.card = function(card) print(format(card))

#' Re-construct the JSON representation of a hand
#' @param hand A hand object
#' @return A JSON representation of the hand
format.hand = function(hand) {
  stopifnot(inherits(hand, 'hand'))
  purrr::map_chr(hand, format.card) %>% paste(collapse=', ')
}

print.hand = function(hand) print(format(hand))
