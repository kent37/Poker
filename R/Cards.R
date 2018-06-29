# Parse and display cards and lists of cards
library(magrittr)

# Forward and reverse lookup for card rank
card_names = c(as.character(2:10), 'J', 'Q', 'K', 'A')
card_ranks = 2:14 %>% magrittr::set_names(card_names)

ranks_to_name = function(ranks) {
  # Card ranks start at 2, hence the offset
  card_names[ranks-1]
}

suits = c('S', 'H', 'D', 'C')
full_deck = paste0(rep(card_names, 4), rep(suits, each=13))

joker = 'Jo'

#' Parse a single card
#' @param card The text name of a card, e.g. 'JH'
#' @return A two element list containing the numeric card rank
#' and suit
#' @importFrom magrittr "%>%"
parse_card = function(card) {
  stopifnot(is.character(card), length(card)==1,
            stringr::str_length(card) %in% 2:3)

  # Split to list
  parsed = stringr::str_match(card, '(..?)(.)')[1, 2:3] %>%
    magrittr::set_names(c('rank', 'suit')) %>% as.list()

  # Look up the integer rank.
  stopifnot(parsed$rank %in% card_names, parsed$suit %in% suits)
  parsed$rank = card_ranks[parsed$rank]

  structure(parsed, class=c('card', class(parsed)))
}

#' Parse a hand provided as a JSON list of cards
#' @param cards The JSON input
#' @return A list of cards in decreasing order of rank
#' @export
parse_cards = function(cards) {
  # Parse to cards
  raw_list = jsonlite::fromJSON(cards)

  jokers = raw_list == joker
  joker_count = sum(jokers)
  raw_list = raw_list[!jokers]

  hand = purrr::map(raw_list, parse_card)

  # Order by rank
  ranks = purrr::map_int(hand, 'rank')
  hand = hand[order(ranks, decreasing=TRUE)]

  structure(list(cards=hand, joker_count=joker_count), class=c('hand', class(list)))
}

#' Add wild cards to a hand, replacing jokers
#' @param hand A hand
#' @param cards Cards to add
#' @return A new hand with the cards added
#' @export
add_wild_cards = function(hand, cards) {
  new_cards = c(hand$cards, cards)
  ranks = purrr::map_int(new_cards, 'rank')
  hand$cards = new_cards[order(ranks, decreasing=TRUE)]
  hand$joker_count = hand$joker_count - length(cards)
  hand
}

#' Construct the string representation of a card
#' @param x A card object
#' @param ... Ignored
#' @return The string representation of the card
#' @export
format.card = function(x, ...) {
  stopifnot(inherits(x, 'card'))

  paste0(names(x$rank), x$suit)
}

#' Print a card
#' @param x A card object
#' @param ... Passed to `print.default`
#' @export
print.card = function(x, ...) print(format(x), ...)

#' Construct a string representation of a hand
#' @param x A hand object
#' @param ... Ignored
#' @return A JSON representation of the hand
#' @export
#' @importFrom magrittr "%>%"
format.hand = function(x, ...) {
  stopifnot(inherits(x, 'hand'))
  c(purrr::map_chr(x$cards, format.card), rep(joker, x$joker_count)) %>%
    paste(collapse=', ')

}

#' Print a hand
#' @param x A hand object
#' @param ... Passed to `print.default`
#' @export
print.hand = function(x, ...) print(format(x), ...)
