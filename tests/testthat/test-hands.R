library(testthat)
library(magrittr)
context("test-hands.R")

# One hand of each type, its type, number of cards that make the hand
test_hands = list(
  c('["AS", "10S", "QS", "JS", "KS"]', 'Royal Straight Flush', 5),
  c('["9S", "10S", "QS", "JS", "KS"]', 'Straight Flush, K High', 5),
  c('["9S", "10S", "8S", "6S", "7S"]', 'Straight Flush, 10 High', 5),
  c('["5H", "5S", "5D", "4H", "5C"]', 'Four of a Kind, 5 High, Kickers: 4', 4),
  c('["3H", "3S", "3D", "4H", "3C"]', 'Four of a Kind, 3 High, Kickers: 4', 4),
  c('["3H", "3S", "3D", "2H", "3C"]', 'Four of a Kind, 3 High, Kickers: 2', 4),
  c('["JS", "9D", "JH", "9C", "JC"]', 'Full House, J over 9', 5),
  c('["JS", "3D", "JH", "3C", "JC"]', 'Full House, J over 3', 5),
  c('["3S", "3D", "JH", "3C", "JC"]', 'Full House, 3 over J', 5),
  c('["3H", "10H", "2H", "AH", "7H"]', 'Flush, A High', 5),
  c('["3H", "10H", "2H", "8H", "7H"]', 'Flush, 10 High', 5),
  c('["8S", "QH", "JS", "9D", "10C"]', 'Straight, Q High', 5),
  c('["8S", "7H", "JS", "9D", "10C"]', 'Straight, J High', 5),
  c('["QH", "QS", "5D", "4H", "QC"]', 'Three of a Kind, Q High, Kickers: 5, 4', 3),
  c('["3H", "3S", "5D", "4H", "3C"]', 'Three of a Kind, 3 High, Kickers: 5, 4', 3),
  c('["3H", "3S", "5D", "2H", "3C"]', 'Three of a Kind, 3 High, Kickers: 5, 2', 3),
  c('["3H", "3S", "AD", "4H", "AC"]', 'Two Pair, A and 3, Kickers: 4', 4),
  c('["JH", "JS", "KD", "4H", "KC"]', 'Two Pair, K and J, Kickers: 4', 4),
  c('["3H", "3S", "KD", "4H", "KC"]', 'Two Pair, K and 3, Kickers: 4', 4),
  c('["3H", "3S", "KD", "2H", "KC"]', 'Two Pair, K and 3, Kickers: 2', 4),
  c('["AS", "KD", "AD", "5C", "9H"]', 'One Pair, A High, Kickers: K, 9, 5', 2),
  c('["AS", "KD", "AD", "3C", "9H"]', 'One Pair, A High, Kickers: K, 9, 3', 2),
  c('["8S", "KD", "8D", "3C", "9H"]', 'One Pair, 8 High, Kickers: K, 9, 3', 2),
  c('["9H", "8H", "3C", "4H", "5H"]', 'High Card, 9 High, Kickers: 8, 5, 4, 3', 1),
  c('["9H", "2H", "5C", "4H", "8H"]', 'High Card, 9 High, Kickers: 8, 5, 4, 2', 1),
  c('["7H", "2H", "3C", "4H", "6H"]', 'High Card, 7 High, Kickers: 6, 4, 3, 2', 1)
  )

# These hands have more than five cards
test_big_hands = list(
  c('["AS", "5S", "2D", "10S", "QS", "JS", "KS"]',
    'Royal Straight Flush', "AS, KS, QS, JS, 10S"),
  c('["9S", "10S", "5S", "2D", "QS", "JS", "KS"]',
    'Straight Flush, K High', "KS, QS, JS, 10S, 9S"),
  c('["9S", "10S", "8S", "5S", "2D", "6S", "7S"]',
    'Straight Flush, 10 High', "10S, 9S, 8S, 7S, 6S"),
  c('["5H", "QD", "5S", "5D", "4H", "5C"]',
    'Four of a Kind, 5 High, Kickers: Q', "5H, 5S, 5D, 5C + QD"),
  c('["2C", "3H", "3S", "3D", "4H", "3C"]',
    'Four of a Kind, 3 High, Kickers: 4', "3H, 3S, 3D, 3C + 4H"),
  c('["JS", "10D", "QS", "9D", "JH", "9C", "JC"]',
    'Full House, J over 9', "JS, JH, JC, 9D, 9C"),
  c('["3S", "3D", "10C", "JH", "3C", "JC"]',
    'Full House, 3 over J', "JH, JC, 3S, 3D, 3C"),
  c('["3H", "3C", "4D", "10H", "2H", "AH", "7H"]',
    'Flush, A High', "AH, 10H, 7H, 3H, 2H"),
  c('["KD", "8D", "8S", "7H", "7D", "JS", "9D", "9C", "10C"]',
    'Straight, J High', "JS, 10C, 9D, 8D, 7H"),
  c('["10C", "QH", "QS", "5D", "4H", "QC"]',
    'Three of a Kind, Q High, Kickers: 10, 5', "QH, QS, QC + 10C, 5D"),
  c('["3H", "3S", "5H", "6S", "AD", "4H", "AC"]',
    'Two Pair, A and 3, Kickers: 6', "AD, AC, 3H, 3S + 6S"),
  c('["AS", "4C", "2D", "KD", "AD", "5C", "9H"]',
    'One Pair, A High, Kickers: K, 9, 5', "AS, AD + KD, 9H, 5C"),
  c('["9H", "JD", "8H", "3C", "4H", "5H"]',
    'High Card, J High, Kickers: 9, 8, 5, 4', "JD + 9H, 8H, 5H, 4H")
  )

test_that("categorize_hand works", {
  for (case in test_hands) {
    expected_rank = case[2]
    #cat('** ', expected_rank, '\n')
    hand = parse_cards(case[1]) %>% categorize_hand
    expect_equal(format(hand$category), expected_rank, info=expected_rank)
    expect_equal(length(hand$category$cards), as.integer(case[3]),
                 info=expected_rank)
  }
})

test_that("categorize_hand works with big hands", {
  for (case in test_big_hands) {
    expected_rank = case[2]
    #cat('** ', expected_rank, '\n')
    hand = parse_cards(case[1]) %>% categorize_hand
    expect_equal(format(hand$category), expected_rank, info=expected_rank)
    expect_equal(format_cards(hand$category), case[3],
                 info=expected_rank)
  }
})

for (case in test_big_hands) {
  hand = parse_cards(case[1]) %>% categorize_hand
  cat('"', format_cards(hand$category), '"\n', sep='')
}

test_that('Ordering hands works', {
  hands = purrr::map(test_hands, 1) %>%
    purrr::map(parse_cards) %>%
    purrr::map(categorize_hand)

  # Random permutation of hands
  permuted = hands[sample(seq_along(hands))]
  sorted = permuted[order_hands(permuted)]
  ranks = rank_hands(sorted)
  sorted_categories = purrr::map(sorted, 'category') %>%
    purrr::map_chr(format)
  expected_categories = purrr::map_chr(test_hands, 2)
  expect_equal(sorted_categories, expected_categories)
  expect_equal(ranks, seq_along(hands))

  # Check that equal hands sort together
  doubled = rep(hands, each=2)
  double_order = order_hands(doubled)
  expect_equal(double_order, seq_along(double_order))
  ranks = rank_hands(doubled)
  expect_equal(ranks, rep(seq_along(hands)*2-1, each=2))

  # Check reverse sort
  reverse_order = order_hands(hands, decreasing=FALSE)
  expect_equal(reverse_order, length(hands):1)
})

