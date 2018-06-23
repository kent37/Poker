library(testthat)
library(magrittr)
context("test-hands.R")

# One hand of each type, and its type
test_hands = list(
  c('["AS", "10S", "QS", "JS", "KS"]', 'Royal Straight Flush'),
  c('["9S", "10S", "QS", "JS", "KS"]', 'Straight Flush, K High'),
  c('["9S", "10S", "8S", "6S", "7S"]', 'Straight Flush, 10 High'),
  c('["5H", "5S", "5D", "4H", "5C"]', 'Four of a Kind, 5 High, Kickers: 4'),
  c('["3H", "3S", "3D", "4H", "3C"]', 'Four of a Kind, 3 High, Kickers: 4'),
  c('["3H", "3S", "3D", "2H", "3C"]', 'Four of a Kind, 3 High, Kickers: 2'),
  c('["JS", "9D", "JH", "9C", "JC"]', 'Full House, J over 9'),
  c('["JS", "3D", "JH", "3C", "JC"]', 'Full House, J over 3'),
  c('["3S", "3D", "JH", "3C", "JC"]', 'Full House, 3 over J'),
  c('["3H", "10H", "2H", "AH", "7H"]', 'Flush, A High'),
  c('["3H", "10H", "2H", "8H", "7H"]', 'Flush, 10 High'),
  c('["8S", "QH", "JS", "9D", "10C"]', 'Straight, Q High'),
  c('["8S", "7H", "JS", "9D", "10C"]', 'Straight, J High'),
  c('["QH", "QS", "5D", "4H", "QC"]', 'Three of a Kind, Q High, Kickers: 5, 4'),
  c('["3H", "3S", "5D", "4H", "3C"]', 'Three of a Kind, 3 High, Kickers: 5, 4'),
  c('["3H", "3S", "5D", "2H", "3C"]', 'Three of a Kind, 3 High, Kickers: 5, 2'),
  c('["3H", "3S", "AD", "4H", "AC"]', 'Two Pair, A and 3, Kickers: 4'),
  c('["JH", "JS", "KD", "4H", "KC"]', 'Two Pair, K and J, Kickers: 4'),
  c('["3H", "3S", "KD", "4H", "KC"]', 'Two Pair, K and 3, Kickers: 4'),
  c('["3H", "3S", "KD", "2H", "KC"]', 'Two Pair, K and 3, Kickers: 2'),
  c('["AS", "KD", "AD", "5C", "9H"]', 'One Pair, A High, Kickers: K, 9, 5'),
  c('["AS", "KD", "AD", "3C", "9H"]', 'One Pair, A High, Kickers: K, 9, 3'),
  c('["8S", "KD", "8D", "3C", "9H"]', 'One Pair, 8 High, Kickers: K, 9, 3'),
  c('["9H", "8H", "3C", "4H", "5H"]', 'High Card, 9 High, Kickers: 8, 5, 4, 3'),
  c('["9H", "2H", "5C", "4H", "8H"]', 'High Card, 9 High, Kickers: 8, 5, 4, 2'),
  c('["7H", "2H", "3C", "4H", "6H"]', 'High Card, 7 High, Kickers: 6, 4, 3, 2')
  )

test_that("categorize_hand works", {
  for (case in test_hands) {
    hand = parse_cards(case[1]) %>% categorize_hand
    expected_rank = case[2]
    expect_equal(format(hand$category), expected_rank, info=expected_rank)
  }
})
