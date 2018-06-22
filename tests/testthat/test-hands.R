library(testthat)
context("test-hands.R")

# One hand of each type, and its type
test_hands = list(
  c('["AS", "10S", "QS", "JS", "KS"]', 'Royal Straight Flush'),
  c('["9S", "10S", "QS", "JS", "KS"]', 'Straight Flush, K High'),
  c('["3H", "3S", "3D", "4H", "3C"]', 'Four of a Kind, 3 High, Kickers: 4'),
  c('["JS", "3D", "JH", "3C", "JC"]', 'Full House, J over 3'), # Three of a kind high
  c('["3S", "3D", "JH", "3C", "JC"]', 'Full House, 3 over J'), # Pair high
  c('["3H", "10H", "2H", "AH", "7H"]', 'Flush, A High'),
  c('["8S", "QH", "JS", "9D", "10C"]', 'Straight, Q High'),
  c('["3H", "3S", "5D", "4H", "3C"]', 'Three of a Kind, 3 High, Kickers: 5, 4'),
  c('["3H", "3S", "KD", "4H", "KC"]', 'Two Pair, K and 3, Kickers: 4'),
  c('["AS", "KD", "AD", "3C", "9H"]', 'One Pair, A High, Kickers: K, 9, 3'),
  c('["7H", "2H", "3C", "4H", "6H"]', 'High Card, 7 High, Kickers: 6, 4, 3, 2')) # Heart breaker...

test_that("rank_hand works", {
  for (case in test_hands) {
    hand = parse_cards(case[1])
    expected_rank = case[2]
    expect_equal(format(rank_hand(hand)), expected_rank, info=expected_rank)
  }
})

# Make every possible hand and see if it can be ranked
# test_that('All hands can be ranked', {
#
# })
