library(testthat)
context("test-hands.R")

# One hand of each type, and its type
test_hands = list(
  c('["AS", "10S", "QS", "JS", "KS"]', 'Royal Straight Flush'),
  c('["9S", "10S", "QS", "JS", "KS"]', 'Straight Flush'),
  c('["3H", "3S", "3D", "4H", "3C"]', 'Four of a Kind'),
  c('["JS", "3D", "JH", "3C", "JC"]', 'Full House'), # Three of a kind high
  c('["3S", "3D", "JH", "3C", "JC"]', 'Full House'), # Pair high
  c('["3H", "10H", "2H", "AH", "7H"]', 'Flush'),
  c('["8S", "QH", "JS", "9D", "10C"]', 'Straight'),
  c('["3H", "3S", "5D", "4H", "3C"]', 'Three of a Kind'),
  c('["3H", "3S", "KD", "4H", "KC"]', 'Two Pair'),
  c('["AS", "KD", "AD", "3C", "9H"]', 'One Pair'),
  c('["1H", "2H", "3C", "4H", "6H"]', 'High Card')) # Heart breaker...

test_that("rank_hand works", {
  for (case in test_hands) {
    hand = parse_cards(case[1])
    expected_rank = case[2]
    expect_equal(rank_hand(hand), expected_rank, info=expected_rank)
  }
})