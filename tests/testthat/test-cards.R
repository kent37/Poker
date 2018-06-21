library(testthat)
context("test-cards.R")

# Test that we can parse cards
test_that("parse_card works", {
  for (card in full_deck) {
    parsed = parse_card(card)
    expect(inherits(parsed, 'card'), info=card)
    expect_equal(names(parsed), c('rank', 'suit'), info=card)
    expect_equal(card, format(parsed), info=card)
  }
})
