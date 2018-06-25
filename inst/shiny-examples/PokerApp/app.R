# Simple application to demonstrate evaluation and ranking of poker hands
library(shiny)
library(Poker)

ui <- fluidPage(
   titlePanel("Poker Examples"),
   p('Enter a poker hand as a JSON array. Cards are represented by their number or first letter for the non-numeric cards (J, Q, K, A). Suits are represented by their first letter (H, C, D, S), for example ["JH", "4C", "4S", "JC", "9H"]'),
   p('Click "Add" to add the entered hand. Click "Add test cases" to add many test cases. Click "Clear all" to clear the list of hands.'),
   fluidRow(
     column(6, textInput('hand', 'Enter a hand')),
     actionButton('add', 'Add'),
     actionButton('tests', 'Add test cases'),
     actionButton('clear', 'Clear all')
   ),
   p(textOutput('error_msg')),
   p('This table shows the entered hands in rank order from high to low.'),
   fluidRow(tableOutput('hands'))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Data store
  values = reactiveValues(hands = list(), results = data.frame())

  add_hand = function(hand_text) {
    hand = purrr::safely(parse_cards)(hand_text)
    if (is.null(hand$result)) {
      output$error_msg = renderText(hand$error$message)
    } else {
      hand = hand$result
      hand = purrr::safely(categorize_hand)(hand)
      if (is.null(hand$result)) {
        output$error_msg = renderText(hand$error$message)
      } else {
        hand = hand$result
        hands = c(values$hands, list(hand))
        desc = format(hand$category)
        cards = format_cards(hand$category)
        results = rbind(values$results,
                        data.frame(Hand=hand_text, Value=desc, Cards=cards, Rank=0))
        order = order_hands(hands)
        values$hands = hands[order]
        ranks = rank_hands(values$hands)
        results = results[order,]
        results$Rank = ranks
        values$results = results
      }
    }
  }

  observeEvent(input$add, {
    hand_text = input$hand
    # We are getting smart quotes somehow
    hand_text = gsub('[“”]', '"', hand_text)
    add_hand(hand_text)
  })

  observeEvent(input$tests, {
    for (case in test_hands)
      add_hand(case[1])
  })

  observeEvent(input$clear, {
    values$results = data.frame()
    values$hands = list()
  })

  observe({
    output$hands = renderTable(values$results)
  })
}

test_hands = list(
  c('["AS", "10S", "QS", "JS", "KS"]'),
  c('["9S", "10S", "QS", "JS", "KS"]'),
  c('["9S", "10S", "8S", "6S", "7S"]'),
  c('["5H", "5S", "5D", "4H", "5C"]'),
  c('["3H", "3S", "3D", "4H", "3C"]'),
  c('["3H", "3S", "3D", "2H", "3C"]'),
  c('["JS", "9D", "JH", "9C", "JC"]'),
  c('["JS", "3D", "JH", "3C", "JC"]'),
  c('["3S", "3D", "JH", "3C", "JC"]'),
  c('["3H", "10H", "2H", "AH", "7H"]'),
  c('["3H", "10H", "2H", "8H", "7H"]'),
  c('["8S", "QH", "JS", "9D", "10C"]'),
  c('["8S", "7H", "JS", "9D", "10C"]'),
  c('["QH", "QS", "5D", "4H", "QC"]'),
  c('["3H", "3S", "5D", "4H", "3C"]'),
  c('["3H", "3S", "5D", "2H", "3C"]'),
  c('["3H", "3S", "AD", "4H", "AC"]'),
  c('["JH", "JS", "KD", "4H", "KC"]'),
  c('["3H", "3S", "KD", "4H", "KC"]'),
  c('["3H", "3S", "KD", "2H", "KC"]'),
  c('["AS", "KD", "AD", "5C", "9H"]'),
  c('["AS", "KD", "AD", "3C", "9H"]'),
  c('["8S", "KD", "8D", "3C", "9H"]'),
  c('["9H", "8H", "3C", "4H", "5H"]'),
  c('["9H", "2H", "5C", "4H", "8H"]'),
  c('["7H", "2H", "3C", "4H", "6H"]')
  )

# Run the application
shinyApp(ui = ui, server = server)

