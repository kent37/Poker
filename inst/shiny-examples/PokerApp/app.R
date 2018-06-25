# Simple application to demonstrate evaluation and ranking of poker hands
library(shiny)
library(Poker)

ui <- fluidPage(
   titlePanel("Poker Examples"),
   p('Enter poker hands in JSON notation'),
   fluidRow(
     column(8, textInput('hand', 'Enter a hand')),
     actionButton('add', 'Add'),
     actionButton('clear', 'Clear All')
   ),
   p(textOutput('error_msg')),
   p('This table shows the entered hands in rank order from high to low.'),
   fluidRow(tableOutput('hands'))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Data store
  values = reactiveValues(hands = list(), results = data.frame())

  observeEvent(input$add, {
    hand_text = input$hand
    # We are getting smart quotes somehow
    hand_text = gsub('[“”]', '"', hand_text)

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
                        data.frame(Hand=hand_text, Value=desc, Cards=cards))
        order = order_hands(hands)
        values$hands = hands[order]
        values$results = results[order,]
      }
    }
  })

  observeEvent(input$clear, {
    values$results = data.frame()
    values$hands = list()
  })

  observe({
    output$hands = renderTable(values$results)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

