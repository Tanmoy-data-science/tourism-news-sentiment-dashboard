# ============================================================
# Tourism News Text Analysis Dashboard
# EPPS 6356 - Data Visualization (Fall 2025)
# app.R  (place in: shiny_app/app.R)
# ============================================================

library(shiny)
library(bslib)
library(tidyverse)
library(readr)
library(plotly)
library(DT)
library(viridis)
library(wordcloud)

# ---- Safe loader -------------------------------------------------

safe_read_csv <- function(path) {
  if (file.exists(path)) read_csv(path, show_col_types = FALSE) else NULL
}

# NOTE: paths are relative to shiny_app/
topics_results     <- safe_read_csv("data/topics_results.csv")
article_data       <- safe_read_csv("data/article_metadata.csv")
state_mentions     <- safe_read_csv("data/state_mentions.csv")
top_words          <- safe_read_csv("data/top_words.csv")
sentiment_timeline <- safe_read_csv("data/sentiment_timeline.csv")

# Topic labels (for dropdown)
topic_labels <- c(
  "1" = "National parks",
  "2" = "Global tourism & markets",
  "3" = "Cities, beaches & destinations",
  "4" = "Medical tourism & safety",
  "5" = "International tourists & spending",
  "6" = "Travel rules & visas"
)

# ---- Prepare sentiment data --------------------------------------

sentiment_df <- NULL
if (!is.null(sentiment_timeline)) {
  # file has: time_period, n_articles, avg_sentiment, pct_positive
  sentiment_df <- sentiment_timeline |>
    mutate(
      year = as.integer(time_period),
      date = as.Date(paste0(year, "-01-01"))
    )
}

# ---- Colors / theme ----------------------------------------------

pal_florida    <- "#4169E1"
pal_california <- "#FF8C00"
pal_neutral    <- "#6C757D"

app_theme <- bs_theme(
  bootswatch = "flatly",
  base_font  = font_google("Source Sans 3")
)

# Helper: safely count indicator columns (case-insensitive)
indicator_count <- function(df, indicator_name) {
  if (is.null(df)) return(NA_integer_)
  nms <- names(df)
  
  hit <- nms[tolower(nms) == tolower(indicator_name)]
  if (length(hit) == 1) {
    v <- df[[hit]]
    if (is.logical(v) || is.numeric(v)) {
      return(sum(v, na.rm = TRUE))
    }
  }
  NA_integer_
}

# ---- UI ----------------------------------------------------------

ui <- fluidPage(
  theme = app_theme,
  titlePanel("Tourism Recovery: Media Narratives"),
  
  # Analysis period note just under the title
  div(
    style = "margin-bottom: 15px; font-size: 14px; font-weight: 500;",
    "Analysis period: 2019–2025 news coverage on tourism recovery"
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Controls"),
      
      selectInput(
        "topic_select",
        "Choose topic:",
        choices = if (!is.null(topics_results) && "topic" %in% names(topics_results)) {
          topic_ids <- sort(unique(topics_results$topic))
          setNames(topic_ids, topic_labels[as.character(topic_ids)])
        } else NULL
      ),
      
      sliderInput(
        "top_terms_n",
        "Number of top terms per topic:",
        min = 3, max = 10, value = 5
      ),
      
      checkboxInput(
        "use_pct_positive",
        "Sentiment timeline: show % positive (instead of average score)",
        value = FALSE
      ),
      
      hr(),
      helpText(
        "Tip: hover over bars and lines for exact values; ",
        "use the article browser to inspect individual stories."
      )
    ),
    
    mainPanel(
      width = 9,
      
      # ---- Metric cards ----
      fluidRow(
        column(
          2,
          wellPanel(
            style = "min-height: 130px;",
            h5("Corpus Size"),
            h3(textOutput("ta_total_articles"),
              style = "font-weight:700; font-size:26px; color:#4169E1;"),
            p("Total articles", style = "font-size: 11px;")
          )
        ),
        column(
          2,
          wellPanel(
            style = "min-height: 130px;",
            h5("Florida Coverage"),
            h3(textOutput("ta_fl_articles"),
              style = "font-weight:700; font-size:26px; color:#4169E1;"),
            p("Articles mentioning FL", style = "font-size: 11px;")
          )
        ),
        column(
          2,
          wellPanel(
            style = "min-height: 130px;",
            h5("California Coverage"),
            h3(textOutput("ta_ca_articles"),
               style = "font-weight:700; font-size:26px; color:#4169E1;"),
            p("Articles mentioning CA", style = "font-size: 11px;")
          )
        ),
        column(
          3,
          wellPanel(
            style = "min-height: 130px;",
            h5("COVID Coverage"),
            h3(textOutput("ta_covid_articles"),
               style = "font-weight:700; font-size:26px; color:#4169E1;"),
            p("Articles mentioning COVID-19", style = "font-size: 11px;")
          )
        ),
        column(
          3,
          wellPanel(
            style = "min-height: 130px;",
            h5("Most Common Word"),
            h3(textOutput("ta_top_word"),
               style = "font-weight:700; font-size:26px; color:#4169E1;"),
            p("Across entire corpus", style = "font-size: 11px;")
          )
        )
      ),
      
      br(),
      
      # ---- Topic terms & sentiment ----
      fluidRow(
        column(
          6,
          h4("Topic Modeling: Top Terms"),
          plotlyOutput("ta_topics_plot", height = "420px")
        ),
        column(
          6,
          h4("Sentiment Timeline"),
          plotlyOutput("ta_sentiment_plot", height = "420px")
        )
      ),
      
      br(),
      
      # ---- States & word cloud ----
      fluidRow(
        column(
          6,
          h4("Top ten states mentioned in articles"),
          plotlyOutput("ta_states_plot", height = "380px")
        ),
        column(
          6,
          h4("Top Words"),
          plotOutput("ta_wordcloud", height = "380px")
        )
      ),
      
      br(),
      
      # ---- Article table ----
      h4("Article Browser"),
      DTOutput("ta_articles_table")
    )
  )
)

# ---- Server ------------------------------------------------------

server <- function(input, output, session) {
  
  # -------- Metric cards --------
  
  output$ta_total_articles <- renderText({
    if (is.null(article_data)) return("N/A")
    format(nrow(article_data), big.mark = ",")
  })
  
  output$ta_fl_articles <- renderText({
    n <- indicator_count(article_data, "mentions_florida")
    if (is.na(n)) "N/A" else format(n, big.mark = ",")
  })
  
  output$ta_ca_articles <- renderText({
    n <- indicator_count(article_data, "mentions_california")
    if (is.na(n)) "N/A" else format(n, big.mark = ",")
  })
  
  output$ta_covid_articles <- renderText({
    n <- indicator_count(article_data, "mentions_covid")
    if (is.na(n)) "N/A" else format(n, big.mark = ",")
  })
  
  output$ta_top_word <- renderText({
    if (!is.null(top_words) && all(c("word", "n") %in% names(top_words))) {
      top_words |>
        arrange(desc(n)) |>
        slice(1) |>
        pull(word)
    } else {
      "\"travel\""
    }
  })
  
  # -------- Topic terms plot --------
  
  output$ta_topics_plot <- renderPlotly({
    req(topics_results, input$topic_select)
    
    df <- topics_results
    validate(
      need(all(c("topic", "term", "beta") %in% names(df)),
           "Topic results file is missing required columns.")
    )
    
    df_topic <- df |>
      filter(topic == input$topic_select) |>
      arrange(desc(beta)) |>
      slice_head(n = input$top_terms_n) |>
      mutate(term = forcats::fct_reorder(term, beta))
    
    topic_name <- topic_labels[as.character(input$topic_select)]
    
    p <- ggplot(df_topic, aes(x = beta, y = term)) +
      geom_col(fill = pal_florida) +
      theme_minimal(base_size = 13) +
      labs(
        x = "Term importance (beta)",
        y = NULL,
        title = paste("Top terms for:", topic_name)
      )
    
    ggplotly(p, tooltip = c("y", "x")) |>
      layout(margin = list(t = 70))
  })
  
  # -------- Sentiment timeline --------
  
  output$ta_sentiment_plot <- renderPlotly({
    req(sentiment_df)
    
    df <- sentiment_df
    validate(
      need(all(c("date", "avg_sentiment", "pct_positive") %in% names(df)),
           "sentiment_timeline.csv must include time_period/avg_sentiment/pct_positive.")
    )
    
    if (isTRUE(input$use_pct_positive)) {
      df_plot <- df |>
        mutate(value = pct_positive / 100) |>
        select(date, value)
      
      y_lab  <- "% positive articles"
      y_fmt  <- scales::label_percent(accuracy = 1)
      baseline <- 0.5   # 50% threshold
    } else {
      df_plot <- df |>
        select(date, value = avg_sentiment)
      
      y_lab  <- "Average sentiment score"
      y_fmt  <- scales::label_number(accuracy = 1)
      baseline <- 0
    }
    
    p <- ggplot(df_plot, aes(x = date, y = value)) +
      geom_line(color = pal_florida, linewidth = 1.2) +
      geom_point(color = pal_florida, size = 2) +
      geom_hline(yintercept = baseline,
                 linetype = "dashed", color = pal_neutral) +
      scale_y_continuous(labels = y_fmt) +
      scale_x_date(date_labels = "%Y") +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
      ) +
      labs(x = NULL, y = y_lab)
    
    ggplotly(p) |>
      layout(margin = list(t = 50))
  })
  
  # -------- State mentions --------
  
  output$ta_states_plot <- renderPlotly({
    req(state_mentions)
    
    df <- state_mentions
    validate(
      need(all(c("state", "count") %in% names(df)),
           "state_mentions.csv needs 'state' and 'count' columns.")
    )
    
    df <- df |>
      slice_max(order_by = count, n = 10) |>
      mutate(state = forcats::fct_reorder(state, count))
    
    p <- ggplot(df, aes(x = count, y = state)) +
      geom_col(fill = pal_florida) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(size = 14, face = "bold")
      ) +
      labs(
        title = "Top ten states mentioned in articles",
        x = "Number of articles",
        y = NULL
      )
    
    ggplotly(p) |>
      layout(margin = list(t = 70))
  })
  
  # -------- Word cloud --------
  
  output$ta_wordcloud <- renderPlot({
    req(top_words)
    
    df <- top_words
    validate(
      need(all(c("word", "n") %in% names(df)),
           "top_words.csv needs 'word' and 'n' columns.")
    )
    
    df <- df |>
      filter(!word %in% c("news.google.com")) |>
      arrange(desc(n)) |>
      slice_head(n = 80)
    
    par(mar = c(0, 0, 0, 0))
    wordcloud(
      words        = df$word,
      freq         = df$n,
      max.words    = 80,
      colors       = viridis(8),
      random.order = FALSE,
      scale        = c(3.2, 0.8)
    )
  })
  
  # -------- Article browser --------
  
  output$ta_articles_table <- renderDT({
    req(article_data)
    
    cols <- intersect(
      c(
        "article_id", "filename", "title", "time_period",
        "mentions_florida", "mentions_california",
        "mentions_covid", "positive", "negative",
        "sentiment_score"
      ),
      names(article_data)
    )
    
    df <- article_data[, cols, drop = FALSE]
    
    # Convert logicals to Yes/No for display
    logical_cols <- c("mentions_florida", "mentions_california", "mentions_covid",
                      "positive", "negative")
    
    for (col in logical_cols) {
      if (col %in% names(df)) {
        df[[col]] <- ifelse(df[[col]] == TRUE, "Yes",
                            ifelse(df[[col]] == FALSE, "No", df[[col]]))
      }
    }
    
    nice_names <- c(
      article_id          = "ID",
      filename            = "File",
      title               = "Title",
      time_period         = "Year",
      mentions_florida    = "Mentions Florida",
      mentions_california = "Mentions California",
      mentions_covid      = "Mentions COVID",
      positive            = "Positive",
      negative            = "Negative",
      sentiment_score     = "Sentiment score"
    )
    colnames(df) <- nice_names[colnames(df)]
    
    datatable(
      df,
      options  = list(
        pageLength = 25,
        autoWidth  = TRUE,
        searching  = FALSE,   # no global search box
        dom        = "tip"    # table + information + pagination
      ),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)