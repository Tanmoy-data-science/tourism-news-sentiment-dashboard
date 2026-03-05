# Tourism News Sentiment Dashboard

This repository contains an **R Shiny dashboard** for analyzing media narratives around tourism recovery.

The dashboard visualizes topic modeling results, sentiment trends, geographic mentions, and article metadata from tourism-related news coverage.

---

# Features

• Topic modeling visualization  
• Sentiment timeline  
• State mentions analysis  
• Word cloud of key terms  
• Article browser for exploring individual stories  

---

# Repository Structure

```
tourism-news-sentiment-dashboard
│
├── app.R
├── README.md
└── data
    ├── topics_results.csv
    ├── article_metadata.csv
    ├── state_mentions.csv
    ├── top_words.csv
    └── sentiment_timeline.csv
```

---

# Requirements

Install the required R packages:

```r
install.packages(c(
  "shiny",
  "bslib",
  "tidyverse",
  "readr",
  "plotly",
  "DT",
  "viridis",
  "wordcloud"
))
```

---

# Run the Dashboard

Clone the repository:

```
git clone https://github.com/Tanmoy-data-science/tourism-news-sentiment-dashboard.git
```

Open R and run:

```r
shiny::runApp("tourism-news-sentiment-dashboard")
```

Or if already inside the folder:

```r
shiny::runApp()
```

---

# Project Context

This dashboard was developed for:

**EPPS 6356 – Data Visualization**  
University of Texas at Dallas

The project analyzes tourism-related news coverage to explore patterns in media narratives surrounding tourism recovery.

---

# Author

Tanmoy Biswas
