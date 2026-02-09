# Jeśli nie masz pakietu shiny, odkomentuj poniższą linię
# install.packages("shiny")

library(shiny)

# ================================
# 1. Pobranie i przetwarzanie danych
# ================================
url <- "https://www.wdib.uw.edu.pl/badania/projekty-naukowe"
html <- readLines(url, encoding = "UTF-8", warn = FALSE)
html <- gsub("<script.*?</script>", "", html, ignore.case = TRUE)
html <- gsub("<style.*?</style>", "", html, ignore.case = TRUE)
html <- paste(html, collapse = " ")
html <- gsub("<[^>]+>", " ", html)
html <- gsub("&nbsp;", " ", html)
html <- gsub("\\s+", " ", html)
text <- trimws(html)

# Podział na projekty
blocks <- unlist(strsplit(
  text,
  "(?=[A-ZĄĆĘŁŃÓŚŹŻ ]{20,})",
  perl = TRUE
))
blocks <- blocks[nchar(blocks) > 300]

# Funkcja pomocnicza
extract_after <- function(txt, label) {
  if (!grepl(label, txt)) return(NA)
  sub(paste0(".*", label, "\\s*"), "", txt)
}

# Parsowanie danych
title <- funding <- leader <- period <- description <- character()
for (b in blocks) {
  sentences <- trimws(unlist(strsplit(b, "\\.")))
  sentences <- sentences[sentences != ""]
  title <- c(title, sentences[1])
  funding <- c(funding, extract_after(b, "Źródło finansowania"))
  leader  <- c(leader,  extract_after(b, "Kierownik"))
  period  <- c(period,  extract_after(b, "Okres realizacji"))
  description <- c(description, paste(sentences[-1], collapse = ". "))
}

projects <- data.frame(
  title = title,
  funding = funding,
  leader = leader,
  period = period,
  description = description,
  stringsAsFactors = FALSE
)

# Czyszczenie tekstu
projects$description <- tolower(projects$description)
projects$description <- gsub("[[:punct:]]", " ", projects$description)
projects$description <- gsub("[0-9]", " ", projects$description)
projects$description <- gsub("\\s+", " ", projects$description)
projects$description <- trimws(projects$description)
projects$desc_length <- nchar(projects$description)
n_projects <- nrow(projects)

# Tokenizacja i filtracja słów
words_by_doc <- strsplit(projects$description, " ")
words_by_doc <- lapply(words_by_doc, function(w) {
  w <- w[nchar(w) > 3]
  w[!w %in% c(
    "oraz","który","która","które","jest","być","będzie","został",
    "projekt","projekty","badanie","badania","naukowy","naukowe",
    "wdib","uw","uniwersytetu","warszawskiego","realizacja","realizowany"
  )]
})
words <- unique(unlist(words_by_doc))

# Macierz dokument–słowo (DTM)
dtm <- matrix(0, nrow = n_projects, ncol = length(words))
colnames(dtm) <- words
rownames(dtm) <- paste0("P", seq_len(n_projects))
for (i in seq_len(n_projects)) {
  tab <- table(words_by_doc[[i]])
  dtm[i, names(tab)] <- as.numeric(tab)
}

# TF-IDF
tf <- dtm / rowSums(dtm)
tf[is.na(tf)] <- 0
df <- colSums(dtm > 0)
idf <- log(n_projects / df)
tfidf <- tf * matrix(idf, nrow = n_projects, ncol = length(idf), byrow = TRUE)

# PCA (usuwa kolumny zerowe)
tfidf_nonzero <- tfidf[, colSums(tfidf) != 0]
pca <- prcomp(tfidf_nonzero, scale. = TRUE)

# Top słowa TF-IDF
top_tfidf <- sort(colMeans(tfidf_nonzero), decreasing = TRUE)[1:15]

# ================================
# 2. Shiny UI
# ================================
ui <- fluidPage(
  titlePanel("Analiza projektów WDIB UW"),
  sidebarLayout(
    sidebarPanel(
      h4("Statystyki projektów"),
      textOutput("nProjects"),
      textOutput("avgLength")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Histogram", plotOutput("histPlot")),
        tabPanel("Boxplot", plotOutput("boxPlot")),
        tabPanel("PCA", plotOutput("pcaPlot")),
        tabPanel("Top słowa TF-IDF", plotOutput("topWordsPlot"))
      )
    )
  )
)

# ================================
# 3. Shiny SERVER
# ================================
server <- function(input, output) {
  
  # Statystyki
  output$nProjects <- renderText({
    paste("Liczba projektów:", n_projects)
  })
  
  output$avgLength <- renderText({
    paste("Średnia długość opisów:", round(mean(projects$desc_length),1), "znaków")
  })
  
  # Histogram
  output$histPlot <- renderPlot({
    hist(projects$desc_length,
         col = "lightblue",
         border = "gray",
         main = "Długość opisów projektów WDIB",
         xlab = "Liczba znaków")
  })
  
  # Boxplot
  output$boxPlot <- renderPlot({
    boxplot(projects$desc_length,
            col = "lightgreen",
            main = "Zróżnicowanie długości opisów",
            ylab = "Liczba znaków")
  })
  
  # PCA
  output$pcaPlot <- renderPlot({
    plot(pca$x[,1], pca$x[,2],
         pch = 19, col = "steelblue",
         xlab = "PC1", ylab = "PC2",
         main = "PCA projektów WDIB (TF-IDF)")
    text(pca$x[,1], pca$x[,2],
         labels = rownames(tfidf_nonzero),
         pos = 3, cex = 0.7)
  })
  
  # Top słowa TF-IDF
  output$topWordsPlot <- renderPlot({
    barplot(top_tfidf,
            las=2, col="tomato",
            main="Najbardziej charakterystyczne słowa (TF-IDF)",
            ylab="Średni TF-IDF")
  })
  
}

# ================================
# 4. Uruchomienie aplikacji
# ================================
shinyApp(ui=ui, server=server)
