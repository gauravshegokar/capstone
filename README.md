# Data Science Specialization Capstone Project
The goal of this project is to predict the next word, given a partial English sentence.

This Shiny app was developed for the Capstone Project of the Johns Hopkins Coursera Data Science Specialization. This Capstone Project is designed in partnership with Swiftkey.

# Model Building

The project is based on Markov models. That is we can approximate future event by just knowing last few events. Tokenization is done and ngrams are computed. I have used ngrams upto 4. That means to predict next word we only see last 3 words for prediction.

*Stupid backoff algorithm* is used for prediction due to its simplycity and high speed. It performs quite well given very large data with accuracy comparable to some more complex models.

# Scripts

1. scripts folder - scripts for getting, cleaning, exploratory data analysis, model building.
2. shinyApp folder - shiny app and necessary data
3. Exploratory report  