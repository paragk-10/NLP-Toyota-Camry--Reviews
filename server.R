rm(list=ls(all=TRUE))
require('rvest')
library(rvest)
require('purrr')
library(purrr)
require('tm')
library(tm)
require('DT')
library(DT)
require('stringr')
library(stringr)
require('dplyr')
library(dplyr)
require('tidytext')
library(tidytext)
require('data.table')
library(data.table)
require('shiny')
library(shiny)
require('shinythemes')
library(shinythemes)
require('nnet')
library(nnet)
require('ggplot2')
library(ggplot2)


UI <- fluidPage( 
  theme =shinytheme("darkly"),
  titlePanel("ParagKedia_NLP"),
  navlistPanel(
    tabPanel("Display Train, Test set",
             tabsetPanel(id = "tab",
                         tabPanel("Training Set", dataTableOutput(outputId = "ToyotaCamry_Train")),
                         tabPanel("Test Set", dataTableOutput(outputId="ToyotaCamry_Test"))
             )
    ),
    
    tabPanel("Display Normalized Train, Test set",
             tabsetPanel(id = "tab",
                         tabPanel("Training Set", dataTableOutput(outputId = "ToyotaCamry_Train1")),
                         tabPanel("Test Set", dataTableOutput(outputId="ToyotaCamry_Test1"))
             )
    ),
    tabPanel("Display Tags for Train, Test set",
             tabsetPanel(id = "tab",
                         tabPanel("Training Set", dataTableOutput(outputId = "ToyotaCamry_Train2")),
                         tabPanel("Test Set", dataTableOutput(outputId="ToyotaCamry_Test2"))
             )
    ),
    tabPanel("Display Sentiment score for Train, Test set",
             tabsetPanel(id = "tab",
                         tabPanel("Training Set", dataTableOutput(outputId = "afinn_score_Train")),
                         tabPanel("Test Set", dataTableOutput(outputId="afinn_score_Test"))
             )
    ),
    tabPanel("Score according to Tags",
             tabsetPanel(id = "tab",
                         tabPanel("Score according to Tags", dataTableOutput(outputId = "tags_n_mean"))
             )
    ),
    tabPanel("Summary & plots",
             tabsetPanel(id = "tab",
                         tabPanel("Plot1", plotOutput("hist1")),
                         tabPanel("Plot2", plotOutput("hist2")),
                         tabPanel("Plot3", plotOutput("hist3")),
                         tabPanel("Summary", verbatimTextOutput("stats")),
                         tabPanel("Accuracy", textOutput(outputId = "avgsentiment6"))
                         
             )
             ),
             tabPanel("Text and Comparisons",
                      tabsetPanel(id = "tab",
                                  tabPanel("Avg_Train_Score", textOutput(outputId = "avgsentiment")),                                  
                                  tabPanel("Comparison", textOutput(outputId = "avgsentiment1")),
                                  tabPanel("Comparison_Service", textOutput(outputId = "avgsentiment2")),
                                  tabPanel("Comparison_Price", textOutput(outputId = "avgsentiment3")),
                                  tabPanel("Comparison_Handling", textOutput(outputId = "avgsentiment4")),
                                  tabPanel("Comparison_Interior", textOutput(outputId = "avgsentiment5"))
                                  
                      )
    ),
    tabPanel("TF-IDF score and Plot",
             tabsetPanel(id = "tab",
                         tabPanel("TF-IDF Table", dataTableOutput(outputId = "TDIDFTableOutput")),
                         tabPanel("Plot", plotOutput("hist4"))
             )
    )
  )
)


Server<-function(input,output)
{
  url <- 'https://www.cars.com/research/toyota-camry-201%d/consumer-reviews/?nr=500&pg=1'
  ToyotaCamry_Train<-map_df(2:6, function(i){
    page<-read_html(sprintf(url,i))
    data.frame(Year=paste("201",i,sep = ""),
               Rating=html_attr(html_nodes(page, '.cr-star-rating'),"rating"),
               Text=html_text(html_nodes(page, '.mmy-reviews__blurb div span')),
               stringsAsFactors = FALSE)
  })
  output$ToyotaCamry_Train<-renderDataTable(
    datatable(
      {
        return(ToyotaCamry_Train)
      }
    )
  )
  
  #I have hardcoded the main links because I really tried getting an input link but I couldn't. So this hardcoded link which takes first 500 reviews for each year. 
  
  url2 <- 'https://www.cars.com/research/toyota-camry-2017/consumer-reviews/?nr=500&pg=1'
  page2<-read_html(sprintf(url2))
  ToyotaCamry_Test<-data.frame(Year=paste("2017"),
                               Rating=html_attr(html_nodes(page2, '.cr-star-rating'),"rating"),
                               Text=html_text(html_nodes(page2, '.mmy-reviews__blurb div span')),
                               stringsAsFactors = FALSE)
  output$ToyotaCamry_Test<-renderDataTable(
    datatable(
      {
        return(ToyotaCamry_Test)
      }
    )
  )
  
  ToyotaCamry_Train1<-ToyotaCamry_Train
  ToyotaCamry_Train1$Normalized_Review<-gsub('[[:punct:] ]+',' ',ToyotaCamry_Train1$Text)
  ToyotaCamry_Train1$Normalized_Review<-tolower(ToyotaCamry_Train1$Normalized_Review)
  output$ToyotaCamry_Train1<-renderDataTable(
    datatable(
      {
        return(ToyotaCamry_Train1)
      }
    )
  )
  
  #These are the normal punctuation removal and uppercase to lowercase operations.
  
  ToyotaCamry_Test1<-ToyotaCamry_Test
  ToyotaCamry_Test1$Normalized_Review<-gsub('[[:punct:] ]+',' ',ToyotaCamry_Test1$Text)
  ToyotaCamry_Test1$Normalized_Review<-tolower(ToyotaCamry_Test1$Normalized_Review)
  output$ToyotaCamry_Test1<-renderDataTable(
    datatable(
      {
        return(ToyotaCamry_Test1)
      }
    )
  )
  
  #created a list of tags and extracted if any of the 4 tags is found in the review and displayed it for the Train and Test sets
  
  tags<-c("service","price", "handling", "interior") 
  Regex <- paste0('(', paste(tags, collapse = '|'), ')')
  ToyotaCamry_Train2<-ToyotaCamry_Train1
  ToyotaCamry_Train2<-ToyotaCamry_Train2 %>% mutate(Tag = sapply(str_extract_all(Normalized_Review,Regex), function(x) paste(unique(x), collapse=',')))
  output$ToyotaCamry_Train2<-renderDataTable(
    datatable(
      {
        return(ToyotaCamry_Train2)
      }
    )
  )
  
  ToyotaCamry_Test2<-ToyotaCamry_Test1
  ToyotaCamry_Test2<-ToyotaCamry_Test1 %>% mutate(Tag = sapply(str_extract_all(Normalized_Review, Regex), function(x) paste(unique(x), collapse=',')))
  output$ToyotaCamry_Test2<-renderDataTable(
    datatable(
      {
        return(ToyotaCamry_Test2)
      }
    )
  )
  
  #Tokenized the words in the training set
  
  ToyotaCamry_Train3<-ToyotaCamry_Train2
  Train_tokenized <- data.frame(Review_No = 1:nrow(ToyotaCamry_Train3), Reviews = ToyotaCamry_Train3$Normalized_Review, Rating = ToyotaCamry_Train3$Rating, REVIEW = ToyotaCamry_Train3$Text, stringsAsFactors = FALSE) %>%  unnest_tokens (word, Reviews)
  afinn_score_Train <- Train_tokenized %>%
    anti_join (stop_words) %>%
    inner_join (get_sentiments("afinn")) %>% 
    group_by (Review_No = Review_No, REVIEW = REVIEW,Ratings=Rating) %>%
    mutate (method = "AFINN") %>%
    summarise (sentiment_score = mean(score)) 
  mean (afinn_score_Train$sentiment_score)
  #Calculated the sentiment score based on the AFINN parameter for all the reviews
  
  output$afinn_score_Train<-renderDataTable(
    datatable(
      {
        return(afinn_score_Train)
      }
    )
  )
  
  #Tokenized the words in the test set
  
  ToyotaCamry_Test3<-ToyotaCamry_Test2
  Test_tokenized <- data.frame(Review_No = 1:nrow(ToyotaCamry_Test3), Reviews = ToyotaCamry_Test3$Normalized_Review, Rating = ToyotaCamry_Test3$Rating, REVIEW = ToyotaCamry_Test3$Text, stringsAsFactors = FALSE) %>% unnest_tokens(word, Reviews)
  afinn_score_Test <- Test_tokenized %>%
    anti_join (stop_words) %>%
    inner_join (get_sentiments("afinn")) %>%
    group_by (Review_No = Review_No, REVIEW = REVIEW,Ratings=Rating) %>%
    mutate (method = "AFINN") %>%
    summarise (sentiment_score = mean(score)) 
  
  output$afinn_score_Test<-renderDataTable(
    datatable(
      {
        return(afinn_score_Test)
      }
    )
  )
  
  output$avgsentiment <- renderText(
    {
      text <- paste("The average sentiment score for the training data is",   mean (afinn_score_Train$sentiment_score))
      return(text)
    }
  )
  output$avgsentiment1 <- renderText(
    {
      text1 <- paste("The average star rating is",   mean (as.numeric(ToyotaCamry_Train$Rating)))
      return(text1)
    }
  )

  #Calculated the average sentiment rating of the reviews corresponding to each of the 4 tags
  service_tag <- ToyotaCamry_Train3[ToyotaCamry_Train3$Tag %like% 'service',]
  service_tag <- data.frame (Review_No = 1:nrow(service_tag), Rating = service_tag$Rating, Reviews = service_tag$Normalized_Review, REVIEW = service_tag$Text, stringsAsFactors = FALSE) %>% unnest_tokens(word, Reviews)
  afinn_score_service_tag <- service_tag %>%
    anti_join (stop_words) %>%
    inner_join (get_sentiments("afinn")) %>%
    group_by (Review_No = Review_No, REVIEW = REVIEW, Ratings=Rating) %>%
    mutate (method = "AFINN") %>%
    summarise (sentiment_score = mean(score))
  mean(afinn_score_service_tag$sentiment_score)
  
  price_tag <- ToyotaCamry_Train3[ToyotaCamry_Train3$Tag %like% 'price',]
  price_tag <- data.frame (Review_No = 1:nrow(price_tag), Rating = price_tag$Rating, Reviews = price_tag$Normalized_Review, REVIEW = price_tag$Text, stringsAsFactors = FALSE) %>% unnest_tokens(word, Reviews)
  afinn_score_price_tag <- price_tag %>%
    anti_join (stop_words) %>%
    inner_join (get_sentiments("afinn")) %>%
    group_by (Review_No = Review_No, REVIEW = REVIEW, Ratings=Rating) %>%
    mutate (method = "AFINN") %>%
    summarise (sentiment_score = mean(score))
  mean(afinn_score_price_tag$sentiment_score)
  
  handling_tag <- ToyotaCamry_Train3[ToyotaCamry_Train3$Tag %like% 'handling',]
  handling_tag <- data.frame (Review_No = 1:nrow(handling_tag), Rating = handling_tag$Rating, Reviews = handling_tag$Normalized_Review, REVIEW = handling_tag$Text, stringsAsFactors = FALSE) %>% unnest_tokens(word, Reviews)
  afinn_score_handling_tag <- handling_tag %>%
    anti_join (stop_words) %>%
    inner_join (get_sentiments("afinn")) %>%
    group_by (Review_No = Review_No, REVIEW = REVIEW, Ratings=Rating) %>%
    mutate (method = "AFINN") %>%
    summarise (sentiment_score = mean(score))
  mean(afinn_score_handling_tag$sentiment_score)
  
  interior_tag <- ToyotaCamry_Train3[ToyotaCamry_Train3$Tag %like% 'interior',]
  interior_tag <- data.frame (Review_No = 1:nrow(interior_tag), Rating = interior_tag$Rating, Reviews = interior_tag$Normalized_Review, REVIEW = interior_tag$Text, stringsAsFactors = FALSE) %>% unnest_tokens(word, Reviews)
  afinn_score_interior_tag <- interior_tag %>%
    anti_join (stop_words) %>%
    inner_join (get_sentiments("afinn")) %>%
    group_by (Review_No = Review_No, REVIEW = REVIEW, Ratings=Rating) %>%
    mutate (method = "AFINN") %>%
    summarise (sentiment_score = mean(score))
  mean(afinn_score_interior_tag$sentiment_score)
  
  #Displayed the average sentiment rating of the reviews corresponding to each of the 4 tags along with the total number of rows in which the tags are found
  service_tag1 <- data.frame(Tag_Name = paste("Service tag"),No_of_rows_for_Tag = nrow(afinn_score_service_tag) , Avg_sentiment_Score = mean(afinn_score_service_tag$sentiment_score), stringsAsFactors = FALSE) 
  price_tag1 <- data.frame(Tag_Name = paste("Price tag"),No_of_rows_for_Tag = nrow(afinn_score_price_tag) , Avg_sentiment_Score = mean(afinn_score_price_tag$sentiment_score), stringsAsFactors = FALSE)
  handling_tag1 <- data.frame(Tag_Name = paste("Handling tag"),No_of_rows_for_Tag = nrow(afinn_score_handling_tag) , Avg_sentiment_Score = mean(afinn_score_handling_tag$sentiment_score), stringsAsFactors = FALSE)
  interior_tag1 <- data.frame(Tag_Name = paste("Interior tag"),No_of_rows_for_Tag = nrow(afinn_score_interior_tag) , Avg_sentiment_Score = mean(afinn_score_interior_tag$sentiment_score), stringsAsFactors = FALSE)
  tags_n_mean<-rbind(service_tag1,price_tag1,handling_tag1,interior_tag1, stringsAsFactors = FALSE) 
  
  output$tags_n_mean<-renderDataTable(
    datatable(
      {
        return(tags_n_mean)
      }
    )
  )
  
  output$avgsentiment2 <- renderText(
    {
      text <- paste("The average star rating corresponding to tag 'service' is", mean(as.numeric(afinn_score_service_tag$Ratings)), "whereas average star rating by user is", mean (as.numeric(ToyotaCamry_Train$Rating)))
      return(text)
    }
  )
  
  output$avgsentiment3 <- renderText(
    {
      text <- paste("The average star rating corresponding to tag 'Price' is", mean(as.numeric(afinn_score_price_tag$Ratings)), "whereas average star rating by user is", mean (as.numeric(ToyotaCamry_Train$Rating)))
      return(text)
    }
  )
  
  output$avgsentiment4 <- renderText(
    {
      text <- paste("The average star rating corresponding to tag 'handling' is", mean(as.numeric(afinn_score_handling_tag$Ratings)), "whereas average star rating by user is", mean (as.numeric(ToyotaCamry_Train$Rating)))
      return(text)
    }
  )
  
  output$avgsentiment5 <- renderText(
    {
      text <- paste("The average star rating corresponding to tag 'interior' is", mean(as.numeric(afinn_score_interior_tag$Ratings)), "whereas average star rating by user is", mean (as.numeric(ToyotaCamry_Train$Rating)))
      return(text)
    }
  )
  
  output$hist1<-renderPlot(
    {
      hist(afinn_score_Train$sentiment_score)
    }
  )
  output$hist2<-renderPlot(
    {
      hist(as.numeric(ToyotaCamry_Train$Rating))
    }
  )
  output$hist3<-renderPlot(
    {
      hist(afinn_score_Test$sentiment_score)
    }
  )
  
  #trained the model and predicted the results
  Training_set<-afinn_score_Train[,-c(1,2)]
  Test_set<-afinn_score_Test[,-c(1,2)]
  set.seed(123)
  classifier = glm(as.factor(Training_set$Ratings) ~ ., data = Training_set, family = binomial(link = "logit"))

  output$stats<-renderPrint(
    {
      summary(classifier)
    }
  )
  
  pred<-round(predict(classifier, Test_set))
  pred<-ifelse(pred>5,5,pred)
  pred1<-table(pred)
  pred11<-as.data.frame(pred1)
  pred2<-table(Test_set$Ratings)
  pred21<-as.data.frame(pred2)
  
  #We can see the prediction rate for Rating='5' and also the AIC in the model summary
  prediction_rate=pred11[5,2]/pred21[5,2]
  output$avgsentiment6 <- renderText(
    {
      text <- paste("Prediction rate is around: ",prediction_rate, "and AIC for the model can also be found in the Summary tab")
      return(text)
    }
  )
  
  #Calculated the TF-IDF for every word in the training set by removing the stop words
  TFIDF_Train<-ToyotaCamry_Train1
  TFIDF_Train<-TFIDF_Train %>% mutate(Tag = sapply(str_extract_all(Normalized_Review,Regex), function(x) paste(unique(x), collapse=' ')))
  
  TFIDF_Train1 <- TFIDF_Train %>%
    mutate(line = row_number()) %>%
    unnest_tokens(word, Normalized_Review) %>%
    anti_join(stop_words, by = "word") %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(line) %>% mutate(sentiment = mean(score))

  TFIDF_Train1 <- TFIDF_Train1[!TFIDF_Train1$Tag == "", ]
  TFIDF_Train1 <- TFIDF_Train1 %>% unnest_tokens(Tags1, Tag)
  TDIDFTable <- TFIDF_Train1[, c(5,8)] #selected the relevant rows for computation

  TDIDFTable1 <- TDIDFTable %>% count(Tags1, word, sort = TRUE) %>% ungroup()

  #Displayed the TF-IDF for every word in the training set 
  
  TDIDFTableOutput <- TDIDFTable1 %>% bind_tf_idf(word, Tags1, n) %>% arrange(desc(tf_idf))
  output$TDIDFTableOutput<-renderDataTable(
    datatable(
      {
        return(TDIDFTableOutput)
      }
    )
  )
  
  #For each tag, ploted the TF-IDF scores for top 10 words 
  
  TDIDFTableOutput2 <- TDIDFTable1 %>% bind_tf_idf(word, Tags1, n) %>% arrange(desc(tf_idf)) %>% mutate(word = factor(word, levels = rev(unique(word)))) %>%
    mutate(Tags1 = factor(Tags1, levels = c("service","price", "handling","interior")))

  output$hist4<-renderPlot(
    {
      TDIDFTableOutput2 %>% group_by(Tags1) %>%  top_n(10, tf_idf) %>%  ungroup() %>% mutate(word = reorder(word, tf_idf)) %>%
      ggplot(aes(word, tf_idf, fill = Tags1)) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(~Tags1, ncol = 2, scales = "free") +
      coord_flip()
    }
  )
}
shinyApp(ui=UI, server = Server)

#I have remove the stop words for most of the operations like Tash 5, 6 as the sentiment score was high due to the inclusion of stop words and I found it relevant to remove the stop words