# Server
server <- function(input,output){
  # Create a subset of data filtering for transmission type
  # get the trigram prefix from input text
  select_3gram <- function(input_string){
    input_split <- unlist(strsplit(input_string," "))
    input_split[(length(input_split)-2):length(input_split)]
  }
  
  select_2gram <- function(input_string){
    input_split <- unlist(strsplit(input_string," "))
    input_split[(length(input_split)-1):length(input_split)]
  }
  
  select_1gram <- function(input_string){
    input_string
  }
  
  quadgram_discount <- 0.5
  trigram_discount <- 0.5
  bigram_discount <- 0.5
  #trigram_prefix <- reactive({select_3gram(tolower(input$text))})
  output$input_text <- reactive({input$text})
  
  # Load n-gram tables: 1-gram to 4-grams
  #saveRDS(en_us_all_count_1,file = "unigram_table")
  #saveRDS(en_us_all_count_2,file = "bigram_table")
  #saveRDS(en_us_all_count_3,file = "trigram_table")
  #saveRDS(en_us_all_count_4,file = "quadgram_table")
  
  en_us_all_count_1 <- readRDS("unigram_table")
  en_us_all_count_2 <- readRDS("bigram_table")
  en_us_all_count_3 <- readRDS("trigram_table")
  en_us_all_count_4 <- readRDS("quadgram_table")
    
    trigram_prefix <- reactive({select_3gram(tolower(input$text))})  
    obs_quadgram_table <- reactive({filter(en_us_all_count_4,word1==trigram_prefix()[1]& word2==trigram_prefix()[2]& word3==trigram_prefix()[3])})
    
    ## 5.3 Calculate denominator for probability of 4-grams (count of trigram prefix)
    
    trigram_prefix_count <- reactive({filter(en_us_all_count_3,word1==trigram_prefix()[1]&word2==trigram_prefix()[2]&word3==trigram_prefix()[3])$n})
    
    ###### **********************Comments***********************************
    
    ## bigram prefix count can be calculated from bigram table or from sum of all counts in observed trigram table 
    ## based on the bigram prefix. there is a possibility that some trigrams containng the bigram prefix 
    ## got removed due to the third word being filtered out in a cleanup. hence the two counts may not match.
    
    ###### **********************End of Comments*****************************
    
    
    ## 5.4 Calculate probability of observed 4-grams based on trigram prefix
    obs_quadgram_table2 <- reactive({df <- obs_quadgram_table()
    df$prob <- (obs_quadgram_table()$n-quadgram_discount)/trigram_prefix_count()
    df })
    #obs_quadgram_table()$prob <- reactive({(obs_quadgram_table()$n-quadgram_discount)/trigram_prefix_count()})
    
    
    ## 5.5 Calculate left over probability mass for unobserved 4-grams
    quad_leftover_prob <- reactive({1-(sum(obs_quadgram_table2()$prob))})
    
    # this should be distributed to all trigrams - observed(excluding those in observed quadgram) 
    # and unobserved that has been adjusted to 3-gram level.
    
    ## 5.3.1 Look for observed 3-grams
    
    obs_bo_trigrams <- reactive({filter(en_us_all_count_3,word1==trigram_prefix()[2]&word2==trigram_prefix()[3])})
    
    ## calculate probabilities of observed 3-grams.
    obs_bo_trigrams2 <- reactive({ df1 <- obs_bo_trigrams()
    df1$prob <- (obs_bo_trigrams()$n-trigram_discount)/sum(obs_bo_trigrams()$n)
    df1 })
    
    
    
    # calculate left over probability from observed trigrams to allocate to unobserved trigrams
    tri_leftover_prob <- reactive({1-sum(obs_bo_trigrams2()$prob)})
    
    # Back off to 2-grams to look for observed 2-grams with THE_WORD4. 
    # the probabilities need to be adjusted back to 3-grams and 4-grams
    
    obs_bo_bigrams <- reactive({filter(en_us_all_count_2,word1==trigram_prefix()[3])})
    
    # calculate the probabilities of the observed bigrams 
    # in order to extract remaining probability mass for unobserved bigrams
    
    obs_bo_bigrams2 <- reactive({df2 <- obs_bo_bigrams()
    df2$prob <- (obs_bo_bigrams()$n-bigram_discount)/sum(obs_bo_bigrams()$n)
    df2})
    
    
    bi_leftover_prob <- reactive({1-sum(obs_bo_bigrams2()$prob)})
    
    # Find unobserved unigrams across 2,3 and 4 grams -base it on 2-grams as that will be a superset of 3 and 4-grams as well
    
    unigram_tail <- reactive({en_us_all_count_1[!en_us_all_count_1$word %in% obs_bo_bigrams2()$word2,]})
    
    ## distribute the left over bigram probability to these unobserved unigrams in 2,3 and 4 grams
    
    unigram_tail2 <- reactive({df3 <- unigram_tail()
    df3$prob <- bi_leftover_prob()*unigram_tail()$n/sum(unigram_tail()$n)
    df3})
    
    
    #change structure of unigram tail table to match structure of observed 2-gram table
    
    unigram_tail3 <- reactive({df4 <- unigram_tail2()
    df4$word1 <- trigram_prefix()[3]
    names(df4)[1] <- "word2"
    df4 <- df4[,c(4,1,2,3)]
    df4})
    
    
    
    # remove tail word in bigrams which was already observed in trigrams (and hence 4-grams as well)
    obs_bo_bigrams_unique <- reactive({obs_bo_bigrams2()[!obs_bo_bigrams2()$word2 %in% obs_bo_trigrams2()$word3,]})
    
    # combine unigram tail and unique BO bigram table to get final BO bigram table
    
    total_bo_bigrams <- reactive({rbind(obs_bo_bigrams_unique(),unigram_tail3())})
    
    # Adjust to Trigram level - Allocate trigram left over probability to total BO bigram table - 
    
    total_bo_bigrams2 <- reactive({df5 <- total_bo_bigrams()
    df5$prob <- tri_leftover_prob()*total_bo_bigrams()$prob/sum(total_bo_bigrams()$prob)
    df5})
    
    # Match structure of 3-grams
    
    adj_bo_trigrams <- reactive({total_bo_bigrams2()})
    adj_bo_trigrams2 <- reactive({df6 <- adj_bo_trigrams()
    names(df6)[1:2] <- c("word2","word3")
    df6$word1 <- trigram_prefix()[2]
    df6 <- df6[,c(5,1,2,3,4)]
    #df6 <- df6[,-5]
    #names(df6)[5] <- "prob"
    df6 })
    
    
    # check tail word in obs trigram table not in obs 4-gram table
    
    obs_bo_trigrams_unique <- reactive({obs_bo_trigrams2()[!obs_bo_trigrams2()$word3 %in% obs_quadgram_table2()$word4,]})
    
    # Combine adjusted BO trigrams and obs BO trigrams unique from 4-grams
    
    total_bo_trigrams <- reactive({rbind(obs_bo_trigrams_unique(),adj_bo_trigrams2())})
    
    # Allocate 4-gram left over probability to total BO trigrams
    
    total_bo_trigrams2 <- reactive({df7 <- total_bo_trigrams()
    df7$prob <- quad_leftover_prob()*total_bo_trigrams()$prob/sum(total_bo_trigrams()$prob)
    df7 })
    
    
    # match total BO trigrams structure to 4-grams
    
    total_bo_trigrams3 <- reactive({df8 <- total_bo_trigrams2()
    names(df8)[1:3] <- c("word2","word3","word4")
    df8$word1 <- trigram_prefix()[1]
    df8 <- df8[,c(6,1,2,3,4,5)]
    df8})
    
    #names(total_bo_trigrams())[1:3] <- c("word2","word3","word4")
    #total_bo_trigrams()$word1 <- trigram_prefix()[1]
    #total_bo_trigrams() <-  total_bo_trigrams()[,c(7,1,2,3,4,6)]
    #names(total_bo_trigrams)()[6]<- "prob"
    
    # bind observed and unobserved 4-grams
    
    total_bo_quadgrams <- reactive({rbind(obs_quadgram_table2(),total_bo_trigrams3())})
    final_bo_quadgrams <- reactive({arrange(total_bo_quadgrams(),desc(prob))[1:3,]})
    
    output$testtable <- renderDataTable({final_bo_quadgrams()})
    
    # Create descritive text
    output$prediction1 <- renderText({final_bo_quadgrams()$word4[1]})
    output$prediction2 <- renderText({final_bo_quadgrams()$word4[2]})
    output$prediction3 <- renderText({final_bo_quadgrams()$word4[3]})
}
