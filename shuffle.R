# shuffle experiment

library(tidyverse)

# clear workspace
rm(list=ls())

cards <-  1:52
max_clump <- 4


human_shuffle <- function(cards, max_clump=4){
  # a function meant to imitate a human shuffle
  
  # split in 2 (top and bottom)
  n_cards <- length(cards)
  split <- n_cards / 2 %>% ceiling()
  top <- cards[1:split]
  bottom <- cards[-(1:split)]
  
  n_top <- length(top)
  n_bottom <- length(bottom)
  
  # randomly chose a number 1:max_clump (check that enough cards are left)
  shuffled_cards <- rep(NA, n_cards)
  shuffle_pointer <- 1
  top_pointer <- 1
  bottom_pointer <- 1
  iter <- 0
  
  while(shuffle_pointer <= n_cards){
    if(top_pointer <= n_top){
      # get valid chunk size
      chunk_size <- sample(1:max_clump,1) - 1
      chunk_size <- ifelse(top_pointer + chunk_size > n_top, n_top - top_pointer, chunk_size)
      # add chunk from top to shuffled deck
      shuffled_cards[shuffle_pointer:(shuffle_pointer+chunk_size)] <-
        top[top_pointer:(top_pointer+chunk_size)]
      # update pointers
      top_pointer <- top_pointer + chunk_size + 1
      shuffle_pointer <- shuffle_pointer + chunk_size + 1
    }

    if(bottom_pointer <= n_bottom){
      # get valid chunk size
      chunk_size <- sample(1:max_clump,1) - 1
      chunk_size <- ifelse(bottom_pointer + chunk_size > n_bottom, n_bottom - bottom_pointer, chunk_size)
      # add chunk from bottom to shuffled deck
      shuffled_cards[shuffle_pointer:(shuffle_pointer+chunk_size)] <- 
        bottom[bottom_pointer:(bottom_pointer+chunk_size)]
      # update pointers
      bottom_pointer <- bottom_pointer + chunk_size + 1
      shuffle_pointer <- shuffle_pointer + chunk_size + 1
    }
    iter <- iter + 1
    # cat(iter,shuffle_pointer,'\n')
    
  }
  
  
  return(shuffled_cards)
}


examine_results <- function(cards){
  # check how many cards are still next to their neighbors
  (diff(cards, lag = 1) == 1) %>% sum()
  
}


# test out the functions

deck <- 1:52
deck

shuffled_deck <- human_shuffle(deck)
shuffled_deck
length(shuffled_deck)
examine_results(shuffled_deck)


# write a function to simulate the process of shuffling a deck 20 times
simulate_shuffles <- function(cards, n_shuffles=20, n_iter=1000, max_clump = 4, human=TRUE){
  # instsantiate a data frame
  df <- matrix(NA, nrow = n_iter,ncol = n_shuffles) %>% as.data.frame()
  
  # fill in the dataframe with results
  for(i in 1:n_iter){
    # reset deck
    deck <- cards
    for(s in 1:n_shuffles){
      if(human){
        shuffled <- human_shuffle(deck, max_clump = max_clump) 
      }
      else{
        shuffled <- sample(deck)
      }
      
      df[i,s] <- examine_results(shuffled)
      deck <- shuffled 
    }
  }
  
  return(df)
}

deck <- 1:52
results <- simulate_shuffles(deck, max_clump = 3)
dim(results)
head(results)

avg <- sapply(results, mean)

# plot average unshuffled cards per shuffle
qplot(x = 1:20, y=avg, geom = 'line')


deck <- 1:52
results <- simulate_shuffles(deck, human = FALSE)
dim(results)
head(results)

avg <- sapply(results, mean)

# plot average unshuffled cards per shuffle
qplot(x = 1:20, y=avg, geom = 'line')

# are different shuffling strategies more effecient?
# how many human shuffles does it take to match pseudo random on average with the different techniques

