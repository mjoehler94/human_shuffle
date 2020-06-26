# shuffle experiment

library(tidyverse)

# clear workspace
rm(list=ls())

cards = 1:52
max_clump=4


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
      chunk_size <- sample(1:max_clump,1)
      chunk_size <- ifelse(top_pointer + chunk_size > n_top, n_top - top_pointer, chunk_size)
      shuffled_cards[shuffle_pointer:(shuffle_pointer+chunk_size-1)] <- top[top_pointer:(top_pointer+chunk_size-1)]
      top_pointer <- top_pointer + chunk_size
      shuffle_pointer <- shuffle_pointer + chunk_size
    }
    
    if(bottom_pointer <= n_bottom){
      chunk_size <- sample(1:max_clump,1)
      chunk_size <- ifelse(bottom_pointer + chunk_size > n_bottom, n_bottom - bottom_pointer, chunk_size)
      shuffled_cards[shuffle_pointer:(shuffle_pointer+chunk_size-1)] <- bottom[bottom_pointer:(bottom_pointer+chunk_size-1)]
      bottom_pointer <- bottom_pointer + chunk_size
      shuffle_pointer <- shuffle_pointer + chunk_size
    }
    iter <- iter + 1
    cat(iter,shuffle_pointer,'\n')
  }

  return(shuffled_cards)
}


examine_results <- function(cards){
  # check how many cards are still next to their neighbors
  
}


# test out the functions (there is still something wrong with this, end deck has NA values)

deck <- 1:52
deck

shuffled_deck <- human_shuffle(deck)
shuffled_deck
length(shuffled_deck)
