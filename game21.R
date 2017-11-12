#GLOBAL VARIABLES
cardnumber<-c("card1",
"card2",
"card3",
"card4",
"card5",
"card6","card7","card8","card9","card10","card11","card12","card13",
"card14","card15","card16","card17","card18","card19","card20",
"card21","card22","card23","card24","card25","card26",
"card27","card28","card29","card30","card31","card32","card33","card34",
"card35","card36","card37","card38","card39",
"card40","card41","card42","card43","card44",
"card45","card46","card47","card48","card49","card50","card51","card52")

rowname<-"row1"
colname<-c("rank","suit")

playingCard<-array("0",dim=c(1,2,52),dimnames=list(rowname,colname,cardnumber))
#Empty playing card array created.....

#empty list of deck of cards.. 
list_cards<-vector("list",52)

playans<-"Y"
facecards<-c("jack","queen","king","ace")

storeRank<-vector("character",10)
value<-c("2","3","4","5","6","7","8","9","10","jack","queen","king","ace")


#FUNCTIONS
deckInitialise<-function()
{


type<-c("spade","club","heart","diamond")
t<-1
v<-1

for (i in 1:52)
  {  
        playingCard[,1,i]<-value[v]
        playingCard[,2,i]<-type[t]
        
        if(i%%13==0)
         {v<-1
          t<-t+1
         }
        else 
         {v<-v+1}

   }
return (playingCard)
}
playingCard<-deckInitialise()

makelist_cards<-function()
{
 for (i in 1:52)
   {
       list_cards[[i]]<-playingCard[,,i]
   }
 return (list_cards)
}

list_cards<-makelist_cards()

while(playans=="Y"|playans=="y")
{ score<-0
  scoreComp<-0
  answer<-"HIT"
  k<-3                                 #for indexing the deck of shuffled cards
  flag<-0


shuffleCards<-function(list_cards)
{
  
  for (i in 1:51)
   {   randomcard<-sample(1:52,1)
        
        temp<-list_cards[i]
        list_cards[i]<-list_cards[randomcard]
        list_cards[randomcard]<-temp
   }

  return (list_cards)
}

list_cards<-shuffleCards(list_cards)                    #shuffled list of cards


cat("WELCOME!\n***LET'S PLAY GAME 21***\nShuffling Cards....\nShuffled Sucessfully\nYOUR TURN\n\n\n")

cat(list_cards[[1]][1],"of",list_cards[[1]][2],"\n")    #picking top two cards
storeRank[1]<-list_cards[[1]][1]
 if(!(list_cards[[1]][1] %in% facecards)){  
         score<-as.numeric(list_cards[[1]][1])
         cat("score =",score,"\n")
 }else{  
      switch(list_cards[[1]][1],
           jack={ score<-10 
                  cat("score =",score,"\n")
                  
                },
           queen={ score<-10 
                   cat("score =",score,"\n")
                   
                },
           king={ score<-10 
                   cat("score =",score,"\n")
                              
                },
           ace={ if ((score+11)>21) 
                     { score<-1}
                 else
                     { score<-11} 
                 cat("score =",score,"\n")
               })
      }
           
 cat(list_cards[[2]][1],"of",list_cards[[2]][2],"\n")
 storeRank[2]<-list_cards[[2]][1]
 
  if(!(list_cards[[2]][1] %in% facecards)){  
         score<-score+(as.numeric(list_cards[[2]][1]))
         cat("score =",score,"\n")
}else {
        switch(list_cards[[2]][1],
           jack={ score<-score+10 
                  cat("score =",score,"\n")
                  
                },
           queen={ score<-score+10 
                  cat("score =",score,"\n")
                   
                },
           king={ score<-score+10 
                   cat("score =",score,"\n")
                              
                },
           ace={ if ((score+11)>21) 
                     { score<-score+1}
                 else
                     { score<-score+11} 
                 cat("score =",score,"\n")
               })
      }

  if (score>21){
    cat("\nSORRY!,YOU LOST\n\n")
    break} 
  
# steps to pick next card....
  while(answer!="STAY"|answer!="stay")
  {
   #-----------------------------probability check....-------------------------
  favcount<-0                                
  diff<-(21-score)
  range<-(diff+1):13
  valueNew<-vector("character",length(range)) 
  
  if(diff>=10){
     cat("\nSECURE HIT\n") 
  }else if (diff==0|diff==1){
    { cat("\nProbability to HIT securely is nearly 0\n") }
  }else{ 
      favcount<-length(range)
      for (x in 1:favcount){ valueNew[x]<-value[diff+x-1]}
    
      favcount<-favcount*4
  
      for (j in 1:(k-1))
        { if(storeRank[j] %in% valueNew){favcount<-favcount-1}
        }
      totalcount<-52-(k-1)
      cat("\nProbability to Make HIT  ",(1-(favcount/totalcount)),"\n")
   }
 #-----------------prob checked!----------------------------------------------
  
  
  answer<-readline(prompt= "HIT or STAY ?  ")
  if(answer=="HIT"|answer=="hit"){  
      cat(list_cards[[k]][1],"of",list_cards[[k]][2],"\n")
      storeRank[k]<-list_cards[[k]][1]
      if(!(list_cards[[k]][1] %in% facecards)){
         score<-score+(as.numeric(list_cards[[k]][1]))
         cat("score =",score,"\n")
         
         if(score>21){ 
            flag<-1
            cat("SORRY, YOU LOST\n")
            break
             }
                  
      }else{
      
           switch(list_cards[[k]][1],
           jack={ score<-score+10 
                  cat("score =",score,"\n")
                  if(score>21){
                    flag<-1
                    cat("SORRY, YOU LOST\n")
                         break
                    }                       
                },
           queen={ score<-score+10 
                   cat("score =",score,"\n")
                   if(score>21){
                         flag<-1
                         cat("SORRY, YOU LOST\n")
                         break
                        }
                },
           king={ score<-score+10 
                   cat("score =",score,"\n")
                   if(score>21){
                         flag<-1
                         cat("SORRY, YOU LOST\n")
                         break
                        }                   
                },
           ace={ if ((score+11)>21){
                     score<-score+1
                 }else{
                      score<-score+11} 
                 cat("score =",score,"\n")
                 if(score>21){
                         flag<-1
                         cat("SORRY,YOU LOST\n")
                         break
                      }
                }  )

       }
                    
   }else if (answer=="STAY"|answer=="stay"){
    
     cat("\n\nCOMPUTER'S TURN\n")
     while(scoreComp<=17)
     {  
        cat(list_cards[[k]][1],"of",list_cards[[k]][2],"\n")
        if(!(list_cards[[k]][1] %in% facecards)){
        
          scoreComp<-scoreComp+(as.numeric(list_cards[[k]][1]))
          cat("Computer Score =",scoreComp,"\n")
          
        
        }else{
          switch(list_cards[[k]][1],
           jack={ scoreComp<-scoreComp+10 
                  cat("Computer Score =",scoreComp,"\n")
                  
                },

           queen={ scoreComp<-scoreComp+10 
                    cat("Computer Score =",scoreComp,"\n")
                    
                 },

           king={ scoreComp<-scoreComp+10 
                  cat("Computer Score =",scoreComp,"\n")
                  
                },

           ace={ if ((scoreComp+11)>21){ 
                      scoreComp<-scoreComp+1
                 }else{
                     scoreComp<-scoreComp+11
                      } 
                 cat("Computer Score =",scoreComp,"\n")
                 
                }  )
       }
     k<-k+1
   }
     if(scoreComp>21){
                         flag<-1
                         cat("\n\n***CONGRATS,YOU WON !!***\n")
                         break}
                        
 }
    k<-k+1
 }
     
     if(flag!=1){
     cat("\nYour Score",score,"\n")
     cat("\nComputer's Score",scoreComp,"\n")
     if(scoreComp>=score){
       cat("SORRY , You Lost!!\n")
     }else{
       cat("***CONGRAGULATIONS, You Won !!***\n")}}
      
   playans<-readline(prompt="\n\nDO YOU WANT TO PLAY AGAIN : y/n ??\n")
}

