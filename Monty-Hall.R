Monty_Hall <- function(change_mind = TRUE, N = 3){
  
  # set up doors --------------------------
  doors <- rep(0,N)
  
  avaliable_doors <- 1:N
  
  # put the car in a door -----------------
  car_door <- sample(avaliable_doors, 1)
  doors[car_door] <- 1 
  
  # player choice -------------------------
  choice <- sample(avaliable_doors, 1)
  
  # remove doors without the car ----------
  if (choice != car_door){
    avaliable_doors <- avaliable_doors[-avaliable_doors[-c(choice, car_door)]]
  }else if (choice == car_door) {
    avaliable_doors <- c(avaliable_doors[choice], sample(avaliable_doors[-choice],1))
  }
  
  # player's second chance ----------------
  if (change_mind == TRUE) choice  <- avaliable_doors[avaliable_doors != choice]
  
  # check if player won -------------------
  if (doors[choice] == 1) return("Win!")
  
}

Monte_Carlo <- function(games = 1000, ...){
  
  result <- c()
  
  for(i in 1:games) result <- c(result, Monty_Hall(...))
  
  return(100*length(result)/games)
}

# Convergance

convergance <- c()
for(j in seq.int(1,10000,length.out = 100)) convergance <- c(convergance, Monte_Carlo(games = j))
plot(seq.int(1,10000,length.out = 100),convergance, type = 'b', pch = 16, ylab = "% Games Won"); abline(h = 100*2/3, col = 'red')

# Original Three Door Game

Monte_Carlo(games = 1000, change_mind = TRUE)
Monte_Carlo(games = 1000, change_mind = FALSE)

# A Hundred Door Game

Monte_Carlo(games = 1000, change_mind = TRUE, N = 100)
Monte_Carlo(games = 1000, change_mind = FALSE, N = 100)

