# STOCHASTIC DYNAMIC MODEL: A SMALL BIRD IN WINTER
# This code is based on Chapter 5 of "Theoetical Modelling for field biologists and 
# other interesting people" by Hannah Kokko 


FORAGE <- function(dmin, dmax, c, f, maxt, maxc){
  # dmin = probability of death at unit time t if the bird is very lean
  # dmax = probability of death at unit time t if the bird is very heavy
  # c = rate of resource consumption
  # f = feeding efficiency 
  # maxt = maximum time - the number of time units that divide the day
  # maxc = maximum condition - the number of different condition units 
  
  # The probability of death increases linearly with body weight 
  # Note: d is a vector that will need to be subset by condition 
  d <-  c(0, seq(dmin, dmax, length.out = maxc)) 
  
  # A bird that is alive can improve or not... 
  P_eat_up <- (1-d)*f
  P_eat_same <- (1-d)*(1-f)   
  P_eat_dead <- d
  P_rest_same <- 1-c 
  P_rest_down <- c  
  
  # Except those who already are in top condition, they cannot improve so they get different values
  Ptop_eat_same <- 1-d[maxc+1]
  Ptop_eat_dead <- d[maxc+1]
  
  # Create matrices for rewards if action is forage and when action is rest
  reward <- matrix(0, nrow=maxc+1, ncol=maxt+1)
  reward[,maxt+1] <- c(0:maxc)   #terminal rewards
  rewardForage <- matrix(0, nrow=maxc+1, ncol=maxt)
  rewardRest <- matrix(0, nrow=maxc+1, ncol=maxt)
  forageRule <- matrix(0, nrow=maxc+1, ncol=maxt)

  
  # Determine fitness for each action. We already know the terminal rewards at t=maxt+1)
  # so we start at t=maxt and work backwards. 
  for (t in maxt:1) { # loop backwards through time 
    for (s in 2:maxc) { # loop through all states exept top and death, they are treated differently
      rewardForage[s, t] <- P_eat_same[s]*reward[s, t+1] + 
        P_eat_up[s]*reward[s+1, t+1] + 
        P_eat_dead[s]*0
      rewardRest[s,t] <- P_rest_same*reward[s, t+1] +
        P_rest_down*reward[s-1, t+1]
    } # end state loop
    
    # dead birds (index=1) don't get any rewards
    rewardForage[1,t] <- 0  #death
    rewardRest[1,t] <- 0  #also death 
    
    # top birds (index=maxc+1) can't improve their condition
    rewardForage[maxc+1, t] <- Ptop_eat_same*reward[maxc+1, t+1] + 
      Ptop_eat_dead*0              
    rewardRest[maxc+1, t] <- P_rest_same*reward[maxc+1, t+1] + 
      P_rest_down*reward[maxc, t+1]
    

    # We know the rewards for each action at each time step, now we want to compare
    # which one yields the highest rewards where 1=forage and 0=rest 
    forageRule[,t] <- ifelse(rewardForage[,t] > rewardRest[,t], 1, 0)
    
    # Update the reward function
    reward[,t]=ifelse(forageRule[,t] == 1, rewardForage[,t], rewardRest[,t])
    
  } # end time loop
  
  print(forageRule) 
  
} # end function 


# Did it work? 
# Test with some of the examples from Chpt 5

FORAGE(0, 0, 0.4, 0.8, 5, 6)

FORAGE(0.0, 0.01, 0.4, 0.8, 5, 6)

#Yay! 






