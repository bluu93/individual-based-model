dir.create(file.path(getwd(),"outputs"))
setwd(file.path(getwd(),"outputs"))

library(gifski)

################################################################################
# Objects
################################################################################

# Define Bees
#-------------------------------------------------------------------------------
set.bees <- function(N.bees){
  bees <- vector(mode = "list",N.bees)
  
  for(b in seq(bees)){
    # Position
    bees[[b]]$x <- runif(1,-50,50) # Initial
    bees[[b]]$y <- runif(1,-50,50) # Initial
    bees[[b]]$x.rec <- bees[[b]]$x # Record x
    bees[[b]]$y.rec <- bees[[b]]$y # Record y
    
    # Attributes
    bees[[b]]$status <- "Alive"
    bees[[b]]$action.state <- "Fly"
    bees[[b]]$health <- 1
    bees[[b]]$detect.rad <- 10 # Detection radius
    bees[[b]]$harvest.time <- 0
    bees[[b]]$flower.indices <- NULL
    
    bees[[b]]$step.length.rec <- NULL
    bees[[b]]$action.state.rec <- NULL
    bees[[b]]$is.hornet.nearby <- FALSE
    
  }
  
  return(bees)
}

# Define Hornets
#-------------------------------------------------------------------------------
set.hornets <- function(N.hornets){
  hornets <- vector(mode = "list",N.hornets)
  
  for(h in seq(hornets)){
    # Position
    hornets[[h]]$x <- runif(1,-100,100)
    hornets[[h]]$y <- runif(1,-100,100)
    hornets[[h]]$x.rec <- hornets[[h]]$x
    hornets[[h]]$y.rec <- hornets[[h]]$y
    
    # Attributes
    hornets[[h]]$status <- "Alive"
    hornets[[h]]$action.state <- "Fly"
    hornets[[h]]$health <- 15
    hornets[[h]]$detect.rad <- 15
    hornets[[h]]$bee.indices <- NULL
    hornets[[h]]$is.threat <- FALSE
    hornets[[h]]$bees.nearby <- 0
    
  }
  
  return(hornets)
}

# Define Flowers
#-------------------------------------------------------------------------------
set.flowers <- function(N.flowers){
  flowers <- vector(mode = "list",N.flowers)
  
  for(f in seq(flowers)){
    # Position
    flowers[[f]]$x <- runif(1,-150,150) # Initial
    flowers[[f]]$y <- runif(1,-150,150) # Initial
    
    # Attributes
    flowers[[f]]$nectar <- 30
    
  }
  
  return(flowers)
}

################################################################################
# Actions
################################################################################

# Define action states
#-------------------------------------------------------------------------------
action.state <- function(object,what.state){
  
  # Default state for bees and hornets
  if(what.state == "Fly"){
    
    object$action.state <- "Fly"
    object$action.state.rec <- append(object$action.state.rec,"Fly")
    
    turning.angle <- runif(1,0,2*pi)
    step.length <- rgamma(1,shape = 2, scale = 2)
    object$step.length.rec <- append(object$step.length.rec,step.length)
    
    object$x <- (object$x + step.length*sin(turning.angle))
    object$y <- (object$y + step.length*cos(turning.angle))
    
    object$x.rec <- append(object$x.rec,object$x)
    object$y.rec <- append(object$y.rec,object$y)
    
  }
  
  # If bee has found nearby flower, harvest it
  else if(what.state == "Harvest"){
    
    object$action.state <- "Harvest"
    object$action.state.rec <- append(object$action.state.rec,"Harvest")
    
    object$x <- object$flower.chosen.x + runif(1,-2,2)
    object$y <- object$flower.chosen.y + runif(1,-2,2)
    
  }
  
  # If a hornet has latched onto a bee, the other bees will swarm
  # hornet.closest.index is found from calling bee.swarm() beforehand
  else if(what.state == "Swarm"){
    
    object$action.state <- "Swarm"
    object$action.state.rec <- append(object$action.state.rec,"Swarm")
    
    slope <- (object$y - hornets[[object$hornet.closest.index]]$y)/(object$x - hornets[[object$hornet.closest.index]]$x)
    intercept <- object$y - (slope*object$x)
    step.length <- rgamma(1,shape = 2,scale = 4)
    
    object$x <- ifelse(object$x >= hornets[[object$hornet.closest.index]]$x,
                       object$x - step.length,
                       object$x + step.length)
    
    # If slope is really big (check by looking at angle between line and x-axis)
    object$y <- ifelse((atan(slope)*(180/pi)) |> abs() >= 75, # 75 is arbitrary
                       # Change y-value directly (Add/subtract depending on y-value relative to hornet)
                       ifelse(object$y >= hornets[[object$hornet.closest.index]]$y,
                              object$y - step.length,
                              object$y + step.length),
                       # Randomize based on slope eq
                       rnorm(1,(slope*object$x) + intercept,1))
    
    object$x.rec <- append(object$x.rec,object$x) # record x
    object$y.rec <- append(object$y.rec,object$y) # record y
    
    dis <- sqrt((object$x - hornets[[object$hornet.closest.index]]$x)^2 + (object$y - hornets[[object$hornet.closest.index]]$y)^2)
    
    if(dis <= object$detect.rad)
      object$is.hornet.nearby <- TRUE
    
  }
  
  # If the bee is within distance of hornet, surround it
  else if(what.state == "Surround"){
    
    object$action.state <- "Surround"
    object$action.state.rec <- append(object$action.state.rec,"Surround")
    
    object$x <- hornets[[object$hornet.closest.index]]$x + runif(1,-2,2)
    object$y <- hornets[[object$hornet.closest.index]]$y + runif(1,-2,2)
    
    object$x.rec <- append(object$x.rec,object$x)
    object$y.rec <- append(object$y.rec,object$y)
    
  }
  
  # If hornet has found nearby bee, latch onto it
  else if(what.state == "Latch"){
    
    object$action.state <- "Latch"
    object$action.state.rec <- append(object$action.state.rec,"Latch")
    
    object$x <- object$bee.chosen.x + runif(1,-2,2)
    object$y <- object$bee.chosen.y + runif(1,-2,2)
    
  }
  
  # If the bee is caught by a hornet, caught
  # If the hornet is caught by bees, caught
  else if(what.state == "Caught"){
    
    object$action.state <- "Caught"
    
    object$action.state.rec <- append(object$action.state.rec,"Caught")
    object$x <- object$x
    object$y <- object$y
    
    object$x.rec <- append(object$x.rec,object$x)
    object$y.rec <- append(object$y.rec,object$y)
    
  }
  
  #Stay represents death of bee/hornet
  else if(what.state == "Stay"){
    
    object$action.state <- "Stay"
    
    object$action.state.rec <- append(object$action.state.rec,"Stay")
    object$x <- object$x
    object$y <- object$y
    
    object$x.rec <- append(object$x.rec,object$x)
    object$y.rec <- append(object$y.rec,object$y)
    
  }
  
  return(object)
}

# Define flower-finding
#-------------------------------------------------------------------------------
find.flowers <- function(object){
  
  object$flower.indices <- NULL # Resets for bees returning to flight after harvesting
  
  for(f in seq(flowers)){
    if(flowers[[f]]$nectar > 0){
      dis <- sqrt((object$x - flowers[[f]]$x)^2 + (object$y - flowers[[f]]$y)^2)
      if(dis <= object$detect.rad)
        object$flower.indices <- append(object$flower.indices,f)
    }
  }
  
  return(object)
}

# Define bee-hunting
#-------------------------------------------------------------------------------
hunt.bees <- function(object){
  
  object$bee.indices <- NULL
  
  for(b in seq(bees)){
    if(bees[[b]]$status == "Alive"){
      dis <- sqrt((object$x - bees[[b]]$x)^2 + (object$y - bees[[b]]$y)^2)
      if(dis <= object$detect.rad)
        object$bee.indices <- append(object$bee.indices,b)
    }
  }
  
  return(object)
}

# Define bee-swarming
#-------------------------------------------------------------------------------
bee.swarm <- function(object){
  
  hornet.closest <- list()
  for(h in seq(hornets)){
    if(hornets[[h]]$is.threat & hornets[[h]]$status == "Alive"){
      dis <- sqrt((object$x - hornets[[h]]$x)^2 + (object$y - hornets[[h]]$y)^2)
      hornet.closest[[as.character(h)]] <- dis
    }
  }
  
  # This returns the closest hornet
  object$hornet.closest.index <- hornet.closest |> unlist() |> order() |> 
    (\(x) x[1])() |> 
    (\(x) hornet.closest[x])() |> 
    names() |> as.numeric()
  
  return(object)
}


################################################################################
# Main
################################################################################

N.bees <- 20
N.flowers <- 30
N.hornets <- 8

bees <- set.bees(N.bees)
flowers <- set.flowers(N.flowers)
hornets <- set.hornets(N.hornets)
time.step <- 400

#-------------------------------------------------------------------------------

for(t in 1:time.step){
  
  xrec.bees <- sapply(bees,function(a) a$x)
  yrec.bees <- sapply(bees,function(a) a$y)
  
  xrec.flowers <- sapply(flowers,function(a) a$x)
  yrec.flowers <- sapply(flowers,function(a) a$y)
  flowers.nectar <- sapply(flowers,function(a) a$nectar)
  
  xrec.hornets <- sapply(hornets,function(a) a$x)
  yrec.hornets <- sapply(hornets,function(a) a$y)
  
  bees.status.state <- sapply(bees,function(a) a$status)
  hornets.status.state <- sapply(hornets,function(a) a$status)
  
  
  #begin save for gif later
  png(paste0(t,".png"), width = 2000, height = 1500, res = 200)
  
  bee.colors <- ifelse(bees.status.state == "Alive","gold1","grey9") #dead is grey9, alive is gold3
  bee.shapes <- ifelse(bees.status.state == "Alive",16,1)
  plot(xrec.bees,yrec.bees,col = bee.colors,pch = bee.shapes,xlim = c(-300,300),ylim = c(-300,300),xlab = "X",ylab = "Y")
  
  #plotting flowers
  flower.colors <- ifelse(flowers.nectar > 0,"green1","darkgreen")
  flower.shapes <- ifelse(flowers.nectar > 0,8,4)
  points(xrec.flowers,yrec.flowers,col = flower.colors,pch = flower.shapes)
  
  #plotting hornets
  hornet.colors <- ifelse(hornets.status.state == "Alive","maroon4","grey9") #dead is grey9, alive is maroon4
  hornet.shapes <- ifelse(hornets.status.state == "Alive",17,2)
  points(xrec.hornets,yrec.hornets,col = hornet.colors,pch = hornet.shapes)
  
  dev.off() #closes PNG
  
  
  
  for(b in seq(bees)){
    
    # Initial check if bee is dead
    if(bees[[b]]$health < 1){
      bees[[b]]$status <- "Dead"
      bees[[b]] <- action.state(bees[[b]],"Stay")
    }
    
    # Swarm to closest hornet until nearby and then surround it if hornet is a threat
    else if((bees[[b]]$action.state == "Fly" | bees[[b]]$action.state == "Harvest") & 
            hornets |> sapply(function(a) a$is.threat) |> grepl(pattern = TRUE) |> any()){
      bees[[b]] <- bee.swarm(bees[[b]])
      bees[[b]] <- action.state(bees[[b]],"Swarm")
    }
    
    # Other states
      
    # If bee is caught, it is losing health
    else if(bees[[b]]$action.state == "Caught")
      bees[[b]]$health <- bees[[b]]$health - 1
    
    # If bees are surrounding hornet, check if the hornet is dead -> reset bees' statuses
    # If hornet is still alive, bees are still "Surround"
    else if(bees[[b]]$action.state == "Surround"){
      
      if(hornets[[bees[[b]]$hornet.closest.index]]$health < 1){
        bees[[b]]$is.hornet.nearby <- FALSE
        bees[[b]]$hornet.closest.index <- NULL
        bees[[b]] <- action.state(bees[[b]],"Fly")
      }
      else{
        bees[[b]] <- action.state(bees[[b]],"Surround")
        hornets[[bees[[b]]$hornet.closest.index]]$bees.nearby <- hornets[[bees[[b]]$hornet.closest.index]]$bees.nearby + 1
      }
        
        
    }
    
    # If bees are swarming and the hornet is still latched, continue swarming
    # There may be a case where the bees are swarming but the hornet is no longer latched
    # Return the bees to fly
    else if(bees[[b]]$action.state == "Swarm"){
      if(bees[[b]]$is.hornet.nearby){
        bees[[b]] <- action.state(bees[[b]],"Surround")
        hornets[[bees[[b]]$hornet.closest.index]]$bees.nearby <- hornets[[bees[[b]]$hornet.closest.index]]$bees.nearby + 1
        hornets[[bees[[b]]$hornet.closest.index]] <- action.state(hornets[[bees[[b]]$hornet.closest.index]],"Caught")
      }
      
      else if(!bees[[b]]$is.hornet.nearby){
        bees[[b]] <- action.state(bees[[b]],"Swarm")
      }
      
      if(hornets |> sapply(function(a) a$is.threat) |> grepl(pattern = FALSE) |> all())
        bees[[b]] <- action.state(bees[[b]],"Fly")
      
    }
    
    # If bees are harvesting, check if flowers are empty or not
    else if(bees[[b]]$action.state == "Harvest"){
      
      if(flowers[[bees[[b]]$flower.chosen.index]]$nectar < 1)
        bees[[b]] <- action.state(bees[[b]],"Fly")
      
      else{
        bees[[b]] <- action.state(bees[[b]],"Harvest")
        bees[[b]]$harvest.time <- bees[[b]]$harvest.time + 1
        flowers[[bees[[b]]$flower.chosen.index]]$nectar <- flowers[[bees[[b]]$flower.chosen.index]]$nectar - 1
      }
    }
    
    # Bees are flying to find flowers
    else if(bees[[b]]$action.state == "Fly"){
      
      bees[[b]] <- find.flowers(bees[[b]])
      
      if(!is.null(bees[[b]]$flower.indices)){
        bees[[b]]$flower.chosen.index <- bees[[b]]$flower.indices[sample(length(bees[[b]]$flower.indices),1)]
        bees[[b]]$flower.chosen.x <- flowers[[bees[[b]]$flower.chosen.index]]$x
        bees[[b]]$flower.chosen.y <- flowers[[bees[[b]]$flower.chosen.index]]$y
        bees[[b]] <- action.state(bees[[b]],"Harvest")
      }
      
      else if(is.null(bees[[b]]$flower.indices))
        bees[[b]] <- action.state(bees[[b]],"Fly")
    }
    
  } # Bee loop end
  
  
  for(h in seq(hornets)){
    
    # Initial check if hornet is dead
    if(hornets[[h]]$health < 1){
      hornets[[h]]$status <- "Dead"
      hornets[[h]]$is.threat <- FALSE
      hornets[[h]] <- action.state(hornets[[h]],"Stay")
      # If the hornet dies but the bee is still alive, the bee goes back to flying
      # Set to fly but do not call function
      if(!is.null(hornets[[h]]$bee.chosen.index))
        bees[[hornets[[h]]$bee.chosen.index]]$action.state <- "Fly"
    }
    
      
    # Probability of hornet losing health will be based on number of bees around it
    # Probability increases by 0.20 for every bee
    else if(hornets[[h]]$action.state == "Caught"){
      losehealthprob <- ifelse(0.20*hornets[[h]]$bees.nearby >= 1,
                         1,
                         0.20*hornets[[h]]$bees.nearby)
      if(sample(c(TRUE,FALSE),size = 1,replace = TRUE,prob = c(losehealthprob,1-losehealthprob)))
         hornets[[h]]$health <- hornets[[h]]$health - 1
    }
    
    else if(hornets[[h]]$action.state == "Latch"){
      if(bees[[hornets[[h]]$bee.chosen.index]]$health < 1){
        hornets[[h]] <- action.state(hornets[[h]],"Fly")
        hornets[[h]]$is.threat <- TRUE
      }
      else
        bees[[hornets[[h]]$bee.chosen.index]] <- action.state(bees[[hornets[[h]]$bee.chosen.index]],"Caught")
    }
    
    else if(hornets[[h]]$action.state == "Fly"){
      hornets[[h]] <- hunt.bees(hornets[[h]])
      if(!is.null(hornets[[h]]$bee.indices)){
        hornets[[h]]$bee.chosen.index <- hornets[[h]]$bee.indices[sample(length(hornets[[h]]$bee.indices),1)]
        hornets[[h]]$bee.chosen.x <- bees[[hornets[[h]]$bee.chosen.index]]$x
        hornets[[h]]$bee.chosen.y <- bees[[hornets[[h]]$bee.chosen.index]]$y
        hornets[[h]] <- action.state(hornets[[h]],"Latch")
      }
      else if(is.null(hornets[[h]]$bee.indices))
        hornets[[h]] <- action.state(hornets[[h]],"Fly")
    }
  
  hornets[[h]]$bees.nearby <- 0
    
  } # hornet loop end
  
} #main loop end


################################################################################
# Save to file
################################################################################

# Get list of PNG files
a <- list.files(path = getwd(), pattern = "*.png", full.names = FALSE)

# Sort the files for gifski
a <- a |> 
  gsub(pattern = ".png", replacement = "") |> # remove .png to get numbers
  as.numeric() |> # convert to numeric
  sort() |> # sort as numeric
  as.character() |> # convert back to character
  paste0(".png") # add .png to labels

savefilename <- paste(
  "ibm",
  paste0(N.bees,"b"),
  paste0(N.flowers,"f"),
  paste0(N.hornets,"h"),
  paste0(time.step,"t"),
  sep = "_"
)

gifski(a,
       gif_file = paste0(savefilename,".gif"),
       width=2000,height=1500,delay=0.1)


# Clear .png files
file.remove(a)