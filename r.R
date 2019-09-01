####Reprex Data

rm(list = ls())

datesEg <- seq(as.Date("2014-09-04"), by = "day", length.out = 6)
#DDsub <-DDdf[c(1:15),c(2:6)]
#DUsub <-DUdf[c(1:15),c(2:6)]

DDsub <-  data.frame("v1" = c(0, -0.0012, -0.1612, -0.1953, -0.2432, -0.1122),
                     "v2" = c(0, 0, 0, 0, 0, 0),
                     "v3" = c(0, -0.0021, -0.1233, -0.1242, -0.0043, -0.0033),
                     "v4" = c(0, -0.0031, -0.3094, -0.1023, -0.0984, -0.01235),
                     "v5" = c(0, NA, NA, NA, NA, NA))

DUsub <-  abs(DDsub)
reprexDD <- data.frame(datesEg, DDsub)  
reprexDU <- data.frame(datesEg, DUsub) 

# Your code below doesn't correspond to the objects you created.
# But I get the question so I will work with some assumptions:
# Because you only have 6 days of data, I will say you need to ignore dates that occur in the following two days and the trigger value is 10%.
# This means means our dates should be: 
# v1: 2014-09-06, 2014-09-09
# v2: blank
# v3: 2014-09-06
# v4: 2014-09-06
# v5: blank

# Let's start by setting parameters:

trigger <- 0.1
window <- 2

# Now we need to go top to bottom and start by getting our first value below the trigger.
# We will be using the relatively uncommon "while" loop
# Be careful with while loops because if you make a mistake they can run forever.
# Our first loop will just provide the positions.

solution <- vector(mode = "integer", length = 1)

l = length(reprexDD$v1)
s = 0
pos = 1
while(s<=l){
  solution[pos] = which(reprexDD$v1[(s+1):l] <= -trigger)[1]+s
  s = solution[pos] + window
  pos = pos + 1
}

# Now we can return the dates of those positions:

datesEg[solution]

# Finally we can remove the naming of the variables and get the loop to run on each column.

trigger_index <- lapply(reprexDD[-1], function(i){
  solution <- vector(mode = "integer", length = 1)
  l = length(i)
  s = 0
  pos = 1
  while(s <= l){
    x = which(i[(s+1):l] <= -trigger)[1]+s
    if(is.na(x)){
      break
    } else{
      solution[pos] = x
    }
    s = solution[pos] + window
    pos = pos + 1
  }
  return(solution)
})


# This looks good and it works the way we would expect.
# We can now simply pass the index values for each variable through the date vector:

lapply(trigger_index, function(z){
  datesEg[z]
})

# Or use a for loop or whatever makes more sense to you.

# You can then drop any vectors that have a length of zero?