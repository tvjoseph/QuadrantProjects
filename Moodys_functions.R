# JMJPFU
# 24-Nov-2016

# Functions for Moodys

######################################################################

# Function 1 : Function to create the aggregated variables for each pool

mood_agg <- function(mood_rec){
  
  Pool_agg <- data.frame(matrix(nrow=0,ncol=3)) # Creating the empty data frame
  names(Pool_agg) <- c("SecondTier","Variable","FirstTier")
  
  # First let us find the aggregated values
  prin_tot <- sum(mood_rec$PRINCIPALBALANCE)
  
  
  # Variable 1 : State Aggregation
  
  state_agg <- mood_rec %>% group_by(STATE) %>% summarise(Variable = ((sum(PRINCIPALBALANCE))/prin_tot)*100)
  
  names(state_agg) <- c("SecondTier","Variable")
  
  state_agg$FirstTier <- "State"
  
  Pool_agg <- rbind(Pool_agg,state_agg)
  
  # Variable 2 : Make Aggregation
  
  make_agg <- mood_rec %>% group_by(MAKE) %>% summarise(Variable = ((sum(PRINCIPALBALANCE))/prin_tot)*100)
  
  names(make_agg) <- c("SecondTier","Variable")
  
  make_agg$FirstTier <- "Make"
  
  Pool_agg <- rbind(Pool_agg,make_agg)
  
  # Variable 3 : APR Aggregartion : This will have to be changed based on confirmation from Moodys
  
  APR_agg <- mood_rec %>% summarise(Variable = sum(PRINCIPALBALANCE*APR)/sum(PRINCIPALBALANCE))
  
  APR_agg$SecondTier <- "APR"
  APR_agg$FirstTier <- "APR"
  
  APR_agg <- APR_agg %>% select(SecondTier,Variable,FirstTier)
  
  Pool_agg <- rbind(Pool_agg,APR_agg)
  
  # Variable 4 : Days Past Due Aggregation
  #DPD_agg <-  mood_rec %>% filter(PRINCIPALBALANCE > 0) %>% summarise(Variable = mean(DAYSPASTDUE))
  DPD_agg <-  mood_rec %>% filter(PRINCIPALBALANCE > 0) %>% select(DAYSPASTDUE)%>% mutate(DPD = (31-DAYSPASTDUE)/length(DAYSPASTDUE)) %>% summarise(Variable = sum(DPD))
  DPD_agg$SecondTier <- "DPD"
  DPD_agg$FirstTier <- "DPD"
  
  DPD_agg <- DPD_agg %>% select(SecondTier,Variable,FirstTier)
  
  Pool_agg <- rbind(Pool_agg,DPD_agg)
  
  # Variable 5 : NewuseIndicator
  
  NUI <- mood_rec %>% summarise(Variable = sum(NEWUSEDINDICATOR)/nrow(mood_rec))
  NUIO <- data.frame(1-NUI$Variable)
  names(NUIO) <- "Variable"
  
  NUI$SecondTier <- "New"
  NUIO$SecondTier <- "Used"
  
  NUI$FirstTier <- NUIO$FirstTier <- "New_Use"
  
  NUI <- NUI %>% select(SecondTier,Variable,FirstTier)
  NUIO <- NUIO %>% select(SecondTier,Variable,FirstTier)
  
  Pool_agg <- rbind(Pool_agg,NUI,NUIO)
  
  # Variable 6 : Credit Score
  
  mood_rec[mood_rec$ORIGCREDITSCORE == "NULL",] <- NA # If there are null values convert that into NA
  
  mood_cred <- mood_rec %>% select(ORIGCREDITSCORE,PRINCIPALBALANCE) %>% filter(!is.na(ORIGCREDITSCORE)) # Taking only the non NA values
  
  Cred_agg <-  mood_cred %>% summarise(Variable = sum(PRINCIPALBALANCE*as.numeric(ORIGCREDITSCORE))/sum(PRINCIPALBALANCE))
  
  Cred_agg$SecondTier <- "Creditscore"
  Cred_agg$FirstTier <- "Creditscore"
  Cred_agg <- Cred_agg %>% select(SecondTier,Variable,FirstTier)
  
  Pool_agg <- rbind(Pool_agg,Cred_agg)
  
  Pool_agg$Variable <- round(Pool_agg$Variable,digits = 3)
  
  Pool_agg
  
}

#####################################################
# JMJPFU
# 28-Nov-2016

# Function to calculate the deviance from goals

goal_deviance <- function(pool,stwt,mkwt,aprwt,nwt,uwt,crwt){ # Input the pool df
  
  dev = 0 # Initializing the deviance to 0
  
  # Find Deviance of State
  
  st_rec <- pool %>% filter(FirstTier == "State") # Take the record of only the state data
  
  for(i in 1:nrow(st_rec)){
    
    if(st_rec$Variable[i] > 20){ dev = dev + (stwt*(st_rec$Variable[i] - 20))} # Weighted deviation of state
    
  }
  
  # Find Deviance of Make
  
  mk_rec <- pool %>% filter(FirstTier == "Make") # Take the record of only the Make data
  
  for(i in 1:nrow(mk_rec)){
    
    if(mk_rec$Variable[i] > 20){ dev = dev + (mkwt*(mk_rec$Variable[i] - 20))} # Weighted deviation of make
    
  } # End of the MK_rec for loop
  
  # Find Deviance of APR
  
  apr_rec <- pool %>% filter(FirstTier == "APR") # Take the record of only the APR data
  
    
    if(apr_rec$Variable < 0.08){ dev = dev + (aprwt*(0.08 - apr_rec$Variable))} # Weighted deviation of APR
    else if(apr_rec$Variable > 0.12){ dev = dev + (aprwt*(apr_rec$Variable - 0.12))}
    
  # Find Deviance of New
  
  new_rec <- pool %>% filter(SecondTier == "New") # Take the record of only the New data
  
  
  if(new_rec$Variable > 0.61){ dev = dev + (nwt*(new_rec$Variable - 0.61))} # Weighted deviation of New assets
  
  # Find Deviance of Used
  
  used_rec <- pool %>% filter(SecondTier == "Used") # Take the record of only the New data
  
  
  if(used_rec$Variable > 0.42){ dev = dev + (uwt*(used_rec$Variable - 0.42))} # Weighted deviation of Used assets
  
  # Find Deviance of Credit Score
  
  cred_rec <- pool %>% filter(FirstTier == "Creditscore") # Take the record of only the APR data
  
  
  if(cred_rec$Variable < 400){ dev = dev + (crwt*(400 - cred_rec$Variable))} # Weighted deviation of APR
  else if(cred_rec$Variable > 800){ dev = dev + (crwt*(cred_rec$Variable - 800))}
  
  # Finally returning the total deviation 
  
  dev  # Return deviation
  
}

#################################################################################################
# JMJPFU
# 30-Nov-2016

# This is a function to calculate deviance score contribution of individual loan

ind_deviance <- function(pooldf,poolagg,stwt,mkwt,aprwt,nwt,uwt,crwt,dpwt){ 
  # pooldf : This is the data frame of all individual loans in the pool that has to be rated.
  # poolagg : This is the dataframe for the aggregated score of the pool
  
  pooldf$indscore <- NA # Introducing a new column
  
  pooldf$ORIGCREDITSCORE[pooldf$ORIGCREDITSCORE== "NULL" ] <- NA # Changing the NULL values to NA
  
  pooldf$ORIGCREDITSCORE <- as.numeric(pooldf$ORIGCREDITSCORE) # Converting original credit score to numeric
  
  for(i in 1:nrow(pooldf)){
    
    
    dev = 0 # Initialising the deviation score for each loan
    loan <- pooldf[i,] # Take the first loan
    
    # First check for the state of the loan
    
    ln_state <- pooldf$STATE[i] # Pick the state of the individual loan
    
    state_agg <- as.numeric(paste(poolagg %>% filter(SecondTier == ln_state) %>% select(Variable))) # Find the score of state
    
    
      
      state_no <- nrow(pooldf %>% filter(STATE == ln_state)) # Finding the number of records who belong to this state
      
      dev = dev + (stwt*(20 - state_agg))/state_no # Finding the proportion of score related to the state
    
    
    # Second check for the make of the loan
    
    ln_make <- pooldf$MAKE[i] # Pick the Make of the individual loan
    
    make_agg <- as.numeric(paste(poolagg %>% filter(SecondTier == ln_make) %>% select(Variable))) # Find the score of Make
    
    
      
      make_no <- nrow(pooldf %>% filter(MAKE == ln_make)) # Finding the number of records who belong to this Make
      
      dev = dev + (mkwt*(20 - make_agg))/make_no # Finding the proportion of score related to the Make
      
     # Third Check for the DPD of the loan
      
      ln_dpd <- pooldf$DAYSPASTDUE[i] # Pick the days past due of the individual loan
      ln_PRCON <- pooldf$PRINCIPALBALANCE[i] # Take the Principal contribution so as to compare the cases that has to be taken
      ln_nrow <- nrow(pooldf %>% filter(PRINCIPALBALANCE > 0)) # Take the number of rows where Principal balance > 0 for taking average
      
      
      ln_ID <- pooldf$ID[i]
      
      if(ln_PRCON > 0){
        
        dev = dev + (((31- ln_dpd)* dpwt)/ln_nrow) # update the deviance with the average contribution only if the Principal balance > 0
        
      }else { dev = dev + 0 }
      
    # FOurth check for APR
    
    ln_APR <- pooldf$APR[i]
    ln_ID <- pooldf$ID[i]
    
    if(ln_APR > .12){ 
      
      APR12_no <- pooldf %>% filter(APR > .12) %>% select(ID,APR,PRINCIPALBALANCE) %>% mutate(wtavg=APR*PRINCIPALBALANCE) # Create a DF with the relevant values
      
      wt_contri <- (.12 - (sum(APR12_no$wtavg)/sum(APR12_no$PRINCIPALBALANCE)))*aprwt # Total deviation contribution
      
      ln_contri <- APR12_no %>% filter(ID==ln_ID) %>% select(wtavg) # This is the contribution of the individual loan
      
      dev = dev + (ln_contri$wtavg/(sum(APR12_no$wtavg))) * wt_contri  # To find proportion of the deviation to be approtioned
      
        }else if(ln_APR <= .12 & ln_APR >= .08){
          
          APR_mid <- pooldf %>% filter(APR <= .12 & APR >= 0.08) %>% select(ID,APR,PRINCIPALBALANCE) %>% mutate(wtavg=APR*PRINCIPALBALANCE) # Create a DF with the relevant values
          
          wt_contri <- ((sum(APR_mid$wtavg)/sum(APR_mid$PRINCIPALBALANCE)) - .08)*aprwt # Total deviation contribution
          
          ln_contri <- APR_mid %>% filter(ID==ln_ID) %>% select(wtavg) # This is the contribution of the individual loan
          
          dev = dev + (ln_contri$wtavg/(sum(APR_mid$wtavg))) * wt_contri  # This is the contribution of the individual loan
        }else if(ln_APR < .08){
        
          APR_last <- pooldf %>% filter(APR <= .08) %>% select(ID,APR,PRINCIPALBALANCE) %>% mutate(wtavg=APR*PRINCIPALBALANCE) # Create a DF with the relevant values
          
          wt_contri <- ((sum(APR_last$wtavg)/sum(APR_last$PRINCIPALBALANCE)) - .08)*aprwt # Total deviation contribution
          
          ln_contri <- APR_last %>% filter(ID==ln_ID) %>% select(wtavg) # This is the contribution of the individual loan
          
          dev = dev + (ln_contri$wtavg/(sum(APR_last$wtavg))) * wt_contri  # This is the contribution of the individual loan
          
          
      } # End of the else condition
    
    
    # Fourth  check for New or old use
    
    ln_newuse <- pooldf$NEWORUSEDINDICATOR[i]
    
    if(ln_newuse == "New"){
      
      newno <- nrow(pooldf %>% filter(NEWORUSEDINDICATOR == "New")) # Finding the number of records with new
      new_rec <- poolagg %>% filter(SecondTier == "New") # Find the variable value
      
      dev = dev + ((nwt*(0.61 - new_rec$Variable))/newno) # Redistributing the deviance
    } # End of the New use case
    
    if(ln_newuse == "Used"){
      
      newno <- nrow(pooldf %>% filter(NEWORUSEDINDICATOR == "Used")) # Finding the number of records with new
      new_rec <- poolagg %>% filter(SecondTier == "Used") # Find the variable value
      
      dev = dev + ((uwt*(0.42 - new_rec$Variable))/newno) # Redistributing the error
      
      
    } # End of the used case
    
   # Fifth check - Looking at Credit score data
    
    ln_cred <- as.numeric(pooldf$ORIGCREDITSCORE[i])
    
    ln_ID <- pooldf$ID[i]
    
    
    if(!is.na(ln_cred)&ln_cred > 800){ 
      
      cred800_no <- pooldf %>% filter(ORIGCREDITSCORE > 800) %>% select(ID,ORIGCREDITSCORE,PRINCIPALBALANCE) %>% mutate(wtavg=ORIGCREDITSCORE*PRINCIPALBALANCE) # Create a DF with the relevant values
      
      if(nrow(cred800_no) > 0){
        
        wt_contri <- (800 - (sum(cred800_no$wtavg)/sum(cred800_no$PRINCIPALBALANCE)))*crwt # Total deviation contribution
        
        ln_contri <- cred800_no %>% filter(ID==ln_ID) %>% select(wtavg) # This is the contribution of the individual loan
        
        dev = dev + (ln_contri$wtavg/(sum(cred800_no$wtavg))) * wt_contri  # To find proportion of the deviation to be approtioned
        
        } # End of if condition to update the deviation score
      
    }else if(!is.na(ln_cred)&(ln_cred <= 800 & ln_cred >= 400)){
      
      cred_mid <- pooldf %>% filter(ORIGCREDITSCORE <= 800 || ORIGCREDITSCORE >= 400) %>% select(ID,ORIGCREDITSCORE,PRINCIPALBALANCE) %>% mutate(wtavg=ORIGCREDITSCORE*PRINCIPALBALANCE) # Create a DF with the relevant values
      
      if(nrow(cred_mid) > 0){
        
        wt_contri <- ((sum(cred_mid$wtavg)/sum(cred_mid$PRINCIPALBALANCE))-400)*crwt # Total deviation contribution
        
        ln_contri <- cred_mid %>% filter(ID==ln_ID) %>% select(wtavg) # This is the contribution of the individual loan
        
        dev = dev + (ln_contri$wtavg/(sum(cred_mid$wtavg))) * wt_contri  # To find proportion of the deviation to be approtioned
        
      } # End of if condition to update the deviation score
      
    }else if(!is.na(ln_cred) & ln_cred < 400) {
      
      cred_last <- pooldf %>% filter(ORIGCREDITSCORE < 400) %>% select(ID,ORIGCREDITSCORE,PRINCIPALBALANCE) %>% mutate(wtavg=ORIGCREDITSCORE*PRINCIPALBALANCE) # Create a DF with the relevant values
      
      if(nrow(cred_last) > 0){
        
        wt_contri <- ((sum(cred_last$wtavg)/sum(cred_last$PRINCIPALBALANCE))-400)*crwt # Total deviation contribution
        
        ln_contri <- cred_last %>% filter(ID==ln_ID) %>% select(wtavg) # This is the contribution of the individual loan
        
        dev = dev + (ln_contri$wtavg/(sum(cred_last$wtavg))) * wt_contri  # To find proportion of the deviation to be approtioned
        
      } # End of if condition to update the deviation score
      
      
    } # End of the else condition for Credit
    
    pooldf$indscore[i] <- dev
    
  } # Looping over the whole data frame
  
  pooldf # Return the pooldf
  
} # End of the function to calculate the individual deviance


#####################################################
# JMJPFU
# 1-Dec-2016

# Revised Function to calculate the deviance from goals

goal_UPDdeviance <- function(pool,stwt,mkwt,aprwt,nwt,uwt,crwt,dpwt){ # Input the pool df
  
  dev = 0 # Initializing the deviance to 0
  
  # Find Deviance of State
  
  st_rec <- pool %>% filter(FirstTier == "State") # Take the record of only the state data
  
  for(i in 1:nrow(st_rec)){
    
    dev = dev + (stwt*(20 - st_rec$Variable[i])) # Weighted deviation of state
    
  }
  
  # Find Deviance of Make
  
  mk_rec <- pool %>% filter(FirstTier == "Make") # Take the record of only the Make data
  
  for(i in 1:nrow(mk_rec)){
    
    dev = dev + (mkwt*(20 - mk_rec$Variable[i])) # Weighted deviation of make
    
  } # End of the MK_rec for loop
  
  # Find Deviance of Days Past Due
  
  # Deviance of DPD is just the same as the pool score
  
  dpd_rec <- pool %>% filter(FirstTier == "DPD")
  
  dev = dev + (dpwt* dpd_rec$Variable) # Getting the deviation score for DPD
  
  
  # Find Deviance of APR
  
  apr_rec <- pool %>% filter(FirstTier == "APR") # Take the record of only the APR data
  
  
  
  if(apr_rec$Variable > 0.12){ dev = dev + (aprwt*(0.12 - apr_rec$Variable))}
  else{dev = dev + (aprwt*(apr_rec$Variable - 0.08)) } # Weighted deviation of APR
  
  # Find Deviance of New
  
  new_rec <- pool %>% filter(SecondTier == "New") # Take the record of only the New data
  
  
  dev = dev + (nwt*(0.061 - new_rec$Variable)) # Weighted deviation of New assets
  
  # Find Deviance of Used
  
  used_rec <- pool %>% filter(SecondTier == "Used") # Take the record of only the New data
  
  
  dev = dev + (uwt*(0.42 - used_rec$Variable)) # Weighted deviation of Used assets
  
  # Find Deviance of Credit Score
  
  cred_rec <- pool %>% filter(FirstTier == "Creditscore") # Take the record of only the APR data
  
  
  if(cred_rec$Variable > 800){
  
   dev = dev + (crwt*(800- cred_rec$Variable))
   
   }else{ dev = dev + (crwt*(cred_rec$Variable - 400)) }
  
  
  
  
  # Finally returning the total deviation 
  
  dev  # Return deviation
  
  
 } # End of function

#####################################################################
# JMJPFU
# 2-Dec-2016

# Function for optimising the scores

Pool_optimiser <- function(unsold,sold,pools,stwt,mkwt,aprwt,nwt,uwt,crwt,dpwt){ # Can introduce a data frame with pool names and goals
  # Unsold is a dataframe with unsold loans
  # sold is a dataframe with sold loans
  # pools is a dataframe with the pools and its goals listed
  # All the other values are weights which are passed for the respective variable
  
  ### Step 1 : Create benchmark measures for  the existing pools
  
  for(i in 1:nrow(pools)){
    
    # Taking the records
    mood_rec <- sold %>% filter(POOLID == pools[i,1]) # Take the relevant records
    pools$Orgrow[i] <- nrow(mood_rec) # Store the nrow values in the second column
    # taking the pool data
    Pool <- mood_agg(mood_rec)
    pools$orgscore[i] <- goal_UPDdeviance(Pool,stwt,mkwt,aprwt,nwt,uwt,crwt,dpwt) # take a benchmark deviance 
    
  } # End of for loop for creating the benchmark score
  
  ## Step 2 : Running the optimiser algorithm  
  
  # 2.1 # Making another column in pools for storing the latest values
  
  pools$latest <- pools$orgscore
  
  # 2.2 # Running the optimiser algorithm
  
  for(j in 1:nrow(unsold)){ # First take all the unsold pool one by one
    
    ln_samp <- unsold[j,] # Take one loan at a time
    
    # loop through all pools to find the Optimum pool it should go into
    
    min_dev <- 0 # An initial value for the negative deviance cases 
    pos_dev <- 0 # An initial value for the positive deviance cases
    pool_sel <- NA # Initialise the pools for which the deviance have to be calculated
    
    for(k in 1:nrow(pools)){
      
      dev_dir <- pools[k,2] - pools[k,5] # To find the direction in which the score has to move with respect to the goal. If                                dev_dir is -ve, then better loans have to be put in so that the score goes down and viceversa
      
      mood_rec <- sold %>% filter(POOLID == pools[k,1]) # Take the first pool
      
      Pool <- mood_agg(mood_rec) # Finding the aggregated portfolio
      
      pools$latest[k] <- goal_UPDdeviance(Pool,stwt,mkwt,aprwt,nwt,uwt,crwt,dpwt) # Finding the current score of the pool
      
      # Now to check for any deviation
      
      new_rec <- rbind(mood_rec,ln_samp) # Adding the one loan into the existing pool to calculate an updated pool
      
      Pool1 <- mood_agg(new_rec) # Finding the aggregated metrics for the updated pool
      
      temp_dev <- goal_UPDdeviance(Pool1,stwt,mkwt,aprwt,nwt,uwt,crwt,dpwt) # Finding the new score for the updated pool
      
      # Let us look at the deviance
      
      diff <- temp_dev - pools$latest[k]
      
      # Finding the right pool to apply the deviance for score increases cases to 0
      
      if(diff > 0 & dev_dir > 0){
        
        if(min_dev < diff ){  # Second if loop to find the most optimum value
          
          min_dev <- diff # Update the min_dev with the latest difference
          
          pool_sel <- pools$PoolID[k] # Update the pool_sel with the latest pool name
          
        } # End of if loop to update the values
      } # End of the IF loop for the score reduction cases
      
      # Finding the right pool to apply the deviance for score decrease cases to 0
      
      if(diff < 0 & dev_dir < 0){
        
        if(pos_dev > diff ){  # Second if loop to find the most optimum value
          
          pos_dev <- diff # Update the pos_dev with the latest difference
          
          pool_sel <- pools$PoolID[k] # Update the pool_sel with the latest pool name
          
        } # End of if loop to update the values
      } # End of the IF loop for the score reduction cases
      
      
      
    } # End of for loop to run over all the pool values
    
    # Updating the loan into the pool only in case of any dip in deviance
    
    if(!is.na(pool_sel)){
      
      ln_samp$POOLID <- pool_sel # Updating the portfolio with the new poolid name
      sold <- rbind(sold,ln_samp) # Updating the sold portfolio
      sel <- paste(pool_sel)
      pools[pools$PoolID==sel,5] <- pools[pools$PoolID==sel,5] + min_dev # Updating the latest score with deviation
      
    } # End of the IF loop for updating the values
    
    
    
  } # End of the loop to run over all the unsold pool values
  
  output <- list(port = sold,finpool = pools)
  
  return(output) # Returning all the values
  
  
  
} # End of the function Pool_Optimiser

















########################## Un used Code

if(ln_APR > 0.12 || ln_APR < 0.08){
  
  APR_12 <- pooldf %>% filter(APR > .12) %>% select(ID,APR,PRINCIPALBALANCE) %>% mutate(wtavg = APR * PRINCIPALBALANCE)
  APR_8 <- pooldf %>% filter(APR < .08) %>% select(ID,APR,PRINCIPALBALANCE) %>% mutate(wtavg = APR * PRINCIPALBALANCE)
  
  APR_comb <- rbind(APR_12,APR_8) # Take a combined df of both records
  
  apr_rec <- poolagg %>% filter(FirstTier == "APR") # Take the record of only the APR data
  
  if(apr_rec$Variable < 0.08){ error = (aprwt*(0.08 - apr_rec$Variable))}else if(apr_rec$Variable > 0.12){ error = (aprwt*(apr_rec$Variable - 0.12))} # Weighted deviation of APR
  
  wt_sum = sum(APR_comb$wtavg) # Find the weighted average
  
  ln_sum = APR_comb$wtavg[APR_comb$ID==ln_ID] # Find the individual contribution
  
  dev = dev + error * (ln_sum/wt_sum)
  
} # End of IF condition for APR






############################

Cred_comb <- data.frame(matrix(nrow=0,ncol=4))

names(Cred_comb) <- c("ID","ORIGCREDITSCORE","PRINCIPALBALANCE","wtavg")

if(ln_cred > 800 || ln_cred < 400){
  
  Cred_800 <- pooldf %>% filter(ORIGCREDITSCORE > 800) %>% select(ID,ORIGCREDITSCORE,PRINCIPALBALANCE)
  
  if(nrow(Cred_800) > 0) { Cred_800$wtavg <- as.numeric(Cred_800$ORIGCREDITSCORE) * Cred_800$PRINCIPALBALANCE 
  Cred_comb <- rbind(Cred_comb,Cred_800) # combining the updated Cred_800 df
  
  }
  
  
  Cred_400 <- pooldf %>% filter(ORIGCREDITSCORE < 400) %>% select(ID,ORIGCREDITSCORE,PRINCIPALBALANCE)
  
  if(nrow(Cred_400) > 0) { Cred_400$wtavg <- as.numeric(Cred_400$ORIGCREDITSCORE) * Cred_400$PRINCIPALBALANCE
  
  Cred_comb <- rbind(Cred_comb,Cred_400) # combining the updated Cred_400 df
  
  }
  
  
  
  cred_rec <- poolagg %>% filter(FirstTier == "Creditscore") # Take the record of only the Credit data
  
  if(cred_rec$Variable < 400){ error = (crwt*(400 - cred_rec$Variable))}else if(cred_rec$Variable > 800){ error = (crwt*(cred_rec$Variable - 800))} # Weighted deviation of Credit
  
  wt_sum = sum(Cred_comb$wtavg) # Find the weighted average
  
  indi_rec <- nrow(Cred_comb %>% filter(ID == ln_ID)) # Checking if the ID is available in the combined df
  
  if(indi_rec > 0){ln_sum = Cred_comb$wtavg[Cred_comb$ID==ln_ID]}else{ln_sum <- 0} # Find the individual contribution
  
  dev = dev + error * (ln_sum/wt_sum)
  
} # End of IF condition for Credit




###########################################################################################################
