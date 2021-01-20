# Load Packagaes ----------------------------------------------------------
## Packages for data organisation and plotting
library(tidyverse)
library(knitr)
library(data.table)
# Package for relative file paths
library(here)
# Benchmark time of functions and profile where time is spent within function
library(microbenchmark)
library(profvis)
# Load Analysis Functions
source(here("scripts", "CoulbournAnalysisFunctions.R"))

# Packages for parallel computing
library(foreach)
library(doParallel)

numCores = 16
registerDoParallel(numCores)

# This script was used to develop the analysis function from an inefficient while loop to a for loop
# While loop version: coulbourn_actioncleantimestamps
# For loop version: coulbourn_actioncleantimestamps1
# Comparison cases benchmarked at the end of the script - For loop version is over 5x as fast as the while loop version
# For loop version is now in CoulbournAnalysisFunctions as the chosen script



# Original Function ----------------------------------------------------------------

coulbourn_actioncleantimestamps <- function(A_on, A_off) {
  # Function takes a vector of Action ON times and a vector of Action OFF times from a session
  # Data are processed to fix any issues with missing/non-matching ON/OFF times
  # Assumptions made are that:
  # 1) If an action OFF signal is the first timestamp, then the first action ON was before the session started (so first A_On set to time = 0)
  # 2) Any ON or OFF signal that is doesn't pair chronologically with the other signals reflects an action ON/OFF signal occuring faster than the temporal resolution of the system
  #   A corresponding On/Off signal is added so that the ON-OFF duration is 0 (i.e. they occurred at the same time)
  # These issues happen surprisingly often for Coulbourn Lever Pressing data in particular
  # N.B. output is a data frame of A_on and A_off, split this output apart for use with other functions.
  
  if (length(A_on) > 0 | length(A_off) > 0) {
    
    # Make sure data are in chronological order!!! Otherwise loop will go on forever
    A_on <- sort(A_on)
    A_off <- sort(A_off)
    
    
    # create new empty variable to populate with values
    # Note that this inefficient method is required because Coulbourn timestamps don't obey logic,
    # There is no way to precalculate the number of events because the on and off signals might both contain independent inconsistencies
    ts = double()
    event = character()
    
    if (is_empty(A_on) & !is_empty(A_off)) {
      # Check to see if there are no ON signals
      # Situation - special case where event initiated prior to recording start
      A_on <- c(0,A_on)
    }
    if (!is_empty(A_on) & is_empty(A_off)) {
      # Check to see if there are no OFF signals
      # Situation - special case where OFF signal occurred within temporal resolution of system for first repsonse
      A_off <- c(A_on[1], A_off)
    }
    if (A_on[1] > A_off[1]) {
      # Check to see if the first event is an Off. Assumption is that the on signal happened before recording started.
      # Artificially set the action initiation as time = 0 (i.e. session start)
      A_on <- c(0,A_on)
    }
    
    # Efficiency Check --------------------------------------------------------
    
    # Check to see if the data are all correct now i.e. all A_on have a corresponding A_off, and each A_on[1] <= A_off[i]
    
    if (length(A_on) == length(A_off)) {
      # Test whether all A_on <= A_off
      correctdata <- as.logical(A_on <= A_off)
      correctdata2 <- as.logical(A_on[2:length(A_on)] >= A_off[1:(length(A_off)-1)])
      if (length(which(correctdata == FALSE)) == 0 & length(which(correctdata2 == FALSE)) == 0) {
        # All data are correct so return unprocessed
        A_data <- data.frame(A_on, A_off)
        return(A_data)
        
      }
    } else {
      
      
      
      # add first onset [which will exist since A_on is not empty AND we have made sure the first on is before the first off]
      
      ts <- c(ts, A_on[1])
      event <- c(event, "A_on")
      A_on <- A_on[-1]
      
      
      # Keep doing the following steps until the on and off signal vectors are empty
      # We will delete things as they are moved from A_on and A_off and transfer them
      # to the new vector with any extra on and off signals required
      while (length(A_on) > 0 | length(A_off) > 0) {
        
        # First check if the last thing in the event list is an On or Off
        if (tail(event,1) == "A_on"){
          ### Last event is an On ###
          
          if (is_empty(A_on) & !is_empty(A_off)){
            # No more recorded ON signals - deal with remaining OFF signals if any exist
            # Solution -> add next OFF signal to ts and event lists
            ts <- c(ts, A_off[1])
            event <- c(event, "A_off")
            # Remove value from off list
            A_off <- A_off[-1]
            
          } else if (!is_empty(A_on) & is_empty(A_off)){
            # No more recorded OFF signals - Remaining ON signals to be dealt with
            # Situation  -> an On signal occurred without a corresponding OFF signal - happened within temporal resolution of system
            # Solution -> add an extra OFF event to the list
            ts <- c(ts, tail(ts,1))
            event <- c(event, "A_off")
            
          }
          
          else if (A_off[1] >= tail(ts,1)) {
            
            # OFF event comes after previous On event
            if (A_off[1] <= A_on[1] ) {
              # Condition 1: Off event is before Next On event and after previous On event
              # Situation -> OFF signal where it was expected
              # Solution -> add to ts and event lists
              ts <- c(ts, A_off[1])
              event <- c(event, "A_off")
              # Remove value from off list
              A_off <- A_off[-1]
              
            } else if (A_off[1] > A_on[1]) {
              # Condition 2: Off event is after Next On event AND correctly after previous ON event
              # Situation -> OFF event after previous ON happened faster than system temporal resolutions
              # Solution -> add an extra OFF event to the list
              ts <- c(ts, tail(ts,1))
              event <- c(event, "A_off")
              
            }
          }
          
        }
        
        
        else if (tail(event,1) == "A_off") {
          ### Last event is an Off ###
          
          if (is_empty(A_off) & !is_empty(A_on)){
            # No more recorded Off signals - deal with remaining OFF signals if any exist
            # Solution -> add next ON signal to ts and event lists
            ts <- c(ts, A_on[1])
            event <- c(event, "A_on")
            # Remove value from on list
            A_on <- A_on[-1]
            
          } else if (!is_empty(A_off) & is_empty(A_on)){
            # No more recorded ON signals - Remaining OFF signals to be dealt with
            # Situation -> an OFF signal occurred without a corresponding ON signal - happened within temporal resolution of system
            # Solution -> add an extra ON event to the list
            ts <- c(ts, tail(ts,1))
            event <- c(event, "A_on")
            
          }
          
          else if(A_on[1] >= tail(ts,1)){
            
            if (A_on[1] <= A_off[1]){
              # COndition 3: Next On event comes before the Next Off event AND Next ON event comes after previous OFF event
              # Situation -> On signal where it is expected
              # SOlution -> add to ts and event lists
              ts <- c(ts, A_on[1])
              event <- c(event, "A_on")
              # Remove value from off list
              A_on <- A_on[-1]
              
            } else if(A_on[1] > A_off[1]){
              # Condition 4: Next On event comes AFTER the Next Off event AND Next ON event comes after previous OFF event
              # Situation -> On event after previous OFF happened faster than system temporal resolutions
              # Solution -> add an extra ON event to the list
              ts <- c(ts, tail(ts,1))
              event <- c(event, "A_on")
              
            }
          }
        }
        
        # While loop end
      }
      
      # Check to see if final event was an ON. If so, then Add a corresponding OFF straight afterwards
      # IMPORTANT - This is a major assumption!!! It is also possible that the final action doesn't have a corresponding OFF signal because the session ended.
      # However, this is really hard to judge without looking at each case individually.
      # Given that the end of the session usually has little of interest, this is a conservative assumption about the animals' behaviour that is consistent
      # with the assumptions earlier in this script.
      
      if (tail(event,1) == "A_on") {
        ts <- c(ts, tail(ts,1))
        event <- c(event, "A_off")
        
      }
      #Finally convert the data back into variables A_on and A_off
      
      A_on <- ts[event == "A_on"]
      A_off <- ts[event == "A_off"]
      
      A_data <- data.frame(A_on, A_off)
      return(A_data)
      
    }
    
  } else {
    A_data <- data.frame(A_on, A_off)
    return(A_data)
  }
  
}

# New Function ----------------------------------------------------------------

coulbourn_actioncleantimestamps1 <- function(A_on, A_off) {
  # Function takes a vector of Action ON times and a vector of Action OFF times from a session
  # Data are processed to fix any issues with missing/non-matching ON/OFF times
  # Assumptions made are that:
  # 1) If an action OFF signal is the first timestamp, then the first action ON was before the session started (so first A_On set to time = 0)
  # 2) Any ON or OFF signal that is doesn't pair chronologically with the other signals reflects an action ON/OFF signal occuring faster than the temporal resolution of the system
  #   A corresponding On/Off signal is added so that the ON-OFF duration is 0 (i.e. they occurred at the same time)
  # These issues happen surprisingly often for Coulbourn Lever Pressing data in particular
  # N.B. output is a data frame of A_on and A_off, split this output apart for use with other functions.
  
  
  if (length(A_on) > 0 | length(A_off) > 0) {
    
    # Make sure data are in chronological order!!! Otherwise loop will go on forever
    A_on <- sort(A_on)
    A_off <- sort(A_off)
    
    
    if (is_empty(A_on) & !is_empty(A_off)) {
      # Check to see if there are no ON signals
      # Situation - special case where event initiated prior to recording start
      A_on <- c(0,A_on)
    }
    if (!is_empty(A_on) & is_empty(A_off)) {
      # Check to see if there are no OFF signals
      # Situation - special case where OFF signal occurred within temporal resolution of system for first response
      A_off <- c(A_on[1], A_off)
    }
    if (A_on[1] > A_off[1]) {
      # Check to see if the first event is an Off. Assumption is that the on signal happened before recording started.
      # Artificially set the action initiation as time = 0 (i.e. session start)
      A_on <- c(0,A_on)
    }
    
    
    
      
      # Combine all on and off signals and order them by time AND [A_on before A_off] when there is a tie 
      # [we will deal with issues this might cause, but it will be correct most of the time]
      time = c(A_on, A_off)
      event = c(rep("A_on", length(A_on)), rep("A_off", length(A_off)))
      # Event ID is just a numeric version of even 1 = On, 2 = Off
      eventID = c(rep(1, length(A_on)), rep(2, length(A_off)))

      #  Order data
      A = data.frame(time, event, eventID)
      A = setorder(A, time, eventID)
      # Delete eventID to save memory
      A <- A[, c("time","event")]
      # Add a numeric order to the list so we can manipulate values later
      A$eventorder = c(1:nrow(A))
      
      
      # Check for sequential times that are tied for more than 1 event
      # N.B. if ties[i] = 0, then that event[i] and event[i+1] happened at the same time
      ties = diff(A$time)
      ties = which(ties == 0)
      
      # If there are ties to consider
      if (length(ties) != 0) {
        
        # Test to ignore first tie if it is also the first event
        if(ties[1] == 1) {j = 2} else {j = 1}
          
        for(i in c(j:length(ties))){
          
          idx = ties[i]
          
          if((A$event[idx-1] == "A_on" & A$event[idx] == "A_on" &  A$event[idx+1] == "A_off")) {
            
            # If the tied A_on and A_off signals are in the wrong order, switch them around
            # Note that this is the only way they can be ordered incorrectly since:
            # 1) We have already ordered the data so any times that are tied will put an A_on before an A_off
            # 2) Coulbourn never records more than 1x A_on and 1x A_off with the same time
            # 3) If the previous A_on had a simultaneous A_off then it would also be recorded as such
            
            A$eventorder[idx] = idx+1
            A$eventorder[idx+1] = idx
            
            # Important to re-order the data after each change in case multiple ties are adjacent to each other
            A = setorder(A, eventorder)
            
          }
        }
      
      }
      
      # clear unused temporary variables for memory efficiency
      rm(ties,event, eventID, time, A_on, A_off)
      

# Initialise variables
      previous_event = A$event[1]
      current_event = A$event[2]
      
      # We will add to these variables if there is a need for new events/times to fix the data
      extra_events = character()
      extra_times = double()
      extra_eventorder = double()
      # J = counter for extra events
      j = 0
      
      for (i in c(2:nrow(A))) {
        current_event = A$event[i]
        
        # First check for an A_on signal
        if (current_event == "A_on" & previous_event == "A_off") { } 
        
        else if (current_event == "A_on" & previous_event == "A_on") {
          
          j = j + 1 
          extra_events[j] = "A_off"
          extra_times[j] = A$time[i]
          extra_eventorder[j] = A$eventorder[i] + 0.5

          } 
        else if (current_event == "A_off" & previous_event == "A_on") { } 
        
        else if (current_event == "A_off" & previous_event == "A_off") { 
          j = j + 1 
          extra_events[j] = "A_on"
          extra_times[j] = A$time[i]
          extra_eventorder[j] = A$eventorder[i] + 0.5
          
          } 
        
        # Update variables for next 
        previous_event = current_event
        
      }
      
# One more check to make sure the final action is an OFF signal
      
      if (previous_event == "A_on") {
        j = j + 1 
        extra_events[j] = "A_off"
        extra_times[j] = A$time[i]
        extra_eventorder[j] = A$eventorder[i] + 0.5
      }
      
      
# Consolidate newdata with old data if necessary
      if (j >0 ) {
        A_new <- data.frame(extra_times, extra_events, extra_eventorder)
        colnames(A_new) <- c("time","event", "eventorder")
        
        # Join with original data
        A <- A[, c("time","event", "eventorder")]
        templist <- list(A, A_new)
        A <- rbind(A, A_new, use.names = TRUE)
        A <- setorder(A, eventorder)
      }
      
      
### Extract data and return function output
      A_on = A$time[A$event == "A_on"]
      A_off = A$time[A$event == "A_off"]
      A_data <- data.frame(A_on, A_off)
      return(A_data)

      
# End Data re-ordering   
  } else {
    A_data <- data.frame(A_on, A_off)
    return(A_data)
    }
  
}


# New FUnction 2 - No loops! ----------------------------------------------

coulbourn_actioncleantimestamps2 <- function(A_on, A_off) {
  # Function takes a vector of Action ON times and a vector of Action OFF times from a session
  # Data are processed to fix any issues with missing/non-matching ON/OFF times
  # Assumptions made are that:
  # 1) If an action OFF signal is the first timestamp, then the first action ON was before the session started (so first A_On set to time = 0)
  # 2) Any ON or OFF signal that is doesn't pair chronologically with the other signals reflects an action ON/OFF signal occuring faster than the temporal resolution of the system
  #   A corresponding On/Off signal is added so that the ON-OFF duration is 0 (i.e. they occurred at the same time)
  # These issues happen surprisingly often for Coulbourn Lever Pressing data in particular
  # N.B. output is a data frame of A_on and A_off, split this output apart for use with other functions.
  
  
  if (length(A_on) > 0 | length(A_off) > 0) {
    
    # Make sure data are in chronological order!!! Otherwise loop will go on forever
    A_on <- sort(A_on)
    A_off <- sort(A_off)
    
    
    if (is_empty(A_on) & !is_empty(A_off)) {
      # Check to see if there are no ON signals
      # Situation - special case where event initiated prior to recording start
      A_on <- c(0,A_on)
    }
    if (!is_empty(A_on) & is_empty(A_off)) {
      # Check to see if there are no OFF signals
      # Situation - special case where OFF signal occurred within temporal resolution of system for first response
      A_off <- c(A_on[1], A_off)
    }
    if (A_on[1] > A_off[1]) {
      # Check to see if the first event is an Off. Assumption is that the on signal happened before recording started.
      # Artificially set the action initiation as time = 0 (i.e. session start)
      A_on <- c(0,A_on)
    }
    
    
    
    
    # Combine all on and off signals and order them by time AND [A_on before A_off] when there is a tie 
    # [we will deal with issues this might cause, but it will be correct most of the time]
    time = c(A_on, A_off)
    event = c(rep("A_on", length(A_on)), rep("A_off", length(A_off)))
    # Event ID is just a numeric version of even -1 = On, 1 = Off
    eventID = c(rep(-1, length(A_on)), rep(1, length(A_off)))
    
    #  Order data
    A = data.frame(time, event, eventID)
    A = setorder(A, time, eventID)
    # Add a numeric order to the list so we can manipulate values later
    A$eventorder = c(1:nrow(A))
    
    
    # Check for sequential times that are tied for more than 1 event
    # N.B. if ties[i] = 0, then that event[i] and event[i+1] happened at the same time
    ties = diff(A$time)
    ties = which(ties == 0)
    
    # If there are ties to consider
    if (length(ties) != 0) {
      
      # Test to ignore first tie if it is also the first event
      if(ties[1] == 1) {j = 2} else {j = 1}
      
      for(i in c(j:length(ties))){
        
        idx = ties[i]
        
        if((A$event[idx-1] == "A_on" & A$event[idx] == "A_on" &  A$event[idx+1] == "A_off")) {
          
          # If the tied A_on and A_off signals are in the wrong order, switch them around
          # Note that this is the only way they can be ordered incorrectly since:
          # 1) We have already ordered the data so any times that are tied will put an A_on before an A_off
          # 2) Coulbourn never records more than 1x A_on and 1x A_off with the same time
          # 3) If the previous A_on had a simultaneous A_off then it would also be recorded as such
          
          A$eventorder[idx] = idx+1
          A$eventorder[idx+1] = idx
          
          # Important to re-order the data after each change in case multiple ties are adjacent to each other
          A = setorder(A, eventorder)
          
        }
      }
      
    }
    
    # clear unused temporary variables for memory efficiency
    rm(ties,event,time, A_on, A_off)
    
    # a value of 0 indicates that this index and the one above it are the same [i.e. two A_on or two A_off in a row] 
    repeats <- diff(A$eventID)
    # Indices in A of first repeat value
    repeatsidx <- which(repeats == 0)
    # Identity of the repeated event name
    repeats_ID <- A$event[repeatsidx]
    # Time of the first repeated event
    extra_time <- A$time[repeatsidx]
    # New position to insert extra event between repeats
    extra_eventorder <- A$eventorder[repeatsidx] + 0.5
    
    # Create datatable and recode the new event identities 
    A_extra <- data.table(repeatsidx, repeats_ID, extra_time, extra_event = repeats_ID, extra_eventorder)
    recodeData <- data.table(
      oldvalues = c("A_on", "A_off"),
      newvalues = c("A_off", "A_on")
    )
    # Recode data to reflect the correct identity of the extra events to insert
    A_extra[recodeData, extra_event := newvalues, on =. (extra_event =  oldvalues)]
    # Drop unnecessary variables
    A_extra <- A_extra[, c("extra_time","extra_event", "extra_eventorder")]
    # Relabel columns to merge with A data
    colnames(A_extra) <- c("time","event", "eventorder")
    
    # Join with original data
    A <- A[, c("time","event", "eventorder")]
    A <- rbindlist(list(A, A_extra), use.names = TRUE)
    # Re organise the data in correct temporal order
    A <- setorder(A, eventorder)
    
    # One last check to make sure final event is an OFF signal
    if (A$event[nrow(A)] == "A_on") {
      # Create extra data and append to A
      A_final <- data.frame(time = A$time[nrow(A)], 
                            event = "A_off", 
                            eventorder = (A$eventorder[nrow(A)] + 0.5))
      A <- rbindlist(list(A, A_final), use.names = TRUE)
      A <- setorder(A, eventorder)
      
    }
    
    ### Extract data and return function output
    A_on = A$time[A$event == "A_on"]
    A_off = A$time[A$event == "A_off"]
    A_data <- data.frame(A_on, A_off)
    return(A_data)
    
    
    # End Data re-ordering   
  } else {
    A_data <- data.frame(A_on, A_off)
    return(A_data)
  }
  
}

# Test Bed Data -----------------------------------------------------------
# Consistent single
A_on_1 <- seq(from = 1, to = 1006, by = 2)
A_off_1 <- seq(from = 2, to = 1001, by = 2)

# Consistent double
A_on_2 <- seq(from = 1, to = 2006, by = 2)
A_off_2 <- seq(from = 2, to = 2001, by = 2)

# Consistent triple
A_on_3 <- seq(from = 1, to = 3006, by = 2)
A_off_3 <- seq(from = 2, to = 3001, by = 2)

# random numbers
A_on_rand <- round(runif(length(A_on_1), min=0, max=1000), digits = 2)
A_off_rand <- round(runif(length(A_off_1), min=0, max=1000), digits = 2) 
# Test --------------------------------------------------------------------

coulbourn_actioncleantimestamps2(A_on_1, A_off_1)


testbench <- microbenchmark("While_1000_ordered" = coulbourn_actioncleantimestamps(A_on_1, A_off_1),
                            "While_2000_ordered" = coulbourn_actioncleantimestamps(A_on_2, A_off_2), 
                            "While_3000_ordered" = coulbourn_actioncleantimestamps(A_on_3, A_off_3),
                            "While_1000_Rand" = coulbourn_actioncleantimestamps(A_on_rand, A_off_rand),
                            "For_1000_ordered" = coulbourn_actioncleantimestamps1(A_on_1, A_off_1),
                            "For_2000_ordered" = coulbourn_actioncleantimestamps1(A_on_2, A_off_2), 
                            "For_3000_ordered" = coulbourn_actioncleantimestamps1(A_on_3, A_off_3),
                            "For_1000_Rand" = coulbourn_actioncleantimestamps1(A_on_rand, A_off_rand),
                            "None_1000_ordered" = coulbourn_actioncleantimestamps2(A_on_1, A_off_1),
                            "None_2000_ordered" = coulbourn_actioncleantimestamps2(A_on_2, A_off_2), 
                            "None_3000_ordered" = coulbourn_actioncleantimestamps2(A_on_3, A_off_3),
                            "None_1000_Rand" = coulbourn_actioncleantimestamps2(A_on_rand, A_off_rand),
                            times = 100)
loop_results <- autoplot(testbench)
loop_results

A_while <- coulbourn_actioncleantimestamps(A_on_rand, A_off_rand)
A_For <- coulbourn_actioncleantimestamps1(A_on_rand, A_off_rand)
A_None <- coulbourn_actioncleantimestamps2(A_on_rand, A_off_rand)

A_while - A_For
A_while - A_None
head(A_For - A_None)
sum(A_For - A_None)

# Action Bin  -------------------------------------------------------------
# test data
A_on <- seq(from = 1, to = 11000, by = 2)
A_off <- seq(from = 2, to = 11001, by = 2)
A_diff <- A_off - A_on



bin_start = c(1.5, 4, 4, 7,   710, 730, 10100)
bin_end =   c(  3, 6, 7.5, 700, 720, 740, 11000)

Aoff_binstart_diff <- A_off-bin_start
Aoff_binend_diff <- A_off-bin_end

for(i in c(1:length(bin_start))) {
# Which on signals are within the time bin
  # Sum all the differences between these A_on signals and their corresponding A_off signal
sum_idx <- (which(A_on >= bin_start[i] & A_on <= bin_end[i]))
as.integer(A_on >= bin_start[i] & A_on <= bin_end[i])
# Which signals start before the bin starts
  # Find the difference between the bin_start and A_off signal, and add this value to the total time
add_idx <- (which(A_on < bin_start[i] & A_off >= bin_start[i]))

# Which signals start before the end of the bin and continue afterwards
  # Find the difference between bin_end and A_off signal and minus this from the total time
subtract_idx <- (which(A_on <= bin_end[i] & A_off > bin_end[i]))

print(sum(A_diff[sum_idx], Aoff_binstart_diff[add_idx])-Aoff_binend_diff[subtract_idx])
}

# trying <- (A_on >= bin_start[i] & A_on <= bin_end[i])
# 
# speedtesting <- microbenchmark("Sumwhich" = sum(A_diff[which(A_on >= bin_start[i] & A_on <= bin_end[i])]),
# "SumConvertingMultiply" = sum(as.integer(A_on >= bin_start[i] & A_on <= bin_end[i])*A_diff), 
# times = 1000)
# autoplot(speedtesting)
# 





# Old Function for binning action durations -------------------------------

OldFunction <- function(A_on, A_off, bin_start, bin_end){

bin_A_freq <- replicate(length(bin_start), 0)
bin_A_dur <- replicate(length(bin_start), 0)

for (i in c(1:length(bin_start))){
  bin_A_freq[i] = sum(A_on >= bin_start[i] & A_on < bin_end[i])
  
  for (j in c(1:length(A_on))){
    # action starts before timebin and ends within timebin
    if ( A_on[j] < bin_start & A_off[j] >= bin_start[i] & A_off[j] < bin_end[i]) {
      bin_A_dur[i] <- bin_A_dur[i] + (A_off[j] - bin_start[i])
      # action after timebin starts and before timebin ends
    } else if ( A_on[j] >= bin_start[i] & A_off[j] < bin_end[i] ) {
      bin_A_dur[i] <- bin_A_dur[i] + (A_off[j] - A_on[j])
      # action starts within the timebin , and ends after timebin
    } else if ( A_on[j] >= bin_start[i] & A_on[j] < bin_end[i] & A_off[j] >= bin_end[i] ) {
      bin_A_dur[i] <- bin_A_dur[i] + bin_end[i] - A_on[j]
      # action before bin starts and continues until after bin
    } else if ( A_on[j] <= bin_start[i] & A_off[j] > bin_end[i] ) {
      bin_A_dur[i] <- bin_A_dur[i] + (bin_end[i] - bin_start[i])
    } else {bin_A_dur[i] <- bin_A_dur[i]}
  }
}

}


speedtesting <- microbenchmark("OldFunction" = OldFunction(A_on, A_off, bin_start, bin_end), times = 1)
autoplot(speedtesting)

newFunction <- function(A_on, A_off, bin_start, bin_end){
  
  # pre-allocate variables with 0s
  bin_A_freq <- replicate(length(bin_start), 0)
  bin_A_dur <- replicate(length(bin_start), 0)
  bin_A_sum <- replicate(length(bin_start), 0)
  bin_A_add <- replicate(length(bin_start), 0)
  bin_A_subtract <- replicate(length(bin_start), 0)
  # Simplify calculations by doing them all in advance for the most common case
  A_diff <- A_off - A_on
  
  for (i in c(1:length(bin_start))){
    # Frequency counts [the easy stuff]
    bin_A_freq[i] <- sum(as.integer(A_on >= bin_start[i] & A_on < bin_end[i]))
    # Duration counts [a bit more going on]
    sum_idx <- (which(A_on >= bin_start[i] & A_on <= bin_end[i]))
    add_idx <- (which(A_on < bin_start[i] & A_off >= bin_start[i]))
    subtract_idx <- (which(A_on <= bin_end[i] & A_off > bin_end[i]))
    
    # Unfortunately no vectorised code for this without lots of redundant steps/inefficiency
    # So just test if any indices were found and update elements appropriately
    if(!is_empty(sum_idx)) {
      bin_A_sum[i] <- sum(A_diff[sum_idx])
      } 
    
    if(!is_empty(add_idx)) {
      bin_A_add[i] <- A_off[add_idx] - bin_start[i]
      } 
    
    if(!is_empty(subtract_idx)) {
      bin_A_subtract[i] <- A_off[subtract_idx] - bin_end[i]
      } 
  }
  # Combine all of these at the end
  bin_A_dur = bin_A_sum + bin_A_add - bin_A_subtract
  
}
  


speedtesting <- microbenchmark("OldFunction" = OldFunction(A_on, A_off, bin_start, bin_end),
                               "NewFunction" = newFunction(A_on, A_off, bin_start, bin_end),
                               times = 10)
speedtesting

autoplot(speedtesting)

