# Variables needed by this function
# bin_time

# ExampleData

A_on = c(1,1,1,2,2)
A_off = c(1,2,3)

# Pre-Check
# Check to see if data are empty
if (is_empty(A_on) & is_empty(A_off)) {
  # If both data sets are empty then return zeros in the output
  bin_A_freq <- replicate(length(bin_time), 0)
  bin_A_dur <- replicate(length(bin_time), 0)
  bin_A_data <- cbind(bin_A_freq,bin_A_dur)
} else {
  
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
  
  }


