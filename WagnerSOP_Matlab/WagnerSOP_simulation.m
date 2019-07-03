%Wagner SOP - Based on trial list

% step 1 create trial list

length = 10;
ISI = 90;
list = [1,1,2];
timeUnits = 1;
types = 2;

[timeList, stimulus] = createTrialList(length,ISI,list,timeUnits,types);

%step 2 apply SOP model to time list
plot(timeList, stimulus)