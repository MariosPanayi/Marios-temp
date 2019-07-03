%Wagner SOP Trial Structure generator
%Parameter units are in seconds
%eg:
% length = 10; length of the stimulus (s)
% ISI = 90; inter stimulus interval
% list = [1,1,2]; list of trial types, different numbers for each trial
% timeUnits = 1; time steps for final list (s)
% types = 2; total number of stimuli used
%
%Output: 
%timeList: a nx1 matrix where n is the total number of time units in the session
%stimulus: a nxm matrix where m is the number of stimuli. 0 when stimulus
%is not present, 1 when present



function [timeList, stimulus]= createTrialList(length,ISI,list,timeUnits,types)

stimulus.length = length;
stimulus.ISI = ISI;
stimulus.list = list;
stimulus.timeUnits = timeUnits;
stimulus.types = types;

total.stimNo = size(stimulus.list,2);
total.sessionLength = stimulus.ISI + total.stimNo*(stimulus.length+stimulus.ISI);

session.timeList = [0:stimulus.timeUnits:(total.sessionLength/stimulus.timeUnits)]';
session.Stimulus = zeros(size(session.timeList,1),stimulus.types);

j = stimulus.ISI/stimulus.timeUnits;
k = j+((stimulus.length)/stimulus.timeUnits);
for i = 1:size(session.timeList,1)-1
    if i<=j
        session.Stimulus(i,1) = 0;
    elseif i>j && i<k
        session.Stimulus(i,1) = 1;
    elseif i==k
        session.Stimulus(i,1) = 1;
        j = k+(stimulus.ISI/stimulus.timeUnits);
        k = j+((stimulus.length)/stimulus.timeUnits);
    end
end
timeList = session.timeList;
stimulus = session.Stimulus;
end


