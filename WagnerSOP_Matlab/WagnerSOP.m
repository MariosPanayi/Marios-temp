%C:\Users\mpanagi\Documents\MATLAB

clear all
close all
clc

%Wagner SOP - Based on trial list
% step 1 create trial list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
length = 500;
ISI = 60;
list = [1, 1, 2];
timeUnits = 1;
types = 2;

[timeList, stimulus] = createTrialList(length,ISI,list,timeUnits,types);

%step 2 apply SOP model to time list

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%initialise values
%Tranisition probabilities
p1 = 0.1;
pd1 = 0.1;
pd2 = 0.02;
%pd2 = pd1/5;

%initialise nodes
A1 = zeros(size(timeList,1)+1,1);
A2 = zeros(size(timeList,1)+1,1);
I = zeros(size(timeList,1)+1,1);

A1(1,1) = 0;
A2(1,1) = 0;
I(1,1) = 1;

%Transitions
for t = 2:size(timeList,1)+1
    A1(t) = A1(t-1) + stimulus(t-1,1)*(p1*I(t-1)) - pd1*(A1(t-1));
    A2(t) = A2(t-1) + pd1*(A1(t-1)) - pd2*A2(t-1);
    I(t) = I(t-1) - stimulus(t-1,1)*(p1*I(t-1)) + pd2*A2(t-1); 
end
figure
plot(A1)
hold on
plot(A2)

hold off

