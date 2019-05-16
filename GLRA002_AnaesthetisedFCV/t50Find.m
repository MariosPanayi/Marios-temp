function [t50, rsq, a, b, Y, pks, pklocs, MaxFitIndex] = t50Find(data)
% Input a vector of dopamine response data - Function finds the t50 rate
% index of rate of decay post-peak response

% Input 
%        data = vector of data
% Output
%       t50 = time (based on sampling rate of data) at which modelled signal is at 50
%       rsq = R squared of exponential model fit
%           Fit function is y = a*e^(bX)
%       a = Y value at t = 0
%       b = rate constant, expressed as reciprocal of X time units
%           Half life = ln(2)/b
%       Y = Data that was modelled/Fit
%       pks = location of peak DA in raw data 
%       pklocs = time at which the peak DA occurred
%       MaxFitIndex = Time (from peak) at which data was truncated (to remove additional noise/rebound effects) 
%% 1. Find Peak value

[pks,pklocs] = findpeaks(data, 'MinPeakProminence',max(data)*.85, 'NPeaks', 1);
% Code hangs up here because some traces do not threshold their peaks well
% here... need new decision criterion for peak location - consider current
% identification of peak data in original analysis

%% 2. Determine how much data to fit post peak

% Truncate data from peak to end of file
Y = data(pklocs:end);
% If any data goes below baseline (by 10%), remove end of data
tooLow = Y(1)*.10;
if find(Y < (-tooLow))
    lowpts = find(Y < (-tooLow));
    Y = Y(1:lowpts(1));
end
% Make lowest Value 0 to avoid negative numbers during model fit
Y = Y- min(Y);

% Assess fit with different lengths of data. Important since a number of
% traces show weird rebound effects.
endpoints = 5:1:length(Y);
data_Fit = zeros(length(endpoints),1);
for j = 1:length(endpoints)
    Ytemp = Y(1:endpoints(j));
    Ytemp = smooth(Ytemp);
    Xtemp = [0:length(Ytemp)-1]';
    [ftemp, goftemp] = fit(Xtemp,Ytemp,'exp1');
    data_Fit(j) = goftemp.rsquare;
end

[~,MaxFitIndex] = max(data_Fit);
% reset Y -> ensure indices are not greater than data set
if (pklocs+endpoints(MaxFitIndex)) > length(data(pklocs:end))
    Y = data(pklocs:end);
else
Y = data(pklocs:pklocs +endpoints(MaxFitIndex));
end
%Set min value to 0
Y = Y- min(Y);
Y = Y';

%% (3) model negative exponential
X = [0:length(Y)-1]';
% function is y = a*e^(bX)
% a = Y value at t = 0
% b = rate constant, expressed as reciprocal of X time units
% Half life = ln(2)/b
[f, gof] = fit(X,Y,'exp1');
coeffs = coeffvalues(f);
rsq = gof.rsquare;
a = coeffs(1);
b = coeffs(2);
t50 = -1*(log(2)/b);

