function [t50_data, rsq_data, datapks, datapklocs, peak_intercepts] = aFCV_Analyze_t50(data, plotfigs, pk_threshold )

% Input data as matrix(time, tracenumber)


%Check Inputs, if no values supplied use default values
if nargin < 2
    warning('supply values for plotfigs and pk_threshold - Defaults used')
    %Default = plot figures; Peak Threshold 85%
    plotfigs = 1;
    pk_threshold = 0.85;
elseif nargin < 3
       warning('supply value for pk_threshold - Defaults used') 
       pk_threshold = 0.85;
end

%%

if plotfigs
title('Peak locaitons DA - Individual Traces')
hold on
end

for i = 1:size(data, 2)
    %Plot
    if plotfigs
    findpeaks(data(:,i), 'MinPeakProminence',max(data(:,i))*pk_threshold)
    end
    

    % Find Peaks
    %if data is all negative i.e. poor signal, then set as NaN\
    try
    [datapks_temp,datapklocs_temp] = findpeaks(data(:,i), 'MinPeakProminence',max(data(:,i))*pk_threshold);
    catch
   % If peaks still not found, just use max value
        [datapks_temp, datapklocs_temp] = max(data(:,i));
    end
   
    % If no peaks found, lower threshold
    if isempty(datapklocs_temp)
    [datapks_temp,datapklocs_temp] = findpeaks(data(:,i), 'MinPeakProminence',max(data(:,i))*(pk_threshold/2));
    end    
    
    % If peaks still not found, just use max value
    if isempty(datapklocs_temp)
        [datapks_temp, datapklocs_temp] = max(data(:,i));
    end
    
    % If there are multiple peaks, take the largest peak
    if length(datapklocs_temp) > 1
        [maxpk, maxpkloc] = max(datapks_temp);
        datapks_temp = datapks_temp(maxpkloc);
        datapklocs_temp = datapklocs_temp(maxpkloc);
    end
    
    %assign values to array
    datapks(i) = datapks_temp;
    datapklocs(i) = datapklocs_temp;
    
end
if plotfigs
ylabel('nM DA')
xlabel('Time in deci-seconds')
hold off
end
%%

if plotfigs
figure
hold on
end

for  i = 1:size(data, 2)
    
    
% (2) set min value of post peak data = 0
%Truncate data from peak
Y = data(datapklocs(i):end,i);
Y = Y- min(Y);
endpoints = 10:1:length(Y);
data_Fit = zeros(length(endpoints),1);
for j = 1:length(endpoints)
    Ytemp = Y(1:endpoints(j));
    Ytemp = smooth(Ytemp);
    Xtemp = [0:length(Ytemp)-1]';
    [ftemp, goftemp] = fit(Xtemp,Ytemp,'exp1');
    data_Fit(j) = goftemp.rsquare;
end

[~,MaxFitIndex] = max(data_Fit);
% reset Y
if (datapklocs(i)+endpoints(MaxFitIndex)) > length(data(datapklocs(i):end,i))
    Y = data(datapklocs(i):end,i);
else
Y = data(datapklocs(i):datapklocs(i)+endpoints(MaxFitIndex),i);
end
%Set min value to 0
Y = Y- min(Y);
Y = smooth(Y);
% Change metric to percentage to easily compare slopes in figure if desired
%Y = (Y/max(Y)) *100;
% (3) model negative exponential
X = [0:length(Y)-1]';
% function is y = a*e^(bX)
% a = Y value at t = 0
% b = rate constant, expressed as reciprocal of X time units
% Half life = ln(2)/b
try
[f, gof] = fit(X,Y,'exp1');
coeffs = coeffvalues(f);
rsq_data(i) = gof.rsquare;
a_data(i) = coeffs(1);
b_data(i) = coeffs(2);
t50_data(i) = log(2)/b_data(i);

catch %catch errors in fit and return NaNs
rsq_data(i) = NaN;
a_data(i) = NaN;
b_data(i) = NaN;
t50_data(i) = NaN;
end
if plotfigs
plot(f,X,Y)
end 

end

if plotfigs
title('Negative Exponential Model fit to data Data')
ylabel('DA nM - shifted to min = 0')
xlabel('Time in deci-seconds from peak release')
hold off
end


% Make values positive
t50_data = t50_data*-1;


% Identify only good fitting data
% poorfit_data = find(rsq_data < 0.90);
% goodfit_data = t50_data;
% if poorfit_data
%     goodfit_data(poorfit_data) = [];  
% end

% variables to save as ouput

peak_intercepts = a_data;




end

