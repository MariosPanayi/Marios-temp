
%% Load Data
load('C:\Users\mpanagi\Documents\GitHub\Marios-temp\GLRA002_AnaesthetisedFCV\GLRA002_BaselinePreDrugData_t50Analysis.mat')

%% Plot Data
figure;
subplot(2,1,1)
hold on
for i= 1:size(KO, 2)
plot(KO(:,i));
end
title('GLRA002 KO Baseline - Individual Traces')
ylabel('Time')
xlabel('nM DA')
hold off

subplot(2,1,2)
hold on
for i= 1:size(WT, 2)
plot(WT(:,i));
end
title('GLRA002 WT Baseline - Individual Traces')
ylabel('Time')
xlabel('nM DA')
hold off

%% To analyse t50:
% (1) identify the point of peak release
% (2) set min value of post peak data = 0
% (3) model negative exponential


% (1) identify the point of peak release
% using find peaks function, find peak prominences in the data that are at
% least 85% of the max release. Make sure only a single peak is identified
% for each animal


figure
subplot(2,1,1)
title('Peak DA - WTs')
hold on
for i = 1:size(WT, 2)
    %Plot
    
    findpeaks(WT(:,i), 'MinPeakProminence',max(WT(:,i))*.85)
    % Save data
    [WTpks(i),WTpklocs(i)] = findpeaks(WT(:,i), 'MinPeakProminence',max(WT(:,i))*.85);
end
hold off

subplot(2,1,2)
title('Peak DA - KOs')
hold on
for i = 1:size(KO, 2)
    %Plot
    
    findpeaks(KO(:,i), 'MinPeakProminence',max(KO(:,i))*.85)
    % Save data
    [KOpks(i),KOpklocs(i)] = findpeaks(KO(:,i), 'MinPeakProminence',max(KO(:,i))*.85);
end
hold off


figure
hold on
for  i = 1:size(WT, 2)
% (2) set min value of post peak data = 0
%Truncate data from peak
Y = WT(WTpklocs(i):WTpklocs(i)+50,i);
%Set min value to 0
Y = Y- min(Y);
Y = (Y/max(Y)) *100
% [~, minloc] = min(Y);
% Y = Y(1:minloc)
X = [0:length(Y)-1]';

% (3) model negative exponential

% function is y = a*e^(bX)
% a = Y value at t = 0
% b = rate constant, expressed as reciprocal of X time units
% Half life = ln(2)/b
f = fit(X,Y,'exp1');
coeffs = coeffvalues(f);
a_WT(i) = coeffs(1);
b_WT(i) = coeffs(2);
t50_WT(i) = log(2)/b_WT(i);
plot(f,X,Y)
end
title('Negative Exponential Model fit to WT Data')
ylabel('DA nM - shifted to min = 0')
xlabel('Time in deci-seconds from peak release')
hold off

figure
hold on
for  i = 1:size(KO, 2)
% (2) set min value of post peak data = 0
%Truncate data from peak
Y = KO(KOpklocs(i):end,i);
%Set min value to 0
Y = Y- min(Y);
Y = (Y/max(Y)) *100

X = [0:length(Y)-1]';

% (3) model negative exponential

% function is y = a*e^(bX)
% a = Y value at t = 0
% b = rate constant, expressed as reciprocal of X time units
% Half life = ln(2)/b
f = fit(X,Y,'exp1');
coeffs = coeffvalues(f);
a_KO(i) = coeffs(1);
b_KO(i) = coeffs(2);
t50_KO(i) = log(2)/b_KO(i);
plot(f,X,Y)
end
title('Negative Exponential Model fit to KO Data')
ylabel('DA nM - shifted to min = 0')
xlabel('Time in deci-seconds from peak release')
hold off



