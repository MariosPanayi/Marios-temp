
%% Load Data
load('C:\Users\mario\Documents\GitHub\Marios-temp\GLRA002_AnaesthetisedFCV\GLRA002_BaselinePreDrugData_t50Analysis.mat')

%% Plot Data
figure;
subplot(2,1,1)
hold on
for i= 1:size(KO, 2)
plot(KO(:,i));
end
title('GLRA002 KO Baseline - Individual Traces')
ylabel('nM DA')
xlabel('Time')
hold off

subplot(2,1,2)
hold on
for i= 1:size(WT, 2)
plot(WT(:,i));
end
title('GLRA002 WT Baseline - Individual Traces')
ylabel('nM DA')
xlabel('Time')
hold off

%% To analyse t50:
% (1) identify the point of peak release
% (2) set min value of post peak data = 0
% (3) model negative exponential


% (1) identify the point of peak release
% using find peaks function, find peak prominences in the data that are at
% least 85% of the max release. Make sure only a single peak is identified
% for each animal

plotfigs = 1;
if plotfigs
figure
subplot(2,1,1)
title('Peak DA - WTs')
hold on
end

for i = 1:size(WT, 2)
    %Plot
    if plotfigs
    findpeaks(WT(:,i), 'MinPeakProminence',max(WT(:,i))*.85)
    end
    % Save data
    [WTpks(i),WTpklocs(i)] = findpeaks(WT(:,i), 'MinPeakProminence',max(WT(:,i))*.85);
end
hold off

subplot(2,1,2)
title('Peak DA - KOs')
hold on
for i = 1:size(KO, 2)
    %Plot
    if plotfigs
    findpeaks(KO(:,i), 'MinPeakProminence',max(KO(:,i))*.85)
    end
    % Save data
    [KOpks(i),KOpklocs(i)] = findpeaks(KO(:,i), 'MinPeakProminence',max(KO(:,i))*.85);
end
hold off





figure
hold on
for  i = 1:size(WT, 2)
    
    
% (2) set min value of post peak data = 0
%Truncate data from peak
Y = WT(WTpklocs(i):end,i);
Y = Y- min(Y);
endpoints = 10:1:length(Y);
WT_Fit = zeros(length(endpoints),1);
for j = 1:length(endpoints)
    Ytemp = Y(1:endpoints(j));
    Ytemp = smooth(Ytemp);
    Xtemp = [0:length(Ytemp)-1]';
    [ftemp, goftemp] = fit(Xtemp,Ytemp,'exp1');
    WT_Fit(j) = goftemp.rsquare;
end

[~,MaxFitIndex] = max(WT_Fit);
% reset Y
if (WTpklocs(i)+endpoints(MaxFitIndex)) > length(WT(WTpklocs(i):end,i))
    Y = WT(WTpklocs(i):end,i);
else
Y = WT(WTpklocs(i):WTpklocs(i)+endpoints(MaxFitIndex),i);
end
%Set min value to 0
Y = Y- min(Y);
% Change metric to percentage to easily compare slopes in figure if desired
Y = (Y/max(Y)) *100;
% (3) model negative exponential
X = [0:length(Y)-1]';
% function is y = a*e^(bX)
% a = Y value at t = 0
% b = rate constant, expressed as reciprocal of X time units
% Half life = ln(2)/b
[f, gof] = fit(X,Y,'exp1');
coeffs = coeffvalues(f);
rsq_WT(i) = gof.rsquare;
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
Y = Y- min(Y);
endpoints = 10:1:length(Y);
KO_Fit = zeros(length(endpoints),1);
for j = 1:length(endpoints)
    Ytemp = Y(1:endpoints(j));
    Ytemp = smooth(Ytemp);
    Xtemp = [0:length(Ytemp)-1]';
    [ftemp, goftemp] = fit(Xtemp,Ytemp,'exp1');
    KO_Fit(j) = goftemp.rsquare;
end

[~,MaxFitIndex] = max(KO_Fit);
% reset Y
if (KOpklocs(i)+endpoints(MaxFitIndex)) > length(KO(KOpklocs(i):end,i))
    Y = KO(KOpklocs(i):end,i);
else
Y = KO(KOpklocs(i):KOpklocs(i)+endpoints(MaxFitIndex),i);
end
%Set min value to 0
Y = Y- min(Y);
% Change metric to percentage to easily compare slopes in figure if desired
Y = (Y/max(Y)) *100;
% (3) model negative exponential
X = [0:length(Y)-1]';
% function is y = a*e^(bX)
% a = Y value at t = 0
% b = rate constant, expressed as reciprocal of X time units
% Half life = ln(2)/b
[f, gof] = fit(X,Y,'exp1');
coeffs = coeffvalues(f);
rsq_KO(i) = gof.rsquare;
a_KO(i) = coeffs(1);
b_KO(i) = coeffs(2);
t50_KO(i) = log(2)/b_KO(i);
plot(f,X,Y)
end
title('Negative Exponential Model fit to KO Data')
ylabel('DA nM - shifted to min = 0')
xlabel('Time in deci-seconds from peak release')
hold off


% Make values positive
t50_WT = t50_WT*-1;
t50_KO = t50_KO*-1;

% ttest on t50 values
[h1,p1,ci1,stats1] = ttest2(t50_WT,t50_KO)

% Find poor fit values
poorFit_WT = find(rsq_WT < 0.90);
goodfit_WT = t50_WT;
if poorFit_WT
    goodfit_WT(poorFit_WT) = [];  
end

poorFit_KO = find(rsq_KO < 0.90);
goodfit_KO = t50_KO;
if poorFit_KO
    goodfit_KO(poorFit_KO) = [];  
end

% ttest on t50 values
[h2,p2,ci2,stats2] = ttest2(goodfit_WT,goodfit_KO)

