close all
% \1 = TrialChange
% \2 = RewardDelivery
% \3 = MagazineEntry
% \4 = MagazineExit
% \5 = LeftLeverResponse
% \6 = LeftLeverResponseEnd
% \7 = RightLeverResponse
% \8 = RightLeverResponseEnd
% \9 = LeftNP
% \10 = LeftNPEnd
% \11 = RightNP
% \12 = RightNPEnd
% \13 = CS1On
% \14 = CS1Off
% \15 = CS2On
% \16 = CS2Off
% \17 = ITI start

%read data file
[num,txt,raw]= xlsread('C:\Users\mario\Documents\GitHub\Marios-temp\MPCTransferData.xlsx');
%NaN indices
index = find(isnan(num(:,1)));

% locate data associated between NaNs
% data = num(1:index(1)-1,:);
data = num(index(13)+1:index(14)-1,:);


% %Scatter plot of RightLP, and mag entries vs LP
figure
scatter(data(find(data(:,1)==7),2),data(find(data(:,1)==7),1));
scatter(data(find(data(:,1)==3|data(:,1)==7),2),data(find(data(:,1)==3|data(:,1)==7),1));

% %Histograms
nbins = 80;
figure%LP 
data1 = data(find(data(:,1)==7),2);
data2 = data1(2:size(data1));
lpDiff = data2-data1(1:size(data1)-1);
histogram(lpDiff,nbins);
figure
plot(data1(1:size(data1)-1),lpDiff)

figure
scatter(data1(1:size(data1)-1), data2)

figure
data3 = data(find(data(:,1)==3),2);
data4 = data(find(data(:,1)==4),2);
magDiff = data4-data3;
histogram(magDiff,nbins);
figure
plot(data3,magDiff)

figure
data5 = data(find(data(:,1)==2),2);
data6 = data5(2:size(data5));
rewardDiff = data6-data5(1:size(data5)-1);
% histogram(rewardDiff,nbins);

%trials10 seconds either side of a reward

rewardTime = data(find(data(:,1)==2),2);
trialStart = rewardTime-10;
trialEnd = rewardTime+10;
numTrials = length(rewardTime);
i = 3; %trialnumber
trial = data(data(:,2) >= trialStart(i) & data(:,2) <= trialEnd(i),:);
figure
scatter(trial(:,2) - rewardTime(i),trial(:,1))


%first response post reward [not indicating what teh response is...]
figure
nbins = 10;
postReward = data(find(data(:,1)==2)+1,:);
histogram(postReward(:,2)-rewardTime,nbins);

%sequences from first LP to first Mag entry

