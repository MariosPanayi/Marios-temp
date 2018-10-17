%Load data
load('C:\Users\Marios\OneDrive\GluA1_Data\TB1_VariablerewardData\TB1sandersonvariablerewarddata4841fullanimalsV2.mat');
%simplify
data = TB1sandersonvariablerewarddata4841fullanimalsV2;

% Column names
% {'uid','Genotype','Mouse','Session','Channel','Time_bin','Dopamine','subject','RewardNo','nanPercent10s_post_reward','lowTTL','medTTL','highTTL','RewardTime'}


data.medTTL = data.medTTL * 2; 
data.highTTL = data.highTTL * 3;

data.RewardTTLs = data.lowTTL + data.medTTL + data.highTTL;
data.RewardSize = ones(size(data, 1), 1);

datatrials = data(:,{'uid','Genotype','Mouse','Session','Channel','subject','RewardNo'});
[G,datatrials] = findgroups(datatrials);


%make max of RewardTTLs = values in RewardSize column
equalSum = @(x,y) {x * max(y)};


equalsum_data = splitapply(equalSum, data.RewardSize, data.RewardTTLs, G);
equalsum_data = vertcat(equalsum_data{:});
data.RewardSize = equalsum_data;

%writetable(data,'C:\Users\Marios\OneDrive\GluA1_Data\TB1_VariablerewardData\TB1_VarRewardData.xlsx');


save('C:\Users\Marios\OneDrive\GluA1_Data\TB1_VariablerewardData\TB1sandersonvariablerewarddata4841fullanimalsV2.mat', 'data', 'TB1sandersonvariablerewarddata4841fullanimalsV2')
