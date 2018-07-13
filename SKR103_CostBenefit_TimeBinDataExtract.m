
%Data from SKR103 Cost-Benefit study sessions - pulling out timestamped information - 'SKR103_cost_benefit_2mag.mpc'
mpc_read_data('G:\My Experiments_2015\EXPERIMENTS BEING RUN\SKR103_COSTBENEFIT STUDY\SKR103 COST BENEFIT\SKR103_costbenefit_ACQUISITION_DAY16');
%Pull out sample rat data - Number 25 happens to be the ifrst rat here
data = ans{1,1};

%PreCS period start time
PreCS_Start = data.Y(2002:2025);
%CS Period Start Time
CS_Start = data.Y(2102:2125);
%CS Reward Time
CSReward = data.Y(2202:2225);

%%
%CS MagEntry
CSMagTot = data.Z(1);
CSMagEntry = data.Y(2: CSMagTot+1);
%CS MagExit
CSMagExit = data.U(2: CSMagTot+1);

%%
%Alternative MagEntry
AltMagTot = data.S(1);
DipperMagEntry = data.Y(3002: 3000+AltMagTot+1);

%%
%5s Bins
Bins = data.E(1);
Bins5s = data.E(7000+2: 7000 + Bins + 1);
% Current Reward Probability
Bins_DipperRewardProb = data.E(1000+2: 1000 + Bins + 1);
% Dipper Reward Present
Bins_DipperReward = data.E(2000+2: 2000 + Bins + 1);
% CS Period 1, 2, 3 = PreCS in 5s blocks; 4, 5, 6 = CS period in 5s blocks, 7 = immediately post CS 5s bin when pellet delivered, 0 = ITI time
Bins_CSPeriod = data.E(3000+2: 3000 + Bins + 1);
%Trial Number
Bins_TrialNum = data.E(6000+2: 6000 + Bins + 1);
% 
% % Saving data to file
% filename = 'timeStampData.xlsx';
% xlswrite(filename, 'PreCS_Start', 'CS_Start','CSReward','CSMagEntry','CSMagExit','DipperMagEntry', 'Bins5s','Bins_DipperReward','Bins_CSPeriod','Bins_TrialNum');