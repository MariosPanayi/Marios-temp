clear all
tic
%% Extract Raw Data



% EventIDs array Y
% TimeStamps array 0
NosePokeEntry = 1;
NosePokeExit = 2;
Sucrose = 3;
Pellet = 4;
LeftLP = 5;
LeftLPRewarded = 6;
RightLP = 7;
RightLPRewarded = 8;
FixedTimeSucroseFlag = 9;
FixedTimePelletFlag = 10;

%% Read counterbalancing file into Table
Counterbalancing = readtable('C:\Users\mario\Documents\GitHub\Marios-temp\Anushka_MPC_dataanalysis med\Counterbalancing.csv');
%%







%%
filepath = 'C:\Users\mario\Documents\GitHub\Marios-temp\Anushka_MPC_dataanalysis med\Anushka_Habits\DATA';
folderpath = {'RI30DAY1',
'RI60DAY1',
'EDEVDAY1',
'EDEVDAY2',
'RI60DAY2',
'RI60DAY3',
'MDEVDAY1',
'MDEVDAY2',
'RI60DAY4',
'RI60DAY5',
'LDEVDAY1',
'LDEVDAY2',
'RI60DAY6',
'RI60DAY7',
'RI60DAY8',
'FDEVDAY1',
'FDEVDAY2'};
%%

dataTable = table();
timebins = [0:6000:180001];
numbins = length(timebins)-1;

for  i = 1:size(folderpath,1)

% data_raw = mpc_read_multiple_data(strcat(filepath,folderpath,filename));
filepaths =  getMedfilepaths(strcat(filepath, '\',folderpath{i}, '\'));

data_raw = {};
for k = 1:size(filepaths,1)
   temp = mpc_read_multiple_data(strcat(filepaths(k).folder, '\',filepaths(k).name));
   data_raw{k,1} = temp{1};
end



for j = 1:size(data_raw,1)
Event = data_raw{j,1}.U(1:1001);
Time = data_raw{j,1}.O(1:1001);
%
TestStage = repmat(folderpath(i),numbins,1);
subject = repmat({data_raw{j,1}.Subject}, numbins,1);
group = repmat(data_raw{j,1}.Group, numbins,1);
box = repmat(data_raw{j,1}.Box, numbins,1);
program = repmat({data_raw{j,1}.MSN}, numbins,1);
stage = repmat({data_raw{j,1}.Experiment}, numbins,1);
date = repmat({data_raw{j,1}.Start_Date}, numbins,1);

LP_RI60Rewards_bin = histcounts(Time(Event == 6), timebins)';
LP_TimeoutRewards_bin = histcounts(Time(Event == 10), timebins)';
LeftLP_bin = histcounts(Time(Event == 5 ), timebins)';
RightLP_bin = histcounts(Time(Event == 7), timebins)';
MagEntries_bin = histcounts(Time(Event == 1), timebins)';
binnumber = [1:1:numbins]';
%
temptable_subject = table(date, TestStage, subject, group, box, program, stage, SucroseRewards_bin, PelletRewards_bin, LeftLP_bin, RightLP_bin, MagEntries_bin, binnumber);
dataTable = [dataTable; temptable_subject];
% [ durationData, eventData ] = extractRDurationm([Event;Time]', 1, 2);
% data(j).magDuration = sum(durationData)/100;
end
end
%% 
toc

tic
% Counterbalancing.LEVER(pos)

%% DELETE EMPTY DATA

for i = 1:size(dataTable,1)
    try
pos(i) = find(strcmp(dataTable.subject{i}, Counterbalancing.Rat_ID));
    catch
        pos(i) = 0;
    end
end

dataTable(find(pos == 0), :) = [];
pos(find(pos == 0)) = [];

%%

dataTable.CageNum = Counterbalancing.CageNum(pos);
dataTable.Earclip = Counterbalancing.Earclip(pos);
dataTable.Genotype = Counterbalancing.Genotype(pos);
dataTable.Sex = Counterbalancing.Sex(pos);
dataTable.Tailmark = Counterbalancing.Tailmark(pos);
dataTable.LEVER = Counterbalancing.LEVER(pos);
%%
for i = 1:size(dataTable,1)
    try
        Devaluation{i} = Counterbalancing.(dataTable.TestStage{i}){pos(i)};
    catch
        Devaluation{i} = '0';
    end
end

dataTable.Devaluation = Devaluation';
%%
toc
%%
strcat(filepath, '\','Anushka_OX007_bin60s');
writetable(dataTable, strcat(filepath, '\','Anushka_OX007_bin60s.xlsx'));