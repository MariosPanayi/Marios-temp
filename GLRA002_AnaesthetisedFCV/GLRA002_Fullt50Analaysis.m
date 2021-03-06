% GLRA002_Fit 250 to all traces
clear all
%% 01Baseline1

DataFilenames = {'F:\Marios aFCV\GLRA_002\DataAnalysis\GLRA002_01BaselineData.mat',
'F:\Marios aFCV\GLRA_002\DataAnalysis\GLRA002_02StimResponse.mat',
'F:\Marios aFCV\GLRA_002\DataAnalysis\GLRA002_03PulseResponse.mat',
'F:\Marios aFCV\GLRA_002\DataAnalysis\GLRA002_04BaselineData.mat',
'F:\Marios aFCV\GLRA_002\DataAnalysis\GLRA002_05BaselinePreDrugData.mat',
'F:\Marios aFCV\GLRA_002\DataAnalysis\GLRA002_06DrugPeriodData.mat'};

SheetSaveName = {'01Baseline',
'02StimResponse',
'03PulseResponse',
'04Baseline',
'05BaselinePreDrug',
'06DrugPeriod'};

%for period = 1:size(DataFilenames,1)
for period = 1
load(DataFilenames{period});

experimentParams = readtable('F:\Marios aFCV\GLRA_002\DataAnalysis\GLRA002_params.xlsx');
tempData = [];


%cutting variables
cut_params.sample_rate = 10;
cut_params.target_pos = 5; %time of stimulation
cut_params.bg_pos = -0.5; %seconds relative target
cut_params.trimData = [0 20]; %crop long files between a [start end] time in seconds



for i = 1: size(data,2)

     
    %Identify subject and update parameters accordingly
    varindex = cellfind(data(i).subject, experimentParams.SubjID);
    
    cut_params.trimData = [experimentParams.TrimStart(varindex) experimentParams.TrimEnd(varindex)];
    cut_params.bg_pos = experimentParams.Baseline(varindex);
    cut_params.target_pos = experimentParams.TargetPos(varindex);
    max_end = experimentParams.PeakPeriod(varindex);
    
    baseline = (cut_params.target_pos - cut_params.trimData(1))*cut_params.sample_rate;
    peaklocation = data(i).summary.DA_latency + baseline;
    
    
    
    
    
    %convert data to numeric matrix, rows 1,3,5... are DA, rows 2,4,6.... are pH
    tempData = cell2mat(data(i).processed.c_predicted(1,:)');
    %DA data picker for even rows
    tempData = tempData(1:2:size(tempData,1),:);
    %Save all trials
    tempData = tempData(1:end,:);
    
    
    %Apply calibration factor to DA data
    temp_Data_Cal = tempData./data(i).calibrationFactor;
    
    for j = 1:size(temp_Data_Cal,1)
        % find t50 and save output
        [t50, rsq, a, b, Y, pklocs, MaxFitIndex] = t50Find(temp_Data_Cal(j,:), peaklocation(j));
        
        data(i).T50Analysis.t50(j) = t50;
        data(i).T50Analysis.rsq(j) = rsq;
        data(i).T50Analysis.a(j) = a;
        data(i).T50Analysis.b(j) = b;

        data(i).T50Analysis.pklocs(j) = pklocs;
        data(i).T50Analysis.MaxFitIndex(j) = MaxFitIndex;
    end
    tempData = [];
end

%% Save data into Long fromat for Excel





%Save all the Traces for Each Trial/Animal

%initialisevars
t50 = [];
rsq = [];
pklocs = [];
MaxFitIndex = [];
DaMax = [];
DALatency = [];
StimFreq = [];
StimPulse = [];
StimStrength = [];
CalibrationFactor = [];
geno = [];
sex = [];
subj = [];

for i = 1: size(data,2)
temp_t50 = data(i).T50Analysis.t50;
temp_rsq = data(i).T50Analysis.rsq;
temp_pklocs = data(i).T50Analysis.pklocs;
temp_MaxFitindex = data(i).T50Analysis.MaxFitIndex;
temp_DAMax = data(i).summary.DA_max;
temp_DALatency = data(i).summary.DA_latency;
temp_stimFreq = data(i).stim_params.stimFreq;
temp_StimPulse = data(i).stim_params.stimPulses;
temp_StimStrength = data(i).stim_params.stimStrength;

%Apply calibration factor to DA data
temp_Data_Cal = zeros(size(temp_t50));
temp_Data_Cal(:) = data(i).calibrationFactor;

% Genotype
temp_geno = cell(size(temp_t50));
temp_geno(:) = data(i).genotype;

% Sex
temp_sex = cell( size(temp_t50));
temp_sex(:) = data(i).sex;

% Subject Name
temp_subj = cell(size(temp_t50));
temp_subj(:) = cellstr(data(i).subject);




%
t50 = [t50; temp_t50'];
rsq = [rsq; temp_rsq'];
pklocs = [pklocs; temp_pklocs'];
MaxFitIndex = [MaxFitIndex; temp_MaxFitindex'];
DaMax = [DaMax; temp_DAMax'];
DALatency = [DALatency; temp_DALatency'];
StimFreq = [StimFreq; temp_stimFreq'];
StimPulse = [StimPulse; temp_StimPulse'];
StimStrength = [StimStrength; temp_StimStrength'];
CalibrationFactor = [CalibrationFactor;temp_Data_Cal'];

geno = [geno; temp_geno'];
sex = [sex; temp_sex'];
subj = [subj; temp_subj'];


end


saveData1 = table(geno, sex, subj, CalibrationFactor, StimStrength, StimPulse,StimFreq,DALatency,DaMax, MaxFitIndex, pklocs, rsq, t50);



writetable(saveData1,'C:\Users\mpanagi\Documents\GitHub\Marios-temp\GLRA002_AnaesthetisedFCV\GLRA002_t50Modeling1.xlsx', 'sheet', SheetSaveName{period});

clearvars -except DataFilenames SheetSaveName period
end

