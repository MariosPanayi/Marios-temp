close all
clear all
tic
%% Parameters
%Move these inside data processing loop if you want to setup individual parameters for each animal/session

%list of experiment specific params
% experimentParams = readtable('C:\Users\mario\Documents\GitHub\Marios-temp\LY354740_Rat\Anaesthetized\LYAnaesthetized_params.xlsx');
experimentParams = readtable('C:\Users\mpanagi\Documents\GitHub\Marios-temp\LY354740_Rat\Anaesthetized\LYAnaesthetized_params.xlsx');

%Set number of channels
no_of_channels = 1;

%bg sub params
bg_params.filt_freq = 2000; %we found 2000Hz for 2 channel data gave a smoother CV
bg_params.sample_freq = 58820;


%chemometric variables
chemo_params.cv_matrix = dlmread('C:\Users\mpanagi\Documents\GitHub\fcv_data_processing\chemoset\cvmatrix1.txt');
chemo_params.conc_matrix = dlmread('C:\Users\mpanagi\Documents\GitHub\fcv_data_processing\chemoset\concmatrix1.txt');
chemo_params.pcs = [];
chemo_params.alpha = 0.05;
chemo_params.plotfigs = 0;

%cutting variables
cut_params.sample_rate = 10;
cut_params.target_pos = 5; %time of stimulation
cut_params.bg_pos = -0.5; %seconds relative target
cut_params.trimData = [0 20]; %crop long files between a [start end] time in seconds


%visualisation params
params.trial_exclude_list = [];%[17,23, 57, 42];
params.plot_each =  1; %plot individual trials/cut timestamps
params.scan_number = 317;
params.plot_all_IvT = 0;
params.apply_chemometrics = 1; %do chemometrics
params.fig_title = 'Amplitude Response Curve';



%% Set up data path and subfolder structure

%Directory containing all files
directory = 'E:\EmilFristedMScDBMEW\Anaesthetised data\Expt001';
%Pull out all folders within the directory, i.e. individual subjects
%find list of relevant folders starting with EXPT001 and update folders variable
folders  = getFCVfoldersAnaesthetized(directory, 'EXPT001');
folders = {folders.name};


subfolder1 = '01_baseline_pre';
subfolder2 = '02_stim_intensity';
subfolder3 = '03_stim_pulse';
subfolder4 = '04_baseline_post';
subfolder5 = '05_baseline_pre';
subfolder6 = '06_stim_intensity';
subfolder7 = '07_stim_pulse';
subfolder8 = '08_baseline_post';
%% Run individual experimental periods here 1 by 1 to ensure no issues arise! 
% subfolders_fulllist = {subfolder1,subfolder2,subfolder3,subfolder4,subfolder5,subfolder6,subfolder7,subfolder8};
subfolders_fulllist = {subfolder2,subfolder3,subfolder4,subfolder5,subfolder6,subfolder7,subfolder8};

for list = 1:length(subfolders_fulllist)

subfolders = {subfolders_fulllist{list}};

% Find relevant subfolder

for i = 1:size(folders,2)
    %Find subfolders for each animal in folders variable  
    for j = 1:size(subfolders,2)
    %Find the cell index within the subfolder that matches each of the subfolder naming conventions using regexp    
    %create a list of all the subfolders such that each {animal,
    %experimental stage} is saved
    subfolderstemp  = getFCVfoldersAnaesthetized(strcat(directory,'\',folders{i}, '\'), subfolders{j});
    subfolderstemp_name = {subfolderstemp.name};
    subfolder_paths{i,j} = subfolderstemp_name;
    end
end



%% Main Loop
%loop through each folder (subject) and each subfolder (protocol)
for i= 1:length(folders)
    %Extract subject and sessiond details from folder name
    sessionDetails = strsplit(folders{i}, '_');
    experiment{i} = sessionDetails{1};
    date{i} = sessionDetails{2};
    subject{i} = sessionDetails{3};
    drug{i} = sessionDetails{4};
    
    
    %Identify subject and update parameters accordingly
    varindex = cellfind(subject{i}, experimentParams.SubjID);
    
    cut_params.trimData = [experimentParams.TrimStart(varindex) experimentParams.TrimEnd(varindex)];
    cut_params.bg_pos = experimentParams.Baseline(varindex);
    cut_params.target_pos = experimentParams.TargetPos(varindex);
    max_end = experimentParams.PeakPeriod(varindex)*cut_params.sample_rate;
    Drug{i} = experimentParams.Drug(varindex);
    Sex{i} = experimentParams.Sex(varindex);
    CalibrationFactor{i} = experimentParams.CalibrationFactor(varindex); 
    
    for j = 1:length(subfolders)
        %Errors can occur here if the format of the datapaths is nto
        %correct and not provided as a string
        datapath = strcat(directory, '\', folders{i}, '\', subfolder_paths{i,j}, '\');
        datapath = datapath{1};
        %% Process data
        
        %Read in separate files for analysis
        [fileNames, TTLs, ch0_fcv_data, ~, ts] = read_separate_tarheel_files(datapath, no_of_channels);
        %Background subtract data
        processed_data = bg_subtract_aFCV(ch0_fcv_data, cut_params, bg_params);
        [processed_data, processed_ts] = trim_data_aFCV(processed_data, cut_params, ts);
        
        
        %% Apply chemometrics
        
        if params.apply_chemometrics
            %initialise variables
            model_cvs = [];
            c_predicted = [];
            residuals = [];
            
            %apply chemometrics
            [model_cvs, c_predicted, residuals.q, residuals.q_crit, residuals.q_cutoff] = ...
                fcv_chemometrics(processed_data, chemo_params, TTLs, processed_ts);
        end
        
        %% Plot
        %         h = plot_fcv_trials_Anaesthetized(model_cvs, processed_ts, TTLs, params, c_predicted);
        %         suptitle([params.fig_title]);
        
        
        %calculate max value of DA i.e. DA peak
        baseline = (cut_params.target_pos - cut_params.trimData(1))*cut_params.sample_rate;
        % check for peak dopamine between baseline and max_end seconds later
        % (important to avoid large values caused by post-stim drift
%         max_end = 5;

        DA_max = [];
        DA_latency = [];
        DA_AUC_baseline = [];
        DA_AUC = [];
        for k = 1: size(c_predicted, 2)
            [max_val, max_index] = max(c_predicted{k}(1,baseline:baseline+max_end),[], 2);
            DA_max(k) = max_val;
            DA_latency(k) = max_index; %N.b. this is latency from baseline in scan number
            DA_AUC_baseline(k) = sum(c_predicted{k}(1,1:baseline));
            DA_AUC(k) = sum(c_predicted{k}(1,baseline+1:baseline+baseline));
        end
        
        
        
        delimiter = '_'; %delimiter used between filename sections
        remove = 1; % Boolean, True = remove the target text from the output, False = leave target text in output
       stimFreq = [];
       stimPulses = [];
       stimStrength = [];
        for l = 1:length(fileNames)
            target = 'Hz'; %target text
            stimFreq(l) = str2double(filenameSplitter(fileNames{l},delimiter,target,remove));
            target = 'p'; %target text
            stimPulses(l) = str2double(filenameSplitter(fileNames{l},delimiter,target,remove));
            target = 'uA'; %target text
            stimStrength(l) = str2double(filenameSplitter(fileNames{l},delimiter,target,remove));
        end
        
    end
    %Save data
    data(i).experiment = experiment{i};
    data(i).subject = subject{i};
    data(i).date = date{i};
    data(i).drug = Drug{i};
    data(i).sex = Sex{i};
    data(i).calibrationFactor = CalibrationFactor{i};
    
    data(i).raw.filename = fileNames;
    data(i).raw.values = ch0_fcv_data;
    data(i).raw.ts = ts;
    data(i).raw.TTLs = TTLs;
    
    data(i).processed.filename = fileNames;
    data(i).processed.processed_data = processed_data;
    data(i).processed.ts = processed_ts;
    data(i).processed.model_cvs = model_cvs;
    data(i).processed.c_predicted = c_predicted;
    data(i).processed.residuals = residuals;
    
    
    data(i).summary.DA_max = DA_max;
    data(i).summary.DA_latency = DA_latency;
    data(i).summary.DA_AUC_baseline = DA_AUC_baseline;
    data(i).summary.DA_AUC = DA_AUC;
    
    data(i).stim_params.stimFreq = stimFreq;
    data(i).stim_params.stimPulses = stimPulses;
    data(i).stim_params.stimStrength = stimStrength;
    
end

%%
  summaryDA_max = [];
  summaryDA_latency = [];
  subjectcol = {};
for i = 1:size(data,2)

   temp_max  = groupstats([data(i).stim_params.stimPulses]', [data(i).summary.DA_max]',@mean);
   temp_latency = groupstats([data(i).stim_params.stimPulses]', [data(i).summary.DA_latency]',@mean);
  
   
   newrows = size(temp_max,1);
   oldpos =  size(subjectcol,1);
   subjectcol((oldpos+1): (oldpos+newrows),1)= {data(i).subject};
   drugcol((oldpos+1): (oldpos+newrows),1)= {data(i).drug};
   sexcol((oldpos+1): (oldpos+newrows),1)= {data(i).sex};
   calibrationcol((oldpos+1): (oldpos+newrows),1)= {data(i).calibrationFactor};
   summaryDA_max = [summaryDA_max; temp_max];
   summaryDA_latency = [summaryDA_latency; temp_latency];
end

    summary = {subjectcol drugcol sexcol calibrationcol summaryDA_max summaryDA_latency};
%    savesummary = table(subjectcol, drugcol, sexcol, calibrationcol, summaryDA_max, summaryDA_latency);
%     xlswrite('C:\Users\mpanagi\Documents\GitHub\Marios-temp\LY354740_Rat\Anaesthetized\LY354740_01_baseline_summaryDA.xlsx', summary)

    
    
    %save('F:\Marios aFCV\GLRA_002\DataAnalysis\LY354740_PulseResponse', 'data', 'summary' )
%%
%Average responses during baseline period
%data stored in data(i).processed.c_predicted


%initialisevars
avg_DA_SAL = [];
avg_DA_LY = [];
avg_DA_cal_SAL = [];
avg_DA_cal_LY = [];
for i = 1: size(data,2)
%convert data to numeric matrix, rows 1,3,5... are DA, rows 2,4,6.... are pH
temp = cell2mat(data(i).processed.c_predicted(1,:)');
%DA data picker for even rows
temp = temp(1:2:size(temp,1),:);
temp_mean = mean(temp);
avg_DA(i,:) = temp_mean;

temp_mean_cal = temp_mean/data(i).calibrationFactor;
avg_DA_cal(i,:) = temp_mean_cal;

if strcmp([data(i).drug], 'SAL');
    avg_DA_SAL = [avg_DA_SAL;  temp_mean];
    avg_DA_cal_SAL = [avg_DA_cal_SAL;  temp_mean_cal];
elseif strcmp([data(i).drug], 'LY');
    avg_DA_LY = [avg_DA_LY; temp_mean];
    avg_DA_cal_LY = [avg_DA_cal_LY; temp_mean_cal];
end
end




%%
save(strcat('C:\Users\mpanagi\Documents\GitHub\Marios-temp\LY354740_Rat\Anaesthetized\',subfolders{1}), 'data', 'summary', 'avg_DA_SAL','avg_DA_cal_SAL' , 'avg_DA_LY','avg_DA_cal_LY')

%%
%Save all the Traces for Each Trial/Animal

%initialisevars
traces  = [];
calbratedTraces = [];
drugs = [];
sex = [];
subj = [];


for i = 1: size(data,2)
%convert data to numeric matrix, rows 1,3,5... are DA, rows 2,4,6.... are pH
tempData = cell2mat(data(i).processed.c_predicted(1,:)');
%DA data picker for even rows
tempData = tempData(1:2:size(tempData,1),:);
%Save last 3 trials only [recordings vary from 5 - 7 trials depending on the animal
% tempData = tempData(end-:end,:);

%Apply calibration factor to DA data
temp_Data_Cal = tempData./data(i).calibrationFactor;

% drug
temp_drugs = cell(1, size(tempData,2));
temp_drugs(:) = data(i).drug;

% Sex
temp_sex = cell(1, size(tempData,2));
temp_sex(:) = data(i).sex;

% Subject Name
temp_subj = cell(1, size(tempData,2));
temp_subj(:) = cellstr(data(i).subject);


traces  = [traces; tempData'];
calbratedTraces = [calbratedTraces; temp_Data_Cal'];
drugs = [drugs; temp_drugs'];
sex = [sex; temp_sex'];
subj = [subj; temp_subj'];


end

ts = repmat([data(1).processed.ts{1,1}], 1, size(data,2));
savetraces = table(drugs, sex, subj, ts', traces, calbratedTraces);
writetable(savetraces,'C:\Users\mpanagi\Documents\GitHub\Marios-temp\LY354740_Rat\Anaesthetized\LYLY354740_Traces.xlsx', 'sheet', subfolders{1});
%
%% Anaylze traces for t50

plotfigs = 0;
pk_threshold = .85;
for subjnum = 1:length(data)
    t50_data = [];
    for tracenum = 1:length(data(1).processed.c_predicted)
        %aggregate all the predicted DA trials into an array and process
        %them all using t50 analysis function
    t50_data = [t50_data; data(subjnum).processed.c_predicted{tracenum}(1,:)];
    end
    t50_data = t50_data';
    [data(subjnum).decayanalysis.t50, data(subjnum).decayanalysis.rsq, data(subjnum).decayanalysis.pks, data(subjnum).decayanalysis.pklocs, data(subjnum).decayanalysis.peak_intercepts] = aFCV_Analyze_t50(t50_data, plotfigs, pk_threshold );
end

%% Save full summary data into a table
%initialisevars
DA_max  = [];
DA_latency = [];
drugs = [];
sex = [];
subj = [];
stimFreq = [];
stimPulses = [];
stimStrength = [];
stimNum = [];
DA_AUC_baseline = [];
DA_AUC = [];

t50 = [];
rsq = [];
pks = [];
pkloks = [];
pk_intercepts = [];


for i = 1: size(data,2)

% Data of interest
tempData_DAMax = data(i).summary.DA_max(1,:)';
tempData_DAlatency = data(i).summary.DA_latency(1,:)';
tempDA_AUC_baseline = data(i).summary.DA_AUC_baseline(1,:)';
tempDA_AUC = data(i).summary.DA_AUC(1,:)';

tempt50 = data(i).decayanalysis.t50(1,:)';
temprsq = data(i).decayanalysis.rsq(1,:)';
temppks = data(i).decayanalysis.pks(1,:)';
temppkloks = data(i).decayanalysis.pklocs(1,:)';
temppk_intercepts = data(i).decayanalysis.peak_intercepts(1,:)';


% Stim Params
tempstimFreq = data(i).stim_params.stimFreq(1,:)';
tempstimPulses = data(i).stim_params.stimPulses(1,:)';
tempstimStrength = data(i).stim_params.stimStrength(1,:)';

% Countup of stimulation number/order
tempstimNum = [1:size(tempData_DAMax,1)];

% drug
temp_drugs = cell(1, size(tempData_DAMax,1));
temp_drugs(:) = data(i).drug;

% Sex
temp_sex = cell(1, size(tempData_DAMax,1));
temp_sex(:) = data(i).sex;

% Subject Name
temp_subj = cell(1, size(tempData_DAMax,1));
temp_subj(:) = cellstr(data(i).subject);


DA_max = [DA_max; tempData_DAMax];
DA_latency  = [DA_latency; tempData_DAlatency];

DA_AUC_baseline = [DA_AUC_baseline; tempDA_AUC_baseline];
DA_AUC = [DA_AUC; tempDA_AUC];

t50 = [t50; tempt50];
rsq = [rsq; temprsq];
pks = [pks; temppks];
pkloks = [pkloks; temppkloks];
pk_intercepts = [pk_intercepts; temppk_intercepts];



stimFreq = [stimFreq; tempstimFreq];
stimPulses = [stimPulses; tempstimPulses];
stimStrength = [stimStrength; tempstimStrength];

drugs = [drugs; temp_drugs'];
sex = [sex; temp_sex'];
subj = [subj; temp_subj'];

stimNum = [stimNum; tempstimNum'];

end

savesummary = table(drugs, sex, subj, stimFreq, stimPulses, stimStrength,stimNum, DA_max, DA_latency, DA_AUC_baseline, DA_AUC, t50, rsq, pks, pkloks, pk_intercepts);
%writetable(savesummary,'C:\Users\mpanagi\Documents\GitHub\Marios-temp\LY354740_Rat\Anaesthetized\LY354740_summaryDA.xlsx',  'sheet', subfolders{1})

end

toc