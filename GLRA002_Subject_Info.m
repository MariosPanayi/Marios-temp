
name = {'GLRA50.6d'
'GLRA52.4d'
'GLRA53.5f'
'GLRA64.1c'
'GLRA64.1e'
'GLRA54.3d'
'GLRA56.2a'
'GLRA65.1a'
'GLRA62.4b'
'GLRA58.3d'
'GLRA58.3b'
'GLRA65.2a'
'GLRA58.3c'};

sex = {'Female'
'Female'
'Female'
'Female'
'Female'
'Female'
'Male'
'Male'
'Male'
'Male'
'Male'
'Male'
'Male'};

Genotype = {'WT'
'KO'
'KO'
'WT'
'WT'
'KO'
'WT'
'WT'
'KO'
'KO'
'WT'
'KO'
'WT'};

%nm/nA
calibrationfactor = [0.04072445
0.086679
0.08166935
0.025954
0.0297486
0.1057612
0.0167
0.0105
0.02465
0.0089
0.0194
0.016
0.01435];
        %% Plot
      close all
      
      for  i = 1:size(data,2)
        params.plot_each =  0; %plot individual trials/cut timestamps
        
        h = plot_fcv_trials_Anaesthetized(data(i).processed.model_cvs, data(i).processed.ts, data(i).raw.TTLs, params, data(i).processed.c_predicted);
       
        suptitle([data(i).subject, data(i).genotype, data(i).sex]); 
      end
      %%

subjects = {'GLRA50.6d';'GLRA51.6c';'GLRA64.1e';'GLRA64.1c';'GLRA52.4d';'GLRA53.5f';'GLRA54.3d';'GLRA56.2a';'GLRA62.4b';'GLRA65.1a';'GLRA58.3c';'GLRA58.3d';'GLRA58.3b';'GLRA65.2a'};

peakperiod = [3;3;6;3;3;3;3;3;3;3;3;3;3;3];
trim = [0 20;
    0 20;
    0 20;
    0 20;
    0 20;
    0 20;
    0 20;
    0 20;
    0 20;
    5 25;
    0 20;
    0 20;
    0 20;
    0 20;];

%%

experimentParams = readtable('C:\Users\mario\Documents\GitHub\Marios-temp\GLRA002_params.xlsx');
%set row names
experimentParams.Properties.RowNames = experimentParams.SubjID;

% subj = {'GLRA64.1e'};
% C = {experimentParams{:,1}};
% answer = cellfun(@(x) strfind(x,subj), C,  'UniformOutput', false )
% location = find(~cellfun(@isempty, answer{1, 1}))
% 

