
%% Raw FCV Tarheel data folders directory and subfolder paths
datadir = 'F:\EmilFristedMScDBMEW\Awake_data\RawData';
folderpaths = {'29\20130227_SAL',
'29\20130301_LY',
'32\20130227_LY',
'32\20130301_SAL',
'34\20130227_LY',
'34\20130301_SAL',
'52\20130911_SAL',
'52\20130913_LY',
'54\20140617_LY',
'54\20140619_SAL',
'55\20140617_SAL',
'55\20140619_LY',
'56\20140617_LY',
'56\20140619_SAL',
'57\20140618_SAL',
'57\20140620_LY',
'58\20140618_LY',
'58\20140620_SAL',
'69\20141209_LY',
'69\20141211_SAL',
'71\20141212_SAL',
'71\20141216_LY',
'72\20141210_LY',
'72\20141212_SAL'};

% Concatenate all data folders
for i = 1:length(folderpaths)
subfolderpaths{i} = [datadir,'\', folderpaths{i}, '\'];
end

for i = 1:length(subfolderpaths)
    [tempfile] = getFCVfilepaths(subfolderpaths{i});
    fulldatapaths(i).folder = subfolderpaths{i};
    for j = 1:length(tempfile)
        fulldatapaths(i).files{j} = tempfile(j).name;
    end
end
%%
% CV template data
cvmatch = load('C:\Users\mario\Documents\GitHub\CV_match\Chemometrics\cv_match');
cv_template = cvmatch.cv_match(:,1:7);


%params
params.filt_freq = 2000; %we found 2000Hz for 2 channel data gave a smoother CV
params.sample_freq = 58820;
params.bg_size = 10;
params.prog_bar = 0;
no_of_channels = 2; %should be metadata
visualise_matches = 0;

threshold.cons = 0.75;
threshold.lib = 0.7;
threshold.smoothing = 5;



%%
tic

parfor i = 1: length(fulldatapaths)
    for j = 1:length(fulldatapaths(i).files)
[fulldatapaths(i).data(j).fcv_header, fulldatapaths(i).data(j).ch0_fcv_data, fulldatapaths(i).data(j).ch1_fcv_data] = tarheel_read([fulldatapaths(i).folder fulldatapaths(i).files{j}],no_of_channels);
    end
end
toc
%% The long one!
% regular for loop 2631.927290 seconds
% parfor loop 2814.683017 seconds
tic
parfor i = 1: length(fulldatapaths)
    
    for j = 1:length(fulldatapaths(i).data)
        currentpos = [i,j];
        try
            [fulldatapaths(i).cvmatch(j).all_roh0,fulldatapaths(i).cvmatch(j).all_bg_scan0,~] = optimised_auto_cv_match(fulldatapaths(i).data(j).ch0_fcv_data, params, cv_template);
        catch
        end
        
        try
            [fulldatapaths(i).cvmatch(j).all_roh1,fulldatapaths(i).cvmatch(j).all_bg_scan1,~] = optimised_auto_cv_match(fulldatapaths(i).data(j).ch1_fcv_data, params, cv_template);
        catch
        end
        
    end
end
toc
%%
tic
for i = 1: length(fulldatapaths)
    for j = 1:length(fulldatapaths(i).data)
    try
[fulldatapaths(i).findDA(j).ch0_da_instance, fulldatapaths(i).findDA(j).ch0_da_bg_scan, fulldatapaths(i).findDA(j).ch0_match_matrix] = ...
find_dopamine_instances(fulldatapaths(i).cvmatch(j).all_roh0,fulldatapaths(i).cvmatch(j).all_bg_scan0, threshold, visualise_matches);
    catch
    end
    try
[fulldatapaths(i).findDA(j).ch1_da_instance, fulldatapaths(i).findDA(j).ch1_da_bg_scan, fulldatapaths(i).findDA(j).ch1_match_matrix] = ...
find_dopamine_instances(fulldatapaths(i).cvmatch(j).all_roh1, fulldatapaths(i).cvmatch(j).all_bg_scan1, threshold, visualise_matches);
    catch
    end
    end
end
toc




%%
% 
% Optional Save data - only do once if possible and just load saved data
% from .mat file

savefolder = 'C:\Users\mario\Documents\GitHub\Marios-temp\LY354740_Rat\AwakeBehaviour';
filename = 'LY354740_Rat_CVMatch_data.mat';
% save([savefolder, '\',filename], '-v7.3');

load([savefolder, '\',filename])


%% Quickly add up and identify which channels pass the CV matching test

for i = 1:length(fulldatapaths)
   for j = 1:length(fulldatapaths(i).findDA)
      fulldatapaths(i).findDA(j).CVMatched_ch0 =  ~isempty(fulldatapaths(i).findDA(j).ch0_da_instance);
            fulldatapaths(i).findDA(j).CVMatched_ch1 =  ~isempty(fulldatapaths(i).findDA(j).ch1_da_instance);
   end
   try
   fulldatapaths(i).ch0_CVmatches = sum([fulldatapaths(i).findDA.CVMatched_ch0]);
   catch
          fulldatapaths(i).ch0_CVmatches = 0;

   end
   try
   fulldatapaths(i).ch1_CVmatches = sum([fulldatapaths(i).findDA.CVMatched_ch1]);
   catch
                 fulldatapaths(i).ch1_CVmatches = 0;
   end
end
%%

