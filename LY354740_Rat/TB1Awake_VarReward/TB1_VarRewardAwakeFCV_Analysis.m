% Raw FCV Tarheel data folders directory and subfolder paths
datadir = 'E:\EmilFristedMScDBMEW\Awake_data\RawData';
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

subfolderspaths = {'29_20130227_vi60\rew_alltrials_uncut',
'29_20130301_vi60\rew_alltrials_uncut',
'32_20130227_vi60\rew_alltrials_uncut',
'32_20130301_vi60\rew_alltrials_uncut',
'34_20130227_vi60\rew_alltrials_uncut',
'34_20130301_vi60\rew_alltrials_uncut',
'0052_20130911_vi60\rew_alltrials_uncut',
'0052_20130913_vi60\rew_alltrials_uncut',
'54_20140617_vi60\rew_alltrials_uncut',
'54_20140619_vi60\rew_alltrials_uncut',
'55_20140617_vi60\rew_alltrials_uncut',
'55_20140619_vi60\rew_alltrials_uncut',
'56_20140617_vi60\rew_alltrials_uncut',
'56_20140619_vi60\rew_alltrials_uncut',
'57_20140618_vi60\rew_alltrials_uncut',
'57_20140620_vi60\rew_alltrials_uncut',
'58_20140618_vi60\rew_alltrials_uncut',
'58_20140620_vi60\rew_alltrials_uncut',
'69_ 20141209_vi60\rew_alltrials_uncut',
'69_20141211_vi60\rew_alltrials_uncut',
'71_20141212_vi60\rew_alltrials_uncut',
'71_20141216_vi60\rew_alltrials_uncut',
'72_20141210_vi60\rew_alltrials_uncut',
'72_20141212_vi60\rew_alltrials_uncut'};


% Included data - Rat Num, Date, ChannelNum, Drug
inclusion = {29,	20130227,	0,	'SAL';
29,	20130301,	0,	'LY';
32,	20130227,	1,	'LY';
32,	20130301,	1,	'SAL';
34,	20130227,	0,	'LY';
34,	20130227,	1,	'LY';
34,	20130301,	0,	'SAL';
34,	20130301,	1,	'SAL';
52,	20130911,	0,	'SAL';
52,	20130911,	1,	'SAL';
52,	20130913,	0,	'LY';
52,	20130913,	1,	'LY';
54,	20140617,	1,	'LY';
54,	20140619,	1,	'SAL';
55,	20140617,	1,	'SAL';
55,	20140619,	1,	'LY';
56,	20140617,	0,	'LY';
56,	20140617,	1,	'LY';
56,	20140619,	0,	'SAL';
56,	20140619,	1,	'SAL';
57,	20140618,	0,	'SAL';
57,	20140618,	1,	'SAL';
57,	20140620,	0,	'LY';
57,	20140620,	1,	'LY';
58,	20140618,	0,	'LY';
58,	20140618,	1,	'LY';
58,	20140620,	0,	'SAL';
58,	20140620,	1,	'SAL';
69,	20141209,	0,	'LY';
69,	20141211,	0,	'SAL';
71,	20141212,	0,	'SAL';
71,	20141216,	0,	'LY';
72,	20141210,	0,	'LY';
72,	20141210,	1,	'LY';
72,	20141212,	0,	'SAL';
72,	20141212,	1,	'SAL'};


% Concatenate all data folders
for i = 1:length(folderpaths)
fulldatapaths{i} = [datadir,'\', folderpaths{i}, '\', subfolderspaths{i}, '\'];
end
fulldatapaths = fulldatapaths';

%% Read Data into Data Array

no_of_channels = 2;

for i = 1:length(folderpaths)
temp = split(folderpaths{i}, '\')';
data(i).subject = temp{1};
temp = split(temp{2}, '_');
data(i).date = temp{1};
data(i).drug = temp{2};

[data(i).TTLs, data(i).ch0_fcv_data, data(i).ch1_fcv_data, data(i).ts] = read_whole_tarheel_session(fulldatapaths{i}, no_of_channels);
end
%% Cut data around relevant TTLs
% TTLs in this task
% 2 = RewardOn
% 3 = RewardOff
% 7 = MagEntry
% 8 = MagExit

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% params for common functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%bg sub params
bg_params.filt_freq = 2000; %we found 2000Hz for 2 channel data gave a smoother CV
bg_params.sample_freq = 58820; 

%chemometric variables
chemo_params.cv_matrix = dlmread("C:\Users\mpanagi\Documents\GitHub\fcv_data_processing\chemoset\cvmatrix2.txt");
chemo_params.conc_matrix = dlmread("C:\Users\mpanagi\Documents\GitHub\fcv_data_processing\chemoset\concmatrix2.txt");
chemo_params.pcs = []; %let the function decide how many principal components to use
chemo_params.alpha = 0.05;
chemo_params.plotfigs = 0; %we've decided not to plot the chemometrics on indivudual trials, set to 1 to see the output

%cutting variables
cut_params.include.bits = []; %include target_bit
cut_params.include.window = []; %time(s) before target,time after target
cut_params.exclude.bits = [];
cut_params.exclude.window = [];
cut_params.target_bit = 2;
cut_params.target_location = 0; %0 = start, 1 = end, 0.5 = middle
cut_params.ignore_repeats = [2]; %no of seconds to ignore repeats
cut_params.sample_rate = 10;
cut_params.time_align = [10 30]; %window size, [seconds before after]
cut_params.bg_pos = -2; %seconds relative to target_location

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Params for visualise function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%visualisation params
fcv_data.TTLnames = {'', 'RewardOn', 'RewardOff', '', '', '', 'HeadEntry', 'HeadExit', '', '', '', '', '', '', '', ''};
params.trial_exclude_list = [];%[17,23, 57, 42];
params.plot_each =  0; %plot individual trials/cut timestamps
params.scan_number = 150; %point in scan to plot if chemometrics not applied
params.apply_chemometrics = 1; %do chemometric processing, set to 0 to just average the raw fcv data

%read in tarheel session
% Already read in above
params.fig_title = 'zorp RI60 Day 3 Rewarded lever press ch0';

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Extract TTL times
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
[TTL_data.start, TTL_data.end] = extract_TTL_times(fcv_data.TTLs);
TTL_data.TTLs = fcv_data.TTLs;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cut fcv data around magazine entry
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
[cut_data, cut_points, cut_TTLs, cut_ts] = cut_fcv_data(fcv_data.data, TTL_data, fcv_data.ts, cut_params);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Background subtract data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%set bg
bg_pos = ones(length(cut_data),1);
bg_pos = bg_pos*((cut_params.time_align(1)+cut_params.bg_pos)*cut_params.sample_rate);

%bg subtract
for i = 1:length(cut_data)
    bg_params.bg_pos  = bg_pos(i);
    [processed_data{i}] = process_raw_fcv_data(cut_data{i}, bg_params);

end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Apply chemometrics 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if params.apply_chemometrics
        
    %initialise chemometric variables
    model_cvs = [];
    c_predicted = [];
    residuals = [];
    [model_cvs, c_predicted, residuals.q, residuals.q_crit, residuals.q_cutoff] = ...
        fcv_chemometrics(processed_data, chemo_params, cut_TTLs, cut_ts);    
end









