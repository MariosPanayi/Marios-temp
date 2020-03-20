% Raw FCV Tarheel data folders directory and subfolder paths
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

uniqueID_inclusion = {'29_20130227_0_SAL';
'29_20130301_0_LY';
'32_20130227_1_LY';
'32_20130301_1_SAL';
'34_20130227_0_LY';
'34_20130227_1_LY';
'34_20130301_0_SAL';
'34_20130301_1_SAL';
'52_20130911_0_SAL';
'52_20130911_1_SAL';
'52_20130913_0_LY';
'52_20130913_1_LY';
'54_20140617_1_LY';
'54_20140619_1_SAL';
'55_20140617_1_SAL';
'55_20140619_1_LY';
'56_20140617_0_LY';
'56_20140617_1_LY';
'56_20140619_0_SAL';
'56_20140619_1_SAL';
'57_20140618_0_SAL';
'57_20140618_1_SAL';
'57_20140620_0_LY';
'57_20140620_1_LY';
'58_20140618_0_LY';
'58_20140618_1_LY';
'58_20140620_0_SAL';
'58_20140620_1_SAL';
'69_20141209_0_LY';
'69_20141211_0_SAL';
'71_20141212_0_SAL';
'71_20141216_0_LY';
'72_20141210_0_LY';
'72_20141210_1_LY';
'72_20141212_0_SAL';
'72_20141212_1_SAL'};

% Concatenate all data folders
for i = 1:length(folderpaths)
fulldatapaths{i} = [datadir,'\', folderpaths{i}, '\', subfolderspaths{i}, '\'];
end
fulldatapaths = fulldatapaths';

%% Read Data into Data Array - split channels into separate rows

no_of_channels = 2;
total_sessions = length(folderpaths);
for i = 1:length(folderpaths)
temp = split(folderpaths{i}, '\')';
data(i).subject = str2double(temp{1});
data(i+total_sessions).subject = str2double(temp{1});
temp = split(temp{2}, '_');
data(i).date = str2double(temp{1});
data(i).drug = temp{2};
data(i+total_sessions).date = str2double(temp{1});
data(i+total_sessions).drug = temp{2};

data(i).channel = 0;
data(i+total_sessions).channel = 1;

data(i).uniqueID = strcat(num2str(data(i).subject), '_',num2str(data(i).date), '_',num2str(data(i).channel), '_', data(i).drug);
data(i+total_sessions).uniqueID = strcat(num2str(data(i+total_sessions).subject), '_',num2str(data(i+total_sessions).date), '_',num2str(data(i+total_sessions).channel), '_', data(i+total_sessions).drug);

[data(i).TTLs, data(i).fcv_data, ~, data(i).ts] = read_whole_tarheel_session(fulldatapaths{i}, no_of_channels);
[data(i+total_sessions).TTLs, ~, data(i+total_sessions).fcv_data, data(i+total_sessions).ts] = read_whole_tarheel_session(fulldatapaths{i}, no_of_channels);
end

%Identify included sessions/channels
for i = 1:length(data)
    data(i).include = ismember(data(i).uniqueID, uniqueID_inclusion);
end
%%
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
chemo_params.cv_matrix = dlmread("C:\Users\mario\Documents\GitHub\fcv_data_processing\chemoset\cvmatrix2.txt");
chemo_params.conc_matrix = dlmread("C:\Users\mario\Documents\GitHub\fcv_data_processing\chemoset\concmatrix2.txt");
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
cut_params.ignore_repeats = [5]; %no of seconds to ignore repeats
cut_params.sample_rate = 10;
cut_params.time_align = [5 10]; %window size, [seconds before after]
cut_params.bg_pos = -0.5; %seconds relative to target_location

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Params for visualise function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%visualisation params
params.TTLnames = {'', 'RewardOn', 'RewardOff', '', '', '', 'HeadEntry', 'HeadExit', '', '', '', '', '', '', '', ''};
params.trial_exclude_list = [];%[17,23, 57, 42];
params.plot_each =  0; %plot individual trials/cut timestamps
params.scan_number = 150; %point in scan to plot if chemometrics not applied
params.apply_chemometrics = 1; %do chemometric processing, set to 0 to just average the raw fcv data
params.plot_avg = 0; %plot average trials/cut timestamps
%read in tarheel session
% Already read in above
params.fig_title = 'Title Here';



%% Process data loop
for i = 1:length(data)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Extract TTL times
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
[data(i).TTL_data.start, data(i).TTL_data.end] = extract_TTL_times(data(i).TTLs);
data(i).TTL_data.TTLs = data(i).TTLs;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cut fcv data around magazine entry
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
[data(i).cut.data, data(i).cut.points, data(i).cut.TTLs, data(i).cut.ts] = cut_fcv_data(data(i).fcv_data, data(i).TTL_data, data(i).ts, cut_params);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Background subtract data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%set bg
bg_pos = ones(length(data(i).cut.data),1);
bg_pos = bg_pos*((cut_params.time_align(1)+cut_params.bg_pos)*cut_params.sample_rate);

%bg subtract
for j = 1:length(data(i).cut.data)
    bg_params.bg_pos  = bg_pos(j);
    [data(i).cut.processed_data{j}] = process_raw_fcv_data(data(i).cut.data{j}, bg_params);

end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Apply chemometrics 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if params.apply_chemometrics
        
    %initialise chemometric variables
    model_cvs = [];
    c_predicted = [];
    residuals = [];
    [data(i).chemo.model_cvs, data(i).chemo.c_predicted, data(i).chemo.residuals.q, data(i).chemo.residuals.q_crit, data(i).chemo.residuals.q_cutoff] = ...
        fcv_chemometrics(data(i).cut.processed_data, chemo_params, data(i).cut.TTLs, data(i).cut.ts);    
end

end

% % Identifying Reward Magnitude for each trial
for i = 1:length(data)
    for j = 1:length(data(i).cut.data)
        %TTL bits ON for each reward delivery on channel 2
        %Reward magnitude is 1,2,4 pellets -> Low,Med,High
        data(i).cut.rewardMagnitude{j} = sum(diff(data(i).cut.TTLs{j}(:,2)) == 1);
        data(i).cut.trialNum{j} = j;
    end
end

% Create new folder for final data analysis
% Use C_predicted signal [row 1 = DA]
% 0.5s moving window smooth, 10Hz sampling rate so 5 ts = 0.5s
% Calculate AUC, Peak, Latency to peak within 5s of event
% AUC must be taken relative to the background DA, so first determine a
% baseline DA, 


for i = 1:length(data)
    for j = 1:length(data(i).cut.data)
        %Smooth data 0.5s filter
        data(i).analysis.traces{j} = smooth(data(i).chemo.c_predicted{1,j}(1,:),5);
        data(i).analysis.rewardMagnitude(j) = data(i).cut.rewardMagnitude{j};
        data(i).analysis.trialNum(j) =  data(i).cut.trialNum{j};
        
        %Identify 1s pre baseline and 5s post baseline relative to cut parameters
        cutbaseline = [(cut_params.time_align(1)-1)*cut_params.sample_rate+1:cut_params.time_align(1)*cut_params.sample_rate];
        cutAUC =  [cut_params.time_align(1)*cut_params.sample_rate+1:(cut_params.time_align(1)+5)*cut_params.sample_rate];
        
        data(i).analysis.BaselineAUC1s(j) = sum(data(i).analysis.traces{j}(cutbaseline));
        data(i).analysis.AUC5s(j) = sum(data(i).analysis.traces{j}(cutAUC));
        data(i).analysis.AUC5s_baselineCorrected(j) = data(i).analysis.AUC5s(j) - data(i).analysis.BaselineAUC1s(j)*5;
        
        
        %Identify Max signal in 5s and latency of this peak
        [data(i).analysis.peak5s(j), data(i).analysis.latency2peak5s(j)] = max(data(i).analysis.traces{j}(cutAUC));
            %Convert latency to seconds
        data(i).analysis.latency2peak5s(j) = data(i).analysis.latency2peak5s(j)/cut_params.sample_rate;
    
        %t50 analysis when possible
        t50plotfigs = 0;
        t50pk_threshold = 0.85;
        [data(i).analysis.t50.t50_data(j), data(i).analysis.t50.rsq_data(j), data(i).analysis.t50.datapks(j), data(i).analysis.t50.datapklocs(j), data(i).analysis.t50.peak_intercepts(j)] = aFCV_Analyze_t50(data(i).analysis.traces{j}(cutAUC), t50plotfigs, t50pk_threshold );

    end
end

%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plot data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% for i = 1:length(data)
for i = 1
    
    if data(i).include
    params.fig_title = strcat('Rat\_', data(i).uniqueID);
     
sum_colourplot = zeros(size(data(i).cut.processed_data{1}));
all_IvT = [];
for j = 1:length(data(i).cut.processed_data)
    if ~ismember(j,params.trial_exclude_list)
        if params.plot_each
            %plot each colour plot
            figure
            subplot(1,3,1)
            plot_fcvdata(data(i).cut.processed_data{j},data(i).cut.ts{j})    
            c = colorbar('eastoutside');
            ylabel(c,'Current(nA)')
            if params.apply_chemometrics
                title('Chemometric FCV data')
            else
                title('Raw FCV data')
            end

            %plot I vs T
            subplot(1,3,2)
            if params.apply_chemometrics
                plot(data(i).cut.ts{j},smooth(data(i).chemo.c_predicted{1,j}(1,:),5),'k')
                title('Chemometric I vs T');xlabel('Time(s)');ylabel('Current (nA)')
            else                
                plot(data(i).cut.ts{j},smooth(data(i).cut.processed_data{j}(params.scan_number,:),5),'k')
                title('I vs T');xlabel('Time(s)');ylabel('Current (nA)')
            end
            xlim([min(data(i).cut.ts{j}), max(data(i).cut.ts{j})]);

            %plot TTLS
            subplot(1,3,3)
            plot_TTLs(data(i).cut.TTLs{j}, data(i).cut.ts{j}, params.TTLnames)
            title('TTLs');xlabel('Time(s)');ylabel('TTLs')
            
            figtitle = sprintf('Trial number %d', j);
            suptitle(params.fig_title)
        end

        sum_colourplot = sum_colourplot+data(i).cut.processed_data{j};
        all_IvT(j,:) = data(i).chemo.c_predicted{1,j}(1,:);
    end
end

if params.plot_avg 
%final plot, avg colour plot and individual i vs t
h = figure;
subplot(1,2,1)
avg_colourplot = sum_colourplot/length(data(i).cut.processed_data);
plot_fcvdata(avg_colourplot);    
c = colorbar('eastoutside');
ylabel(c,'Current(nA)')
title('Average Colour plot')
subplot(1,2,2)
plot(all_IvT')
hold on
plot(mean(all_IvT),'k','LineWidth', 2)
title('I vs T');xlabel('Time(s)');ylabel('Current (nA)')
set(gcf, 'Position', [300, 300, 1300, 500]);

suptitle([params.fig_title]);

end
    end

end
