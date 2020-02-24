%% It is best to have all your videos in a single folder (since this is the format you need to keep your videos in for the Deep Lab Cut analysis
% a list of the .avi files in the specified folder is accessed using dir()
videofolder = 'K:\MK001_MK002_VideosCombinedforAnalysis';
filelist = dir(strcat(videofolder, '\*.avi'));

%% Process the average luminance of the videos - this can be used to align to light based events in the box
% read in filename
% Option to find all of a file type in a particular folder
% videofolder = 'E:\MK001_MK002_VideosCombinedforAnalysis';
% filelist = dir(strcat(videofolder, '\*.avi'));

tic
for i = 1:size(filelist,1)
    % this can take a while so display progress in command window
    totalvids = size(filelist,1);
    fprintf('Video number %d out of %d is being processed. \n', i, totalvids)
    
    %read in video as a video object
    vidObj = VideoReader(strcat(filelist(i).folder, '\', filelist(i).name));
    totalframes = vidObj.NumFrames;
    avg_luminance = zeros(totalframes,1);
    for index = 1: totalframes
        %average the luminance for each frame
        s.cdata = readFrame(vidObj);
        avg_luminance(index) = mean(mean(rgb2gray(s.cdata)));
    end
    %Save processed data for each video
    videoproc(i).filename = strcat(filelist(i).folder, '\', filelist(i).name);
    videoproc(i).avg_luminance = avg_luminance;
    
end
toc

tic
parfor i = 1:size(filelist,1)
    vidObj = VideoReader(videoproc(i).filename);
    videoproc(i).videproperites.Duration = vidObj.Duration;
    videoproc(i).videproperites.Name = vidObj.Name;
    videoproc(i).videproperites.Path = vidObj.Path;
    videoproc(i).videproperites.BitsPerPixel = vidObj.BitsPerPixel;
    videoproc(i).videproperites.FrameRate = vidObj.FrameRate;
    videoproc(i).videproperites.Height = vidObj.Height;
    videoproc(i).videproperites.NumFrames = vidObj.NumFrames;
    videoproc(i).videproperites.VideoFormat = vidObj.VideoFormat;
    videoproc(i).videproperites.NumFrames = vidObj.NumFrames;
    videoproc(i).videproperites.Width = vidObj.Width;
    videoproc(i).videproperites.Tag = vidObj.Tag;
    videoproc(i).videproperites.UserData = vidObj.UserData;
    videoproc(i).videproperites.CurrentTime = vidObj.CurrentTime;
end
toc

%% Save the processed video data and filelists to a .mat file
tic
save('E:\MK001_MK002_VideosCombinedforAnalysis\MK001_MK002_VideoProcessing.mat', 'videoproc', 'filelist')
toc
%% Due to encoding methods, it is necessary to process videos with the
% ffmpeg codec to convert putative frame numbers into time stamps (ts)
% Function is available here: https://uk.mathworks.com/matlabcentral/fileexchange/61235-video-frame-time-stamps
% however you need to install the codec first from here: https://www.ffmpeg.org/
% N.B. you have to provide the filepath to ffmpeg.exe in the function

ffmpegpath = 'C:\Users\mpanagi\Downloads\ffmpeg-20190805-5ac28e9-win64-static\ffmpeg-20190805-5ac28e9-win64-static\bin';

tic
for i = 1:size(filelist,1)
    % this can take a while so display progress in command window
    totalvids = size(filelist,1);
    fprintf('Video number %d out of %d is being processed. \n', i, totalvids)
    ts = videoframets(ffmpegpath, strcat(filelist(i).folder, '\', filelist(i).name));
    %Save processed data for each video
    videoproc(i).ts = ts;
end
toc



%% Process avg_luminance for key start and end points of light related events in your videos.
% This can be done by usinf the findpeaks function to find a specified
% number of peaks in luminance associated with light events
% Option to load in saved data from earlier analysis:

% Unfortunately you can't just rely on this automated process without
% visually inspecting the results to make sure it isn't acciedntally
% picking up other noise in some videos. Best practice is to plot and check
% a range of options to filter the videos.


% Videos 14 and 32 have issues with only detecting 8 peaks in this window,
% so 9 peaks have been detected for all mice and additional pruning of the
% data is required post

% Step 1 apply a uniform approach to finding Houselight onset
for i = 1:size(filelist,1)
    % peaks and locations of these peaks
    [pks_temp, locs_temp] = findpeaks(videoproc(i).avg_luminance(50000:end-10000),'MinPeakHeight', 140, 'MinPeakDistance',10,'SortStr','descend','NPeaks', 8);
    [locs_temp, I] = sort(locs_temp);
    pks_temp = pks_temp(I);
    videoproc(i).pks = pks_temp;
    videoproc(i).locs = locs_temp + 50000;
end

% Step2 apply specific solutions for 2 videos that don't work well.
% Note that video 3 has only 7 peaks and it appears that the 4th houselight
% onset is not visible in the luminance signal - not an issue if alignment
% to Med events is based on the first stimulus onset :)
for i = [14,32]
    [pks_temp, locs_temp] = findpeaks(videoproc(i).avg_luminance(50000:end-10000),'MinPeakHeight', 140, 'MinPeakDistance',10,'SortStr','descend','NPeaks', 9);
    
    [locs_temp, I] = sort(locs_temp);
    pks_temp = pks_temp(I);
    videoproc(i).pks = pks_temp(1:8);
    videoproc(i).locs = locs_temp(1:8) + 50000;
end



% to find the inflection point of when the light actually turns on use the
% following code that looks at the avg_luminance 1s before and 1s after the
% peak signal, then finds the peak of the derivative. N.B. using a
% derivative for the initial detection doesn't work well enough.
for i = 1:size(filelist,1)
    for index = 1:size(videoproc(i).locs,1)
        [~, locs_temp] = findpeaks(diff(videoproc(i).avg_luminance(videoproc(i).locs(index)-10:videoproc(i).locs(index) + 10)), 'SortStr','descend','Npeaks',1);
        videoproc(i).HLonset(index) = videoproc(i).locs(index) - 10 + locs_temp + 1;
    end
end

% figure out time between HL onset use this for a sanity check that the ISI
% values are correct. Compare accuracy of peak vs putative onset of peak in
% HLonset
for i = 1:size(filelist,1)
    videoproc(i).HLts = videoproc(i).ts(videoproc(i).HLonset);
    videoproc(i).HLts_diffs = diff(videoproc(i).HLts);
    videoproc(i).locsts = videoproc(i).ts(videoproc(i).locs);
    videoproc(i).locsts_diffs = diff(videoproc(i).locsts);
end



%% Process filenames for matching video to Med data
for i = 1:size(filelist,1)
    videoproc(i).sessionNum = videoproc(i).filename(59);
    videoproc(i).mouseID = videoproc(i).filename(47);
    
    %Add in expt run number for first 6 mice
    if sum(strcmp(videoproc(i).mouseID, {'A', 'B', 'C', 'D', 'E', 'F'}))
        videoproc(i).exptID = 'MK001';
    else
        videoproc(i).exptID = 'MK002';
    end
end


%% Load Med data to sync behaviour with video

folderpath = 'E:\MK001_MK002_VideosCombinedforAnalysis\MedData\';
tic
parfor i = 1:size(filelist,1)
    medfilename = strcat(folderpath, videoproc(i).exptID, '_', videoproc(i).mouseID, '_Sanderson_Test',videoproc(i).sessionNum);
    temp = mpc_read_multiple_data(medfilename);
    med(i).data = temp{1,1};
    med(i).filename = medfilename;
    med(i).exptID = videoproc(i).exptID;
    med(i).mouseID =  videoproc(i).mouseID;
    med(i).sessionNum =  videoproc(i).sessionNum;
end
toc


%% Med program event IDs and timestamps for videoproc(i).med.A = eventID videoproc(i).med.A = eventtimes
eventNum = [1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9,
    10,
    11,
    12,
    13,
    14,
    15,
    16,
    17,
    18];

eventID = {'TrialChange',
    'RewardDelivery',
    'RewardAvailable',
    'MagazineEntry',
    'MagazineExit',
    'HouseLightOn',
    'HouseLightOff',
    'LEDOn',
    'LEDOff',
    'FanOn',
    'FanOff',
    'SirenOn',
    'SirenOff',
    'WhiteNoiseOn',
    'WhiteNoiseOff',
    'IRLED_Sync',
    'Sucrose',
    'Pellet'};

% Trial List H = House, F = Flash [1 = HH, 2 = HF, 3 = FH, 4 = FF]
% med(i).data.M = trial order

%% Find the first Houselight onset time in the Med data and align the first HL onset in the video file
for i = 1:size(filelist,1)
    videoproc(i).med_HLtimes = med(i).data.B(med(i).data.A == 6);
    videoproc(i).videoMedstart = videoproc(i).HLts(1) - videoproc(i).med_HLtimes(1);
end
%% Extract all the DLC processed data and save to data struct

tic
parfor i = 1:size(videoproc,2)
    DLC_folder = 'E:\MK001_MK002_VideosCombinedforAnalysis\DLC';
    DLC_fileextension = 'DLC_resnet50_MK001_SandersonNov1shuffle1_1030000.csv';
    DLC_filepath = strcat(DLC_folder, '\', videoproc(i).filename(42:59),DLC_fileextension);
    [data_temp, bodyparts_temp] = DLC_RawRead(DLC_filepath);
    DLC(i).data = data_temp;
    DLC(i).bodyparts = bodyparts_temp;
    DLC(i).exptID = videoproc(i).exptID;
    DLC(i).mouseID =  videoproc(i).mouseID;
    DLC(i).sessionNum =  videoproc(i).sessionNum;
end
toc
%% Identify points in Med session to focus video analysis
% Trial changes are stored in med(i).data.A == 1
% Houselight is stored in med(i).data.A == 6
% Flashing panel lights are stored in med(i).data.A == 8
% trial identities are stored in order in med(i).data.M
% Trial List H = House, F = Flash [1 = HH, 2 = HF, 3 = FH, 4 = FF]

% 1. Identify trial start times
trialNum = [1:8];
trialNum = repmat(trialNum,2,1);
trialNum = trialNum(:)';
for i = 1:size(med,2)
    
    trialList = repmat(med(i).data.M,2,1);
    trialList = trialList(:)';
    index = find(med(i).data.A == 6 | med(i).data.A == 8);
    % med(i).stimonset has 4 rows [trial number, trial identity, stimulus identity, stimulus time]
    med(i).stimonset = [trialNum;trialList;med(i).data.A(index);med(i).data.B(index)];
    % start of the first and 2nd stim in each trial to cut around DLC analysis
    med(i).dlcStim1 = med(i).stimonset(4,[1:2:16]);
    med(i).dlcStim2 = med(i).stimonset(4,[2:2:16]);
end

%% DLC analysis

% 1. establish fixed points in box.
% These are the 4 corners of the box and the 3 points defining the magazine
% {'frontleft','frontright','backleft','backright','magleft','magright','magcentre'}
% To do this filter out x.y co-ordinates with low tracking confidence then
% use the median of these points for the most stable estimate of this point
%
% Use the following syntax to access specific values for each point
% nose(x), nose(y), nose(confidence)
x = 1;
y = 2;
confidence = 3;
% Body parts
nose = [2:4];
head1 = [5:7];
head2 = [8:10];
lear = [11:13];
rear = [14:16];
mid1 = [17:19];
mid2 = [20:22];
tail = [23:25];
% Box parts
frontleft = [26:28];
frontright = [29:31];
backleft = [32:34];
backright = [35:37];
magleft = [38:40];
magright = [41:43];
magcentre = [44:46];
%
for i = 1:size(DLC,2)
    %
    rows = find(DLC(i).data(:,frontleft(confidence)) == 1);
    DLC(i).boxparts.frontleft(x) = median(DLC(i).data(rows,frontleft(x)));
    DLC(i).boxparts.frontleft(y) = median(DLC(i).data(rows,frontleft(y)));
    %
    rows = find(DLC(i).data(:,frontright(confidence)) == 1);
    DLC(i).boxparts.frontright(x) = median(DLC(i).data(rows,frontright(x)));
    DLC(i).boxparts.frontright(y) = median(DLC(i).data(rows,frontright(y)));
    %
    rows = find(DLC(i).data(:,backleft(confidence)) == 1);
    DLC(i).boxparts.backleft(x) =  median(DLC(i).data(rows,backleft(x)));
    DLC(i).boxparts.backleft(y) =  median(DLC(i).data(rows,backleft(y)));
    %
    rows = find(DLC(i).data(:,backright(confidence)) == 1);
    DLC(i).boxparts.backright(x) =  median(DLC(i).data(rows,backright(x)));
    DLC(i).boxparts.backright(y) =  median(DLC(i).data(rows,backright(y)));
    %
    rows = find(DLC(i).data(:,magleft(confidence)) == 1);
    DLC(i).boxparts.magleft(x) =  median(DLC(i).data(rows,magleft(x)));
    DLC(i).boxparts.magleft(y) =   median(DLC(i).data(rows,magleft(y)));
    %
    rows = find(DLC(i).data(:,magright(confidence)) == 1);
    DLC(i).boxparts.magright(x) =  median(DLC(i).data(rows,magright(x)));
    DLC(i).boxparts.magright(y) =  median(DLC(i).data(rows,magright(y)));
    %
    rows = find(DLC(i).data(:,magcentre(confidence)) == 1);
    DLC(i).boxparts.magcentre(x) =   median(DLC(i).data(rows,magcentre(x)));
    DLC(i).boxparts.magcentre(y) =   median(DLC(i).data(rows,magcentre(y)));
    
end
%% 2. DLC analysis -
%  a. Cut data around trial types
%  b. Filter all body parts for confidence that is less than 1 and interpolate


% First identify the points in the sesion you want to analyze and convert
% them into ts values i.e. convert Med associates session times into video
% time
for i = 1:size(DLC,2)
    DLC(i).stim1 = videoproc(i).videoMedstart + med(i).dlcStim1;
    DLC(i).stim2 = videoproc(i).videoMedstart + med(i).dlcStim2;
    DLC(i).trialstart = DLC(i).stim1 - 30;
    DLC(i).trialend = DLC(i).stim2 + 40;
end

% rowstart = find(videoproc(i).ts == DLC(i).trialstart(k));
% rowend = find(videoproc(i).ts == DLC(i).trialend(k));
% %plot(DLC(i).data(rowstart:rowend, head1(x)),DLC(i).data(rowstart:rowend, head1(y)))

% Data filtering criterion for confidence level. Used below in
% DLC_interpolateLowConfidence()
criterion = 1;

for i = 1:size(DLC,2)
    for k = 1:size(DLC(i).stim1,2)
        %Identify the start and end rows of the trial in terms of "frame number"
        rowstart = min(find(videoproc(i).ts >= DLC(i).trialstart(k)));
        rowend = min(find(videoproc(i).ts >= DLC(i).trialend(k)));
        % Copy data for each trial
        DLC(i).trials(k).data = DLC(i).data(rowstart:rowend,:);
        %Filter the data with a linear interpolation for any trackign that
        %is below the confidence threshold
        DLC(i).trials(k).filtered = DLC_interpolateLowConfidence(DLC(i).trials(k).data, criterion);
        %Save corresponding ts during video for the trial period
        DLC(i).trials(k).ts = videoproc(i).ts(rowstart:rowend);
        
    end
end
%% Calculate a rectangle around the magazine, then calculate when the head (nose | head1 | head2) are inside these co-ordinates
for i = 1:size(DLC,2)
    [DLC(i).magCoord.bottomleft, DLC(i).magCoord.bottomright, DLC(i).magCoord.topleft, DLC(i).magCoord.topright] = triangletorectangle(DLC(i).boxparts.magleft, DLC(i).boxparts.magright, DLC(i).boxparts.magcentre);
end

% Determine if a bodypart is inside/on the magazine
for i = 1:size(DLC,2)
    magazine_xcoord = [DLC(i).magCoord.bottomleft(x), DLC(i).magCoord.bottomright(x), DLC(i).magCoord.topright(x), DLC(i).magCoord.topleft(x)];
    magazine_ycoord = [DLC(i).magCoord.bottomleft(y), DLC(i).magCoord.bottomright(y), DLC(i).magCoord.topright(y), DLC(i).magCoord.topleft(y)];
    for k = 1:size(DLC(i).trials,2)
        DLC(i).trials(k).inmag = DLC_bodypartsinpolygon(DLC(i).trials(k).filtered, magazine_xcoord, magazine_ycoord);
        DLC(i).trials(k).tsDuration = diff(DLC(i).trials(k).ts);
    end
end


% Calculate distance travelled  by each body part
for i = 1:size(DLC,2)
    for k = 1:size(DLC(i).trials,2)
        DLC(i).trials(k).distance = DLC_distancetravelled(DLC(i).trials(k).filtered);
        DLC(i).trials(k).distancefromMag = DLC_distancefrom(DLC(i).trials(k).filtered, DLC(i).boxparts.magcentre);
    end
end

% Body parts in a list in the order that DLC is using
bodyparts = {'nose','head1','head2','lear','rear','mid1','mid2','tail','frontleft','frontright','backleft','backright','magleft','magright','magcentre'};

%% Create a new datastructure with the 1s, 5s, and 10s binned data and identifying information
tic
for i = 1:size(DLC,2)
    for k = 1:size(DLC(i).trials, 2)
        
        %Time within each trial (s) - S1 starts at 30 and S2 starts at 70
        %Timebins of varying lengths also created as variables for
        %summarising data later
        bin_02s
        bin_1s = ceil(DLC(i).trials(k).ts(2:end)- DLC(i).trials(k).ts(1));
        bin_5s = ceil(bin_1s/5);
        bin_10s = ceil(bin_1s/10);
        
        %Time the animal's Nose spends in the magazine
        % N.b. (2:end-1) is used for the data to align with the time
        % between frames tsDuration which is a difference score on a vector
        % of the same length, so n-1 total length
        headpart_inmag = [DLC(i).trials(k).tsDuration .* (sum(DLC(i).trials(k).inmag(2:end,1:3),2) > 1)];
        dur_inmag = [DLC(i).trials(k).tsDuration .* DLC(i).trials(k).inmag(2:end,:)];
        
        %Use accumarray to collect the data in time bins N.B. column 1 of
        %dur_inmag is the nose :)
        %
        magdur_1sBins = accumarray(bin_1s,headpart_inmag(:,1), [],@sum);
        magdur_5sBins = accumarray(bin_5s,headpart_inmag(:,1), [],@sum);
        magdur_10sBins = accumarray(bin_10s,headpart_inmag(:,1), [],@sum);
        
        % distance travelled
        nosedist_1sBins = accumarray(bin_1s,DLC(i).trials(k).distancefromMag(2:end,1), [],@sum);
        nosedist_5sBins = accumarray(bin_5s,DLC(i).trials(k).distancefromMag(2:end,1), [],@sum);
        nosedist_10sBins = accumarray(bin_10s,DLC(i).trials(k).distancefromMag(2:end,1), [],@sum);
        
        %CreateColumns to repeat same identifying information i.e. prepare
        %for long format data
        
        %trial type - H = House, F = Flash [1 = HH, 2 = HF, 3 = FH, 4 = FF]
        size_1s = size(magdur_1sBins);
        df.bin1s(i).trials(k).trialID = repmat(med(i).data.M(k), size_1s);
        df.bin1s(i).trials(k).trialNum = repmat(k, size_1s);
        df.bin1s(i).trials(k).bins = unique(bin_1s, 'stable');
        df.bin1s(i).trials(k).magduration = magdur_1sBins;
        df.bin1s(i).trials(k).nosedistance = nosedist_1sBins;
        df.bin1s(i).trials(k).subject = repmat({med(i).data.Subject}, size_1s);
        df.bin1s(i).trials(k).testnumber = repmat({med(i).sessionNum}, size_1s);
        df.bin1s(i).trials(k).drug = repmat({med(i).data.Group}, size_1s);
        df.bin1s(i).trials(k).date = repmat({med(i).data.Start_Date}, size_1s);
        df.bin1s(i).trials(k).box = repmat(med(i).data.Box, size_1s);
        df.bin1s(i).trials(k).program = repmat({med(i).data.MSN}, size_1s);
        df.bin1s(i).trials(k).cohort = repmat({med(i).data.Experiment}, size_1s);
        
        
        %trial type - H = House, F = Flash [1 = HH, 2 = HF, 3 = FH, 4 = FF]
        size_5s = size(magdur_5sBins);
        df.bin5s(i).trials(k).trialID = repmat(med(i).data.M(k), size_5s);
        df.bin5s(i).trials(k).trialNum = repmat(k, size_5s);
        df.bin5s(i).trials(k).bins = unique(bin_5s, 'stable');
        df.bin5s(i).trials(k).magduration = magdur_5sBins;
        df.bin5s(i).trials(k).nosedistance = nosedist_5sBins;
        df.bin5s(i).trials(k).subject = repmat({med(i).data.Subject}, size_5s);
        df.bin5s(i).trials(k).testnumber = repmat({med(i).sessionNum}, size_5s);
        df.bin5s(i).trials(k).drug = repmat({med(i).data.Group}, size_5s);
        df.bin5s(i).trials(k).date = repmat({med(i).data.Start_Date}, size_5s);
        df.bin5s(i).trials(k).box = repmat(med(i).data.Box, size_5s);
        df.bin5s(i).trials(k).program = repmat({med(i).data.MSN}, size_5s);
        df.bin5s(i).trials(k).cohort = repmat({med(i).data.Experiment}, size_5s);
        
        %trial type - H = House, F = Flash [1 = HH, 2 = HF, 3 = FH, 4 = FF]
        size_10s = size(magdur_10sBins);
        df.bin10s(i).trials(k).trialID = repmat(med(i).data.M(k), size_10s);
        df.bin10s(i).trials(k).trialNum = repmat(k, size_10s);
        df.bin10s(i).trials(k).bins = unique(bin_10s, 'stable');
        df.bin10s(i).trials(k).magduration = magdur_10sBins;
        df.bin10s(i).trials(k).nosedistance = nosedist_10sBins;
        df.bin10s(i).trials(k).subject = repmat({med(i).data.Subject}, size_10s);
        df.bin10s(i).trials(k).testnumber = repmat({med(i).sessionNum}, size_10s);
        df.bin10s(i).trials(k).drug = repmat({med(i).data.Group}, size_10s);
        df.bin10s(i).trials(k).date = repmat({med(i).data.Start_Date}, size_10s);
        df.bin10s(i).trials(k).box = repmat(med(i).data.Box, size_10s);
        df.bin10s(i).trials(k).program = repmat({med(i).data.MSN}, size_10s);
        df.bin10s(i).trials(k).cohort = repmat({med(i).data.Experiment}, size_10s);
        
        
    end
end
toc

%% Put data from data from into long format
tic
%create empty data tables for long format data to go into
Table_1sbin = table();
Table_5sbin = table();
Table_10sbin = table();

for i = 1:size(df.bin1s,2)
    for j = 1:size(df.bin1s(i).trials,2)
        Table_1sbin = [Table_1sbin; struct2table(df.bin1s(i).trials(j))];
        Table_5sbin = [Table_5sbin; struct2table(df.bin5s(i).trials(j))];
        Table_10sbin = [Table_10sbin; struct2table(df.bin10s(i).trials(j))];
    end
end

writetable( Table_1sbin,'E:\MK001_MK002_VideosCombinedforAnalysis\Analysis\MK001_MK002_DLC_TimeBins.xlsx', 'Sheet', 'RawData_Bin1s');
writetable( Table_5sbin,'E:\MK001_MK002_VideosCombinedforAnalysis\Analysis\MK001_MK002_DLC_TimeBins.xlsx', 'Sheet', 'RawData_Bin5s');
writetable( Table_10sbin,'E:\MK001_MK002_VideosCombinedforAnalysis\Analysis\MK001_MK002_DLC_TimeBins.xlsx', 'Sheet', 'RawData_Bin10s');

toc

% need to add in baseline filter,
% parts, change trialID to string


%%
% Plot to make sure the points are reasonable!
% figure
% hold on
% for i = 1:size(DLC,2)
% plot([DLC(i).magCoord.bottomleft(x), DLC(i).magCoord.bottomright(x), DLC(i).magCoord.topright(x), DLC(i).magCoord.topleft(x)], [DLC(i).magCoord.bottomleft(y), DLC(i).magCoord.bottomright(y), DLC(i).magCoord.topright(y), DLC(i).magCoord.topleft(y)])
% end

%%


x1 = DLC(i).trials(k).filtered(:, nose(x));
y1 = DLC(i).trials(k).filtered(:, nose(y));
x2 = DLC(i).trials(k).filtered(:, head1(x));
y2 = DLC(i).trials(k).filtered(:, head1(y));
x3 = DLC(i).trials(k).filtered(:, head2(x));
y3 = DLC(i).trials(k).filtered(:, head2(y));

plotHeadx = [x1,x2,x3];
plotHeady = [y1,y2,y3];

x4 = DLC(i).trials(k).filtered(:, lear(x));
y4 = DLC(i).trials(k).filtered(:, lear(y));
x5 = DLC(i).trials(k).filtered(:, rear(x));
y5 = DLC(i).trials(k).filtered(:, rear(y));


plotEarx = [x4,x5];
plotEary = [y4,y5];

x6 = DLC(i).trials(k).filtered(:, mid1(x));
y6 = DLC(i).trials(k).filtered(:, mid1(y));
x7 = DLC(i).trials(k).filtered(:, mid2(x));
y7 = DLC(i).trials(k).filtered(:, mid2(y));
x8 = DLC(i).trials(k).filtered(:, tail(x));
y8 = DLC(i).trials(k).filtered(:, tail(y));

plotBodyx = [x6,x7,x8];
plotBodyy = [y6,y7,y8];

boxx = [DLC(i).boxparts.frontleft(x),DLC(i).boxparts.frontright(x),DLC(i).boxparts.backright(x),DLC(i).boxparts.backleft(x),DLC(i).boxparts.frontleft(x)];
boxy = [DLC(i).boxparts.frontleft(y),DLC(i).boxparts.frontright(y),DLC(i).boxparts.backright(y),DLC(i).boxparts.backleft(y),DLC(i).boxparts.frontleft(y)];

magx = [DLC(i).boxparts.magcentre(x),DLC(i).boxparts.magleft(x),DLC(i).boxparts.magright(x),DLC(i).boxparts.magcentre(x)];
magy = [DLC(i).boxparts.magcentre(y),DLC(i).boxparts.magleft(y),DLC(i).boxparts.magright(y),DLC(i).boxparts.magcentre(y)];

for k = 1:length(x1)
    
    plot(boxx,boxy);
    axis([50 250 0 200]);
    text(160,20,'MK801 Mouse F Session 2');
    hold on
    plot(magx,magy);
    plot(plotHeadx(k, :),plotHeady(k, :), 'r', 'LineWidth', 1);
    plot(plotEarx(k, :),plotEary(k, :), 'go', 'LineWidth', 1);
    plot(plotBodyx(k, :),plotBodyy(k, :), 'k', 'LineWidth', 1, 'LineStyle', '--');
    hold off
    
    if k > length(x1)*0.2727 && k < length(x1)*0.3636
        text(60,60,'Stim 1: House Light On');
    elseif k > length(x1)*0.6364 && k < length(x1)*0.7273
        text(60,60,'Stim 2: Flashing Lights On');
    end
    
    M(k) = getframe;
end


% create the video writer with 1 fps
writerObj = VideoWriter('SandersonTrial.avi');
writerObj.FrameRate = 30;
% set the seconds per image
% open the video writer
open(writerObj);
% write the frames to the video
for k=1:length(M)
    % convert the image to a frame
    frame = M(k) ;
    writeVideo(writerObj, frame);
end
% close the writer object
close(writerObj);

%%

%% Save the processed video data and filelists to a .mat file
%  save('E:\MK001_MK002_VideosCombinedforAnalysis\MK001_MK002_VideoProcessing.mat', 'videoproc', 'filelist', 'eventNum', 'eventID', 'med', 'DLC', 'df', '-v7.3')
%%
 load('E:\MK001_MK002_VideosCombinedforAnalysis\MK001_MK002_VideoProcessing.mat')

%%

