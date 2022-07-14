function [samples, outputPath] = plexon2bin(rawDatapath, saveBinPath, channelTpye, opts)
%  only pl2 files are supported, continous data must be recorded
%       channelTpye: 'SPKC' or 'WB'
%% data file names:
rawFileName = {};
for i = dir(rawDatapath)'
    if numel(i.name)<3 || ~strcmp(i.name(end-2:end), 'pl2')
        continue
    end
    rawFileName{end+1} = i;
end

samples = [];
% output folder
outputPath  = cell(1, numel(rawFileName));
for i = numel(rawFileName):-1:1
    name = rawFileName{i}.name;
    outputPath{i} = fullfile(saveBinPath, name(1:end-4));
%     if ~exist(outputPath{i}, 'dir')
%         mkdir(outputPath{i});
%     else
%         outputPath(i) = [];
%         rawFileName(i) = [];
%         fprintf('file %s may have already been converted, checked it and fix it mannully \n', name);
%     end
end

% return
% all files have been converted to .bin files
if isempty(rawFileName); samples=[]; return; end

%% use optional arguements and/or set defaults:
% init:
if ~exist('opts', 'var')
    opts = struct;
end

if ~isfield(opts, 'commonAverageReferencing')
    opts.commonAverageReferencing = false;
end

if ~isfield(opts, 'commonMedianReferencing')
    opts.commonMedianReferencing = false;
end

% remove artifacts
if ~isfield(opts, 'removeArtifacts')
    opts.removeArtifacts = false;
end

%% file names for .dat file (EPHYS) & .mat file (Timestamps and info):

% % EPHYS: dat file named after dsn:
% datPath = fullfile(opts.outputFolder, [dsn '.dat']);
% % if a .dat file already exists delete it so that new file is so fresh and
% % so clean clean
% if exist(datPath, 'file')
%     delete(datPath)
% end
%
%% begin conversion:

% Different file types require different code to extract goodies. Each
% filetype (e.g. plx, mpx, etc.) gets its own case in this switch loop:
tic;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% pl plx plx plx plx plx plx plx plx plx plx plx plx plx %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% create a list of all ad continuous channel names in cell array:
parfor nFile = 1:numel(rawFileName)
    converter(rawDatapath, rawFileName, outputPath, channelTpye, opts, nFile);
end

end


function converter(rawDatapath, rawFileName, outputPath, channelTpye, opts, nFile)
    fileName = fullfile(rawDatapath, rawFileName{nFile}.name);
    fprintf('          is converting file "%s" \n', fileName);
    % gets the names of a/d channels
    [nCh, adChName]     = plx_adchan_names(fileName);
    chNameList = cell(nCh,1);
    for ii = 1:nCh
        chNameList{ii} = adChName(ii,:);
    end
    idxContinousCh = false(numel(chNameList),1);

    % if user provided specific channels to use for conversion, take
    % them:
    if isfield(opts, 'specificChannels') && opts.specificChannels
        idxContinousCh(opts.specificChannels) = true;
    else
        % otherwise, figure out which spike-continuous channels have
        % data and grab'em:

        % get indices for the contiunous data
        for iCh = 1:numel(chNameList)
            if ~isempty(strfind(chNameList{iCh}, channelTpye))
                idxContinousCh(iCh) = true;
            else
                idxContinousCh(iCh) = false;
            end
        end
    end

    % get number of spikes counts per ad channel and get use only
    % those that have data:
    [~, samplecounts] = plx_adchan_samplecounts(fileName);
    idxDataCh = samplecounts~=0;
    % get indices for channels that are both spk channels & have data:
    idxGoodCh =  idxContinousCh & idxDataCh;
    if sum(idxGoodCh)==0
        disp('no continous data have been found');
        return
    end
    % nChannels & nSamples:
    nChannels   = sum(idxGoodCh);
    tmp         = samplecounts(idxGoodCh);
    nSamples    = tmp(1); % taking the number of samples in first spk channel. Rest are identical.

    % build data matrix 'samples' of size [nChannels, nSamples]:
    samples     = zeros(nChannels, nSamples, 'int16'); % 
    tChRead     = nan(nChannels,1); % time keeping
    % gotta map out indices to plxeon's ad channel numbers:
    [~,   adChNumber]   = plx_ad_chanmap(fileName);
    spkChNumber = adChNumber(idxGoodCh);
    fprintf('%0.1fs: Getting data from %0.0d spike channels!\n', toc, sum(idxGoodCh))
    hWait = waitbar(0, 'Converting channels...');
    for iCh = 1:nChannels
        tChRead(iCh) = toc;
        fprintf('\t%0.1fs: read channel #%0.0d \n', tChRead(iCh), spkChNumber(iCh));
        % data matrix 'samples':
        [~, ~, ~, ~, ad] = plx_ad(fileName, spkChNumber(iCh)); % returns signal in miliVolts
        samples(iCh,:) =  ad(:, 1);
        waitbar(iCh/nChannels, hWait, ['Converting channel ' num2str(iCh) ' of ' num2str(nChannels)]);
    end
    close(hWait)

    %% ephys data to bin file:
    % subtract mean across channels:
    idxDataCh = idxDataCh(idxContinousCh);
    if opts.commonAverageReferencing || opts.commonMedianReferencing
        % get the group in which only good channels are included
        [groups, groups2] = deal(opts.groups); 
        n = sum(idxContinousCh)+1;
        for iCh = sum(idxContinousCh):-1:1
            if idxContinousCh(iCh); n = n - 1; else; continue; end
            if idxDataCh(iCh);  continue; end 
            for iGup = 1:numel(groups)
                disp(n)
                groups{iGup}(groups{iGup}==n) = [];
                groups{iGup}(groups{iGup}>n) = groups{iGup}(groups{iGup}>n)-1;

                groups2{iGup}(groups2{iGup}==n) = [];
            end
        end
    else
        groups = {1:nChannels};s
        groups2 = find(idxGoodCh)' - find(idxContinousCh,1) + 1;
    end

    if opts.commonAverageReferencing
        disp('Performing common average subtraction...')
        % subtract:
        for group = groups
            samplesMean = mean(samples(group{:}, :), 1);
            samples(group{:}, :) = samples(group{:}, :) - samplesMean;
        end
    end

    if opts.commonMedianReferencing
        disp('Performing common median subtraction...')
        % subtract median across channels
        for group = groups
            samplesMedian = median(samples(group{:}, :), 1);
            samples(group{:}, :) = samples(group{:}, :) - samplesMedian;
        end
    end
    samples = int16(samples);
    name = rawFileName{nFile}.name;
    outputFileName = fullfile(outputPath{nFile}, [name(1:end-4),'.bin']);
    fidout = fopen(outputFileName, 'w'); % opening file for writing
    fwrite(fidout, samples, 'int16');
    fclose(fidout);
    % % save ephys data from each channel to bin file
    groups2 = cell2mat(groups2);
    assert(numel(groups2) == size(samples,1));
    for i = 1:numel(groups2)
        outputPerChName = [name(1:end-4), '_', num2str(groups2(i)), '.bin'];
        if groups2(i) < 10
            outputPerChPath = fullfile(outputPath{nFile}, [name(1:end-4), '_0', num2str(groups2(i))]);
        else
            outputPerChPath = fullfile(outputPath{nFile}, [name(1:end-4), '_', num2str(groups2(i))]);
        end
        % make a folder
        mkdir(outputPerChPath);
        % write date for each channel
        fidout = fopen(fullfile(outputPerChPath, outputPerChName), 'w'); 
        fwrite(fidout, samples(i, :), 'int16');
        fclose(fidout);
    end

    %%  extract timing information from raw file in "real" time
    % 'tsMap' has a timestamp for every sample recorded. This will be a
    % vector of size nSamples. tsMap is used to convert from the spike
    % index output of kiloSort (these are simply integers that indicate
    % which sample number each spike occurred at), to time (in seconds)
    % relative to the beginning of the ephys recording.
    % This is needed because the event time stamps (evTs) from the raw
    % file are in same relative time (also in seconds).

    % get timestamps start values (tsStartVals) at start of each fragment:
    disp('Getting plexon timestamps for ad samples');

    % must read in a spike channel to construct the "timestamp map" from
    % samples (kilosort) to time in seconds.
    ad = PL2Ad(fileName, [channelTpye, '01']);
    % place to store the "map" from samples to seconds.
    sampsToSecsMap = zeros(sum(ad.FragCounts),1);

    % sample duration
    sampDur = 1/ad.ADFreq;

    % how many fragments of recording?
    nFrags = length(ad.FragTs);
    currentSample = 1;
    for i = 1:nFrags
        chunkIndex = currentSample:(currentSample + ad.FragCounts(i) - 1);
        timeStamps = ad.FragTs(i) + (0:(ad.FragCounts(i)-1))*sampDur;
        sampsToSecsMap(chunkIndex) = timeStamps;
        currentSample = chunkIndex(end)+1;
    end


    %% extract strobed events:
    % read the strobed word info (values & time stamps):
    strobedEvents.eventInfo = PL2EventTs(fileName, 'Strobed');

    % read the time-stamps of recording start / stop events:
    strobedEvents.startTs = PL2StartStopTs(fileName, 'start');
    strobedEvents.stopTs = PL2StartStopTs(fileName, 'stop');


    %% extract analog input channels
    % Analog Inputs (AI) extracted differently in different systems:
    %  In opx-A, AI are in the 4 topmost LFP channels.
    %  In opx-D, they have dedicated channels termed "AI"
    % I'm gonna make an assumption that if my input file is in the
    % newer 'pl2' version, it is from opx-D, while if it is the older
    % 'plx', it is opx-A. This assumption is not bulletproof, so
    % proceed with caution...
    tic
    clear ai
    ai(1) = PL2Ad(fileName, 'AI01');
    ai(2) = PL2Ad(fileName, 'AI02');
    ai(3) = PL2Ad(fileName, 'AI03');
    ai(4) = PL2Ad(fileName, 'AI04');
    toc
    % construct a vector of time (in seconds) that corresponds to the
    % voltages in ai.Values.
    ii = 1; % time is identical for all ai channels so I will run the following code on one of them
    aiTimeStamps = zeros(sum(ai(ii).FragCounts),1);

    % sample duration
    sampDur = 1/ai(ii).ADFreq;

    % how many fragments of recording?
    nFrags = length(ai(ii).FragTs);
    currentSample = 1;
    for i = 1:nFrags
        chunkIndex = currentSample:(currentSample + ai(ii).FragCounts(i) - 1);
        chunkTimeStamps = ai(ii).FragTs(i) + (0:(ai(ii).FragCounts(i)-1))*sampDur;
        aiTimeStamps(chunkIndex) = chunkTimeStamps;
        currentSample = chunkIndex(end)+1;
    end

    %% extract info:
    pl2 = PL2GetFileIndex(fileName);

    %% Pack up and save:

    % meta info:
    info.rawFile        = fileName;
    info.rawFolder      = rawDatapath;
    info.spkChNumber    = spkChNumber;
    % info.strbChNumber   = strbChNumber;
    info.opts           = opts;
    info.datestr        = datestr(now, 'yyyymmddTHHMM');
    info.pl2            = pl2;

    % save info:
    save(fullfile(outputPath{nFile}, 'convertInfo.mat'),  'info');


    % % timing data to mat file:
    % disp('Saving mat file with timestamps & info')
    % save(tsPath, 'sampsToSecsMap', 'info');

    % save sampsToSecsMap (has to be 7.3 cause these can get BIG):
    save(fullfile(outputPath{nFile}, 'sampsToSecsMap.mat'),  'sampsToSecsMap', '-v7.3')

    % save strobe info:
    save(fullfile(outputPath{nFile}, 'strobedEvents.mat'),  'strobedEvents')

    % save analog input:
    save(fullfile(outputPath{nFile}, 'aiChannels.mat'), 'aiTimeStamps', 'ai');


    fprintf('%f0.1s: CONVERSION COMPLETE!', toc)
end
