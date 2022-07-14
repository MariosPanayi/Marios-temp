

% addPathsForSpikeSorting;
addpath(genpath('E:\Zhewei\packages\kilosort_2'));

rmpath(genpath('E:\Zhewei\packages\kilosort_25'))
rmpath(genpath('E:\Zhewei\packages\kilosort_3'))

 

%% creat configuration file

step = 50;
Nchannels = 1;
connected = true(Nchannels, 1);
chanMap   = 1:Nchannels;
chanMap0ind = chanMap - 1;
xcoords   = ones(Nchannels,1); % 50:50:1600;( unexpected error occurred during CUDA execution)

ycoords   = [1:50:step*Nchannels]'; %[1:Nchannels]' 
kcoords   = [1:Nchannels]'; % ones(Nchannels,1); % grouping of channels (i.e. tetrode groups)

fs = 40000; % sampling frequency
save('E:\Zhewei\offline_sorting\chanMap_SA.mat', ...
    'chanMap','connected', 'xcoords', 'ycoords', 'kcoords', 'chanMap0ind', 'fs')

chanMapFile = 'chanMap_SA.mat';
configuFile = 'config_SA.m';
configFilePath = 'E:\Zhewei\offline_sorting\'; 

%%

channelTpye = 'WB'; % SPKC
opts = struct('commonMedianReferencing', true, 'groups', {{1:16, 17:32}});
rawDatapath = 'E:\Zhewei\offline_sorting\data\raw_pl2\SA01';
saveBinPath = 'E:\Zhewei\offline_sorting\data\raw_bin\SA01';

[~, binPath] = plexon2bin(rawDatapath, saveBinPath, channelTpye, opts);

for path = binPath
    % sort .bin file in each subfolder
    subfolds = dir(path{:})';
    for n = 1:numel(subfolds)
        subfolder = subfolds(n);
        if ~ subfolder.isdir; continue; end 
        if any(strcmp(subfolder.name,{'.','..'})); continue; end
        subfolderPath = fullfile(path{:}, subfolder.name);
        disp(repmat('*', 1, 80));
        fprintf('start sorting file %s \n', subfolderPath)
        try
            sorting_zzw(subfolderPath, subfolderPath, configFilePath, ...
                chanMapFile, configuFile, Nchannels)
            fprintf('sorting on file %s was successful\n', subfolderPath)
        catch 
            fprintf('sorting on file %s was NOT successful\n', subfolderPath)
        end
    end

    % combine results in each subfolder
    try
        combinePhy(path{:});
    end
    close all;
end



