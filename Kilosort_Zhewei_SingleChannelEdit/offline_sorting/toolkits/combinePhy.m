function combinePhy(parentFolder)

[~,name,~] = fileparts(parentFolder);

%% build a fold to save the combined data
savePath = fullfile(parentFolder, ['kilosort_', name]);
if ~exist(savePath, 'dir')
    mkdir(savePath)
end


%% how many channels in this folder
numChannels = 0;
for subfolder = dir(parentFolder)'
    if subfolder.isdir && any(strfind(subfolder.name, name))
        if any(strfind(subfolder.name, 'kilo')); continue; end
        numChannels = numChannels + 1;
    end
end


%%%% TODO: these three parts are not flexible
%% write params.py file.
% dat_path     - location of raw data file
% channels_dat - total number of rows in the data file
% dtype - data type to read, e.g. 'int16'
% offset - number of bytes at the beginning of the file to skip
% sample_rate - in Hz
% hp_filtered - True/False, whether the data have already been filtered
%make params file


fs = [dir(fullfile(parentFolder, '*.bin')) dir(fullfile(parentFolder, '*.dat'))];
[~, fname, ext] = fileparts(fs(1).name);

fid = fopen(fullfile(savePath,'params.py'), 'w');

fprintf(fid,['dat_path = ''', fname ext '''\n']);
fprintf(fid,'n_channels_dat = %i\n', numChannels);
fprintf(fid,'dtype = ''int16''\n');
fprintf(fid,'offset = 0\n');
fprintf(fid,'sample_rate = %i.\n', 40000);
fprintf(fid,'hp_filtered = False');
fclose(fid);


%% write channel_map.npy - [nChannels, ]
% int32 vector with the channel map,
% i.e. which row of the data file to look in for the channel in question
chanMap   = 1:numChannels;
chanMap0ind = int32(chanMap - 1);
writeNPY(chanMap0ind, fullfile(savePath, 'channel_map.npy'));


%% write channel_positions.npy - [nChannels, 2]
% double matrix with each row giving the x and y coordinates of that channel.
% Together with the channel map, this determines how waveforms will be
% plotted in WaveformView (see below).
step = 50;
xcoords   = ones(numChannels,1); % 50:50:1600;
ycoords   = [1:50:step*numChannels]'; %[1:Nchannels]'
writeNPY([xcoords ycoords], fullfile(savePath, 'channel_positions.npy'));

%% write tsv files
KSLabelFilename = fullfile(savePath, 'cluster_KSLabel.tsv');
fileID = fopen(KSLabelFilename,'w');
fprintf(fileID, 'cluster_id%sKSLabel', char(9));
fprintf(fileID, char([13 10]));

fileIDCP = fopen(fullfile(savePath, 'cluster_ContamPct.tsv'),'w');
fprintf(fileIDCP, 'cluster_id%sContamPct', char(9));
fprintf(fileIDCP, char([13 10]));

fileIDA = fopen(fullfile(savePath, 'cluster_Amplitude.tsv'),'w');
fprintf(fileIDA, 'cluster_id%sAmplitude', char(9));
fprintf(fileIDA, char([13 10]));



%% initate variables

amplitudes = [];
pcFeatures = [];
pcFeatureInds = [];
spikeTemplates = [] ;
templateFeatures = [];
templateFeatureInds = [];
templates = [];
templatesInds = [];
spikeTimes = [];
spikeClusters = [];
similarTemplates = [];
whiteningMatrix = [];
whiteningMatrixInv = [];
%%
nTemp = 0;
nChannel = 0;
nCluster = 0;
for subfolder = dir(parentFolder)'
    if ~subfolder.isdir; continue; end
    if ~any(strfind(subfolder.name, name)); continue; end
    if  any(strfind(subfolder.name, 'kilo')); continue; end
    subfolderPath = fullfile(parentFolder, subfolder.name);
    nChannel = nChannel+1;
    %     disp(subfolder.name)

    %% check if the file in the current folder was sorted successfully.
    if ~exist(fullfile(parentFolder, subfolder.name, 'params.py'), 'file')
        if nChannel == numChannels
            % whitening_mat.npy/whitening_mat_inv.npy - [nChannels, nChannels]
            nChan = size(whiteningMatrix, 1);
            whiteningMatrixTemp = zeros(nChannel);
            whiteningMatrixInvTemp = zeros(nChannel);
            whiteningMatrixTemp(1:nChan, 1:nChan) = whiteningMatrix;
            whiteningMatrixInvTemp(1:nChan, 1:nChan) = whiteningMatrixInv;
            for i = nChan+1:nChannel-1
                whiteningMatrixTemp(i, i) = 1;
                whiteningMatrixInvTemp(i, i) = 1;
            end
            whiteningMatrix = whiteningMatrixTemp;
            whiteningMatrixInv = whiteningMatrixInvTemp;

            % template 
            nTemp = size(templates, 1);
            nTime = size(templates, 2);
            nChan = size(templates, 3);
            
            templatesTemp = zeros(nTemp, nTime, nChannel);
            templatesTemp(:, :, 1:nChan) = templates;
            templates = templatesTemp;
        end
        continue
    end


    %% amplitudes.npy - [nSpikes, ]
    % double vector with the amplitude scaling factor that was applied to
    % the template when extracting that spike
    amplitudeCurr = readNPY(fullfile(subfolderPath, 'amplitudes.npy'));
    amplitudes = [amplitudes; amplitudeCurr];  %# ok


    %% pc_features.npy - [nSpikes, nFeaturesPerChannel, nPCFeatures]
    % single matrix giving the PC values for each spike.
    % The channels that those features came from are specified in
    % pc_features_ind.npy.
    % E.g. the value at pc_features[123, 1, 5] is the projection of the
    % 123rd spike onto the 1st PC on the channel given by pc_feature_ind[5].
    pcFeaturesCurr = readNPY(fullfile(subfolderPath, 'pc_features.npy'));
    pcFeatures = [pcFeatures; pcFeaturesCurr];  %# ok


    %% pc_feature_ind.npy - [nTemplates, pcFeatureInds]
    % uint32 matrix specifying which pcFeatures are included
    % in the pc_features matrix.
    pcFeatureIndsCurr = readNPY(fullfile(subfolderPath, 'pc_feature_ind.npy'));
    pcFeatureInds = [pcFeatureInds; pcFeatureIndsCurr];  %# ok


    %% similar_templates.npy - [nTemplates, nTemplates]
    % single matrix giving the similarity score (larger is more similar)
    % between each pair of templates
    similarTemplatesCurr = readNPY(fullfile(subfolderPath, 'similar_templates.npy'));
    if isempty(similarTemplates)
        similarTemplates = similarTemplatesCurr;
    else
        nTemp = size(similarTemplates, 1);
        nTempCurr = size(similarTemplatesCurr, 1);
        similarTemplatesTemp = zeros(nTemp+nTempCurr);
        similarTemplatesTemp(1:nTemp, 1:nTemp) = similarTemplates;
        similarTemplatesTemp(1+nTemp:end, 1+nTemp:end) = similarTemplatesCurr;
        similarTemplates = similarTemplatesTemp;
    end


    %% spike_templates.npy - [nSpikes, ]
    % uint32 vector specifying the identity of the template that was used
    % to extract each spike
    spikeTemplatesCurr = readNPY(fullfile(subfolderPath, 'spike_templates.npy'));
    spikeTemplates = [spikeTemplates; nTemp+spikeTemplatesCurr];  %# ok


    %% spike_times.npy - [nSpikes, ]
    % uint64 vector giving the spike time of each spike in samples.
    % To convert to seconds, divide by sample_rate from params.py.
    spikeTimesCurr = readNPY(fullfile(subfolderPath, 'spike_times.npy'));
    spikeTimes = [spikeTimes; spikeTimesCurr];  %# ok

    %% template_features.npy - [nSpikes, nTempFeatures]
    % single matrix giving the magnitude of the projection of each spike
    % onto nTempFeatures other features. Which other features is specified
    % in template_feature_ind.npy
    templateFeaturesCurr = readNPY(fullfile(subfolderPath, 'template_features.npy'));
    templateFeatures = [templateFeatures; templateFeaturesCurr];  %# ok

    %% template_feature_ind.npy - [nTemplates, nTempFeatures]
    % uint32 matrix specifying which templateFeatures are included
    % in the template_features matrix.
    templateFeatureIndsCurr = readNPY(fullfile(subfolderPath, 'template_feature_ind.npy'));
    if isempty(templateFeatureInds)
        templateFeatureInds = templateFeatureIndsCurr;
    else
        nTemp = size(templateFeatureInds, 1);
        templateFeatureInds = [templateFeatureInds; templateFeatureIndsCurr+nTemp];  %# ok
    end

    %% templates.npy - [nTemplates, nTimePoints, nTempChannels]
    % single matrix giving the template shapes on the channels given
    % in templates_ind.npy
    templatesCurr = readNPY(fullfile(subfolderPath, 'templates.npy'));
    if isempty(templates)
        nTempCurr = size(templatesCurr, 1);
        nTime = size(templatesCurr, 2);
        
        templates = zeros(nTempCurr, nTime, nChannel);
        templates(:, :, end) = templatesCurr;
    else
        nTemp = size(templates, 1);
        nTime = size(templates, 2);
        nChan = size(templates, 3);
        nTempCurr = size(templatesCurr, 1);
        
        templatesTemp = zeros(nTemp+nTempCurr, nTime, nChannel);
        templatesTemp(1:nTemp, :, 1:nChan) = templates;
        templatesTemp(1+nTemp:end, :, end) = templatesCurr;
        templates = templatesTemp;
    end


    %% templates_ind.npy - [nTemplates, nTempChannels]
    % double matrix specifying the channels on which each template is defined.
    % In the case of Kilosort templates_ind is just the integers
    % from 0 to nChannels-1, since templates are defined on all channels.
    templatesIndsCurr = readNPY(fullfile(subfolderPath, 'templates_ind.npy'));
    if isempty(templatesInds)
        templatesInds = templatesIndsCurr+nChannel-1;
    else
        templatesInds = [templatesInds; nChannel-1+zeros(size(templatesIndsCurr))];  %# ok
    end

    %% spike_clusters.npy - [nSpikes, ]
    % int32 vector giving the cluster identity of each spike.
    % This file is optional and if not provided will be automatically
    % created the first time you run the template gui, taking the same
    % values as spike_templates.npy until you do any merging or splitting.
    spikeClustersCurr = readNPY(fullfile(subfolderPath, 'spike_clusters.npy'));
    if isempty(spikeClusters)
        spikeClusters = spikeClustersCurr;
    else
        spikeClusters = [spikeClusters; spikeClustersCurr+nCluster+1];  %# ok
    end
    nCluster = max(spikeClusters);
    %% whitening_mat.npy - [nChannels, nChannels]
    % double whitening matrix applied to the data during automatic spike sorting
    whiteningMatrixCurr = readNPY(fullfile(subfolderPath, 'whitening_mat.npy'));
    nChan = size(whiteningMatrix, 1);
    whiteningMatrixTemp = zeros(nChannel);
    whiteningMatrixTemp(1:nChan, 1:nChan) = whiteningMatrix;
    for i = nChan+1:nChannel-1
        whiteningMatrixTemp(i, i) = 1;
    end
    whiteningMatrixTemp(end, end) = whiteningMatrixCurr;
    whiteningMatrix = whiteningMatrixTemp;
    
    %% whitening_mat_inv.npy - [nChannels, nChannels]
    % double, the inverse of the whitening matrix.
    whiteningMatrixInvCurr  = readNPY(fullfile(subfolderPath, 'whitening_mat_inv.npy'));

    nChan = size(whiteningMatrixInv, 1);
    whiteningMatrixInvTemp = zeros(nChannel);
    whiteningMatrixInvTemp(1:nChan, 1:nChan) = whiteningMatrixInv;
    for i = nChan+1:nChannel-1
        whiteningMatrixInvTemp(i, i) = 1;
    end
    whiteningMatrixInvTemp(end, end) = whiteningMatrixInvCurr;
    whiteningMatrixInv = whiteningMatrixInvTemp;

    %% cluster_groups.csv
    % comma-separated value text file giving the "cluster group" of
    % each cluster (0=noise, 1=MUA, 2=Good, 3=unsorted)
    ksLabelCurr = tdfread(fullfile(subfolderPath, 'cluster_KSLabel.tsv'));    
    ampltudeCurr = tdfread(fullfile(subfolderPath, 'cluster_Amplitude.tsv'));
    contamPctCurr = tdfread(fullfile(subfolderPath, 'cluster_ContamPct.tsv'));

    for i = 1:numel(ksLabelCurr.cluster_id)
        fprintf(fileID, '%d%s%s', nTemp+i-1, char(9), ksLabelCurr.KSLabel(i,:));
        fprintf(fileID, char([13 10]));

        fprintf(fileIDA, '%d%s%.1f', nTemp+i-1, char(9), ampltudeCurr.Amplitude(i));
        fprintf(fileIDA, char([13 10]));

        fprintf(fileIDCP, '%d%s%.1f', nTemp+i-1, char(9), contamPctCurr.ContamPct(i));
        fprintf(fileIDCP, char([13 10]));
    end

end

fclose(fileID);
fclose(fileIDA);
fclose(fileIDCP);
%% sort variables related to spikes, 
% the spike times must be increasing
[spikeTimes, I] = sort(spikeTimes);
amplitudes = amplitudes(I, :);
pcFeatures = pcFeatures(I, :, :);
spikeClusters = spikeClusters(I, :);
spikeTemplates = spikeTemplates(I, :);
templateFeatures = templateFeatures(I, :);



%% save files

% amplitudes.npy - [nSpikes, ]
writeNPY(amplitudes, fullfile(savePath, 'amplitudes.npy'));

% pc_features.npy - [nSpikes, nFeaturesPerChannel, nPCFeatures]
writeNPY(pcFeatures, fullfile(savePath, 'pc_features.npy'));

% pc_feature_ind.npy - [nTemplates, nPCFeatures]
writeNPY(pcFeatureInds, fullfile(savePath, 'pc_feature_ind.npy'));

% similar_templates.npy - [nTemplates, nTemplates]
writeNPY(similarTemplates, fullfile(savePath, 'similar_templates.npy'));

% spike_templates.npy - [nSpikes, ]
writeNPY(uint32(spikeTemplates), fullfile(savePath, 'spike_templates.npy'));

% spike_times.npy - [nSpikes, ]
writeNPY(spikeTimes, fullfile(savePath, 'spike_times.npy'));

% template_features.npy - [nSpikes, nTempFeatures]
writeNPY(templateFeatures, fullfile(savePath, 'template_features.npy'));

% template_feature_ind.npy - [nTemplates, nTempFeatures]
writeNPY(templateFeatureInds, fullfile(savePath, 'template_feature_ind.npy'));

% templates.npy - [nTemplates, nTimePoints, nTempChannels]
writeNPY(templates, fullfile(savePath, 'templates.npy'));

% templates_ind.npy - [nTemplates, nTempChannels]
writeNPY(templatesInds, fullfile(savePath, 'templates_ind.npy'));

% whitening_mat.npy - [nChannels, nChannels]
writeNPY(whiteningMatrix, fullfile(savePath, 'whitening_mat.npy'));

% whitening_mat_inv.npy - [nChannels, nChannels]
writeNPY(whiteningMatrixInv, fullfile(savePath, 'whitening_mat_inv.npy'));

% spike_clusters.npy - [nSpikes, ]
writeNPY(uint32(spikeClusters), fullfile(savePath, 'spike_clusters.npy')); 

% cluster_groups.csv
% comma-separated value text file giving the "cluster group" of
% each cluster (0=noise, 1=MUA, 2=Good, 3=unsorted)
copyfile(KSLabelFilename, fullfile(savePath, 'cluster_group.tsv'));


end