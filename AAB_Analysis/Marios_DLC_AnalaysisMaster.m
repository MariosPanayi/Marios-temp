tic
%%
file.folder = "C:\Users\panayimc\Box\NIDA_Expts\Papers\AAB_Analysis_2020\Summary";
file.subfolder = "GluA1KO";
file.name = 'GluA1AAB_Data.mat';


%
% List of DLC labels in order [from RawData.bodyparts]
nose = 1;
head1 = 2;
head2 = 3;
lear = 4;
rear = 5;
mid1 = 6;
mid2 = 7;
tail = 8;
innertopleft = 9;
innertopright = 10;
innerbottomleft = 11;
innerbottomright = 12;
outertopleft = 13;
outertopright = 14;
outerbottomleft = 15;
outerbottomright = 16;
%composite parts - number to continue from last DLC part above
head = 17;
body = 18;
composites = [head, body];

% Separate body parts from points in apparatus
parts = [nose head1 head2 lear rear mid1 mid2 tail];
boxlimits = [outertopleft outertopright outerbottomleft outerbottomright];
boxinner = [innertopleft innertopright innerbottomleft innerbottomright];

% Create composites of body parts if desired
head_comp = [nose head1 head2 lear rear];
body_comp = [mid1 mid2 tail];
% Add these to a list of composites
composites_comp = {head_comp; body_comp};

% Order of sub columns from Raw data
x = 1;
y = 2;
likelihood = 3;

% params
params.crop = 1; % Crop data based on box limits? True or false
params.interpolate = 1; %Interpolate tracking data if below some criterion - linear interpolation used [ see interpolateLowConfidence2.m]
params.criterion = 1; %Threshold criterion for trackign points to interpolate
params.smoothxy = 0; %0.25 %Option to smooth data with moving average over some period of time in seconds
params.boxdimension = 40; %Parameter used to convert pixel distance to cm. Value here should be the distance (in cm) between the inner corners of the square box.
params.downsamplerate = 20; %Downsample rate to regularise the variable framerate e.g. 10 Hz = 10


% Run pre-processing scripts - might need to update how these functions
% take in file information!!!
runpreprocessing = 0;

if runpreprocessing
%     AAB_preprocessDLCRawFiles
%     AAB_preprocessDLCdata
else
    load(fullfile(file.folder,file.subfolder,file.name))
end

%%
% Plot arena exploration
% Distanced Travelled
% Percent arena coverage
% Correlation of exploration patterns
%   - Need to convert to fisher's Z score for analysis with parametric
%   stats
%   - Run permutation test with correlated spatial information for each
%   comparison
%   - 10x10 makes the arena 4cm squares - reasonable given a mouse size
% Direction the animal is looking [convert into 8 areas, corners and middle
% of walls] -> performa a similar correlation analysis with this?
%

%% Distanced Travelled
bin_duration = 5;
fps = params.downsamplerate;
framesSmoothing = 1;
for i = 1:size(RawData, 2)
    data = RawData(i).data_downsampled;
[distance] = DLC_distancetravelled(data);
RawData(i).distance = distance*RawData(i).px2cm;
[Distance_Bins] = DLC_dataBins(distance, fps, bin_duration, framesSmoothing);
RawData(i).Distance_Bins = Distance_Bins;

end

%% Percent arena coverage
% Calculate te percentage of the arena covered/explored

tstart = 1;
tend =  params.downsamplerate*45;
Nbins = [10 10];
bodypart = head;
bp.x = 1+ (bodypart*3)-2;
bp.y = 1+ (bodypart*3)-1;
for i = 1:size(RawData, 2)
data = RawData(i).data_downsampled;
[coverage] = histcounts2(data(tstart:tend,bp.x),data(tstart:tend,bp.y),Nbins);

coverage_perc = (sum(coverage(:) > 0)/ length(coverage(:)))*100;
RawData(i).coverage_perc = coverage_perc;
RawData(i).coverage = coverage;
end


% Convert struct to table to order variables - for correlation analysis

clear Tabledata Tablecorrs tocompare
for i = 1:size(RawData, 2)
Tabledata(i).animal = RawData(i).animal;
Tabledata(i).vidname = RawData(i).vidname;
Tabledata(i).treatment = RawData(i).treatment;
Tabledata(i).stage = RawData(i).stage;
Tabledata(i).experiment = RawData(i).experiment;
Tabledata(i).coverage = RawData(i).coverage;
Tabledata(i).coverage_perc = RawData(i).coverage_perc;
Tabledata(i).data_downsampled = RawData(i).data_downsampled;
Tabledata(i).px2cm = RawData(i).px2cm;
Tabledata(i).Distance_Bins = RawData(i).Distance_Bins;
Tabledata(i).distance = RawData(i).distance;
end

Tabledata = struct2table(Tabledata);
Tabledata = sortrows(Tabledata,{'treatment','animal', 'stage'});

% Coverage correlations
% tstart = 1;
% tend =  downsamplerate*30;
% Nbins = [10 10];
% bodypart = mid1;
% bp.x = 1+ (bodypart*3)-2;
% bp.y = 1+ (bodypart*3)-1;
for i = 1:size(RawData, 2)/3
    trial1 = ((i-1)*3) + 1;
    trial2 = ((i-1)*3) + 2;
    trial3 = ((i-1)*3) + 3;
    tocompare(:,1) = Tabledata.coverage{trial1}(:);
    tocompare(:,2) = Tabledata.coverage{trial2}(:);
    tocompare(:,3) = Tabledata.coverage{trial3}(:);
    %  
    Tablecorrs(i).animal = Tabledata.animal{trial1};
    Tablecorrs(i).vidname = Tabledata.vidname{trial1};
    Tablecorrs(i).treatment = Tabledata.treatment{trial1};
    Tablecorrs(i).experiment = Tabledata.experiment{trial1};
    Tablecorrs(i).corr_12 = corr(tocompare(:,1), tocompare(:,2), 'type','Spearman');
    Tablecorrs(i).corr_13 = corr(tocompare(:,1), tocompare(:,3), 'type','Spearman');
    Tablecorrs(i).corr_23 = corr(tocompare(:,2), tocompare(:,3), 'type','Spearman');
    
end
Tablecorrs = struct2table(Tablecorrs);
% %%
% figure;
% j = 0;
% tstart = 1;
% tend =  downsamplerate*30;
% toCompare = [];
% 
% for i = [6 7 8];
%     
%     j = j+1;
%     
%     subplot(2,3,j)
%     scatter(RawData(i).data_downsampled(tstart:tend,50), RawData(i).data_downsampled(tstart:tend,51),2,[tstart:tend])
%     
%     subplot(2,3,j+3)
%     hist3([RawData(i).data_downsampled(tstart:tend,50), RawData(i).data_downsampled(tstart:tend,51)],'Nbins',[10 10], 'FaceColor','interp', 'CDataMode','auto')
%     [N,~] = hist3([RawData(i).data_downsampled(tstart:tend,50), RawData(i).data_downsampled(tstart:tend,51)],'Nbins',[10 10], 'FaceColor','interp', 'CDataMode','auto');
%     toCompare(:,j) = N(:);
%     %colormap('bone')
%     colorbar
%     caxis([1 100])
%     view(2)
% end
% 
% c(1) = corr(toCompare(:,1), toCompare(:,2), 'type','Spearman');
% c(2) = corr(toCompare(:,1), toCompare(:,3), 'type','Spearman');
% c(3) = corr(toCompare(:,2), toCompare(:,3), 'type','Spearman');
% d =  corr(toCompare, toCompare, 'type','Spearman');
% % d = 1-d;
% figure
% imagesc(d)
% colorbar
% caxis([0 1])

%%
toc
