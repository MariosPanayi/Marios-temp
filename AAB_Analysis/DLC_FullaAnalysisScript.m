
% Load Data
path = 'C:\Users\mpanagi\Documents\GitHub\Marios-temp\AAB_Analysis\';
filename = 'Test 1DeepCut_resnet50_AABFeb18shuffle1_225000.csv';
[data, bodyparts]  = DLC_RawRead([path,filename]);

% Remove NaNs
% x = data(:, 2);
% y = data(:, 3);
% probability = data(:,4);
% badfit = find(probability < 0.9)


%% remove first ~5s of data when still frame is in the video

pcutoff = 0.9

a = find(data(:,4) > pcutoff , 1, 'first');
b = find(data(:,7) > pcutoff , 1, 'first');
c = find(data(:,10) > pcutoff , 1, 'first');
d = find(data(:,13) > pcutoff , 1, 'first');
cutoff_frame = max([a b c d]);




%%
smoothxy = 1;

nose = [diff(smooth(data(:,2),smoothxy)),smooth(diff(data(:,3)),smoothxy)];
nose_dist = sqrt(nose(:,1).^2 + nose(:,1).^2);

Tail = [smooth(diff(data(:,5)),smoothxy),smooth(diff(data(:,6)),smoothxy)];
tail_dist = sqrt(Tail(:,1).^2 + Tail(:,1).^2);

LEar = [smooth(diff(data(:,8)),smoothxy),smooth(diff(data(:,9)),smoothxy)];
LEar_dist = sqrt(LEar(:,1).^2 + LEar(:,1).^2);

REar = [smooth(diff(data(:,11)),smoothxy),smooth(diff(data(:,12)),smoothxy)];
REar_dist = sqrt(REar(:,1).^2 + REar(:,1).^2);

framenum = data(2:end,1);

Distance = [nose_dist, tail_dist, LEar_dist, REar_dist];
Confidence = [data(:,4),data(:,7),data(:,10),data(:,13)];

%% Bins
smoothing = 5;
binwidth = 34.44*5;
start = 174;
bins = ceil((framenum - (start-1))/binwidth);
nose_bins = accumarray([bins-min(bins)+1], smooth(nose_dist, smoothing),[],@sum);
tail_bins = accumarray([bins-min(bins)+1], smooth(tail_dist, smoothing),[],@sum);
LEar_bins = accumarray([bins-min(bins)+1], smooth(LEar_dist, smoothing),[],@sum);
REar_bins = accumarray([bins-min(bins)+1], smooth(REar_dist, smoothing),[],@sum);

figure
plot(nose_bins(2:end))
hold on
plot(tail_bins(2:end))
plot(LEar_bins(2:end))
plot(REar_bins(2:end))
legend(bodyparts)
hold off





