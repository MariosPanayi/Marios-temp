function [data_bins] = DLC_dataBins(data, fps, bin_duration, framesSmoothing)

% Input the processed datafram (e.g. distance travelled per body part - rows = frames, cols = body part)
% Input the frame rate as fps
% Input the desired time duration in seconds e.g. bin_duration = 5 will
% give you 5s bins
% framesSmoothing will let you mean filter across a desired number of
% frames prior to binning the data (set fps and bin_duration to 1 if you
% want to see the effect of just smoothing the data)


binwidth = fps*bin_duration;
bins = ceil([1:size(data,1)]/binwidth)';

for i = 1:size(data,2)
data_bins(:,i) = accumarray(bins, smooth(data(:,i), framesSmoothing),[],@mean);
data_bins(:,i) = data_bins(:,i).*binwidth;
end
Bins = 1:size(data_bins,1);
data_bins(:,(size(data,2)+1)) = Bins;