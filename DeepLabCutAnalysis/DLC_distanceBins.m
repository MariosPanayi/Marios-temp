function [Distance_Bins] = DLC_distanceBins(Distance, fps, bin_duration, framesSmoothing)

% Input the distance travelled per frame as data (Distance)
% Input the frame rate as fps
% Input the desired time duration in seconds e.g. bin_duration = 5 will
% give you 5s bins
% framesSmoothing will let you mean filter across a desired number of
% frames prior to binning the data (set fps and bin_duration to 1 if you
% want to see the effect of just smoothing the data)


binwidth = fps*bin_duration;
bins = ceil([1:size(Distance,1)]/binwidth)';

for i = 1:size(Distance,2)
Distance_Bins(:,i) = accumarray(bins, smooth(Distance(:,i), framesSmoothing),[],@mean);
Distance_Bins(:,i) = Distance_Bins(:,i).*binwidth;
end
Bins = 1:size(Distance_Bins,1);
Distance_Bins(:,(size(Distance,2)+1)) = Bins;