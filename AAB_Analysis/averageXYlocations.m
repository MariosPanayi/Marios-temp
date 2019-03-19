function [avgXLocations, avgYLocations, bins] = averageXYlocations(Xdata,Ydata,fps, bin_duration)

binwidth = fps*bin_duration;
bins = ceil([1:size(Xdata,1)]/binwidth)';


for i = 1:size(Xdata,2)
X_Bins(:,i) = accumarray(bins,Xdata(:,i),[],@mean);
Y_Bins(:,i) = accumarray(bins,Ydata(:,i),[],@mean);
end

Bins = 1:size(X_Bins,1);


for i = Bins
    index = find(bins == i);
    Size = length(index);
    avgXLocations(index,:) = repmat(X_Bins(i,:),Size,1);
    avgYLocations(index,:) = repmat(Y_Bins(i,:),Size,1);
end
