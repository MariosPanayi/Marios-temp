[pks,locs,w,p] = findpeaks(data)

findpeaks(smooth(A(:,2), 5), 'MinPeakProminence', 0.05,  'Annotate','extents')