function [data, cutoff_frame] = DLC_prunestart(dataraw, points, likelihood)

% Prune first ~5s of anymaze video when it contains empty frames
% This function uses the confidence measure from deep lab cut to identify
% the first point at which the network is confident of the tracking, and
% throws these first bits out. N.B. The column containing the frame number
% is still left in the original units, so use cutoff_frame to identify what
% the true first frame is.
% Default confidence cutoff is hardcoded as pcutoff = 0.9, based on the DLC
% training criteria

% pcutoff = 0.9;
%
% a = find(dataraw(:,4) > pcutoff , 1, 'first');
% b = find(dataraw(:,7) > pcutoff , 1, 'first');
% c = find(dataraw(:,10) > pcutoff , 1, 'first');
% d = find(dataraw(:,13) > pcutoff , 1, 'first');
% cutoff_frame = max([a b c d]);


for i = 1:length(points)
    index = ((points(i)-1)*3 + likelihood + 1);
    a(i) = find(dataraw(:,index) == 1, 1, 'first');
end
cutoff_frame = max(a);

% a = find(dataraw(:,4) == 1 , 1, 'first');
% b = find(dataraw(:,7) == 1 , 1, 'first');
% c = find(dataraw(:,10) == 1 , 1, 'first');
% d = find(dataraw(:,13) == 1 , 1, 'first');
% cutoff_frame = max([a b c d]);


data = dataraw((cutoff_frame):end,:);