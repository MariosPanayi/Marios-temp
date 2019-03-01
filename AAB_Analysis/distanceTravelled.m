function [Distance, Confidence, framenum, Distance_labels] = distanceTravelled(data, smoothxy)

% No smoothing of xy co-ordinates, set smoothxy = 1


nose = [diff(smooth(data(:,2),smoothxy)),smooth(diff(data(:,3)),smoothxy)];
nose_dist = sqrt(nose(:,1).^2 + nose(:,1).^2);

Tail = [smooth(diff(data(:,5)),smoothxy),smooth(diff(data(:,6)),smoothxy)];
tail_dist = sqrt(Tail(:,1).^2 + Tail(:,1).^2);

LEar = [smooth(diff(data(:,8)),smoothxy),smooth(diff(data(:,9)),smoothxy)];
LEar_dist = sqrt(LEar(:,1).^2 + LEar(:,1).^2);

REar = [smooth(diff(data(:,11)),smoothxy),smooth(diff(data(:,12)),smoothxy)];
REar_dist = sqrt(REar(:,1).^2 + REar(:,1).^2);

avg_xy = [mean([data(:,2),data(:,5),data(:,8),data(:,11)],2), mean([data(:,3),data(:,6),data(:,9),data(:,12)],2)];
avg = [diff(smooth(avg_xy(:,1),smoothxy)),smooth(diff(avg_xy(:,2)),smoothxy)];
avg_dist = sqrt(avg(:,1).^2 + avg(:,1).^2);

head_xy  = [mean([data(:,2),data(:,8),data(:,11)],2), mean([data(:,3),data(:,9),data(:,12)],2)];
head = [diff(smooth(head_xy(:,1),smoothxy)),smooth(diff(head_xy(:,2)),smoothxy)];
head_dist = sqrt(head(:,1).^2 + head(:,1).^2);


framenum = data(2:end,1);

Distance = [nose_dist, tail_dist, LEar_dist, REar_dist, avg_dist, head_dist];
Confidence = [data(:,4),data(:,7),data(:,10),data(:,13)];
Distance_labels = {'nose', 'tail', 'LEar', 'REar', 'avg', 'head'};