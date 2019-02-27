


path = 'C:\Users\mpanagi\Documents\GitHub\Marios-temp\AAB_Analysis\';
filename = 'Test 1DeepCut_resnet50_AABFeb18shuffle1_84600.csv';

startRow = 4;
data = csvread([path,filename],startRow);


% Header from file
% scorer	DeepCut_resnet50_AABFeb18shuffle1_225000	DeepCut_resnet50_AABFeb18shuffle1_225000	DeepCut_resnet50_AABFeb18shuffle1_225000	DeepCut_resnet50_AABFeb18shuffle1_225000	DeepCut_resnet50_AABFeb18shuffle1_225000	DeepCut_resnet50_AABFeb18shuffle1_225000	DeepCut_resnet50_AABFeb18shuffle1_225000	DeepCut_resnet50_AABFeb18shuffle1_225000	DeepCut_resnet50_AABFeb18shuffle1_225000	DeepCut_resnet50_AABFeb18shuffle1_225000	DeepCut_resnet50_AABFeb18shuffle1_225000	DeepCut_resnet50_AABFeb18shuffle1_225000
% bodyparts	nose	nose	nose	tail	tail	tail	rightear	rightear	rightear	leftear	leftear	leftear
% coords	x	y	likelihood	x	y	likelihood	x	y	likelihood	x	y	likelihood


figure
plot(data(:,2),data(:,3))
hold on
plot(data(:,5),data(:,6))
plot(data(:,8),data(:,9))
plot(data(:,11),data(:,12))
legend({'Nose', 'Tail', 'Left ear', 'Right ear'})
hold off

% convert to distance\

nose = [diff(data(:,2)),diff(data(:,3))];
nose_dist = sqrt(nose(:,1).^2 + nose(:,1).^2);

Tail = [diff(data(:,5)),diff(data(:,6))];
tail_dist = sqrt(Tail(:,1).^2 + Tail(:,1).^2);

LEar = [diff(data(:,8)),diff(data(:,9))];
LEar_dist = sqrt(LEar(:,1).^2 + LEar(:,1).^2);

REar = [diff(data(:,11)),diff(data(:,12))];
REar_dist = sqrt(REar(:,1).^2 + REar(:,1).^2);

Distance_300 = [nose_dist, tail_dist, LEar_dist, REar_dist];
Confidence_300 = [data(:,4),data(:,7),data(:,10),data(:,13)];

all_parts = [nose_dist,tail_dist,LEar_dist,REar_dist];
corr(all_parts)


smoothing = 500;
figure
plot(smooth(nose_dist(180:end,:),smoothing))
hold on
plot(smooth(tail_dist(180:end,:),smoothing))
plot(smooth(LEar_dist(180:end,:),smoothing))
plot(smooth(REar_dist(180:end,:),smoothing))
plot(smooth(mean(all_parts(180:end,:),2),smoothing))
legend({'Nose', 'Tail', 'Left ear', 'Right ear','Average'})
hold off




