filename = "C:\Users\mpanagi\Downloads\2020-01-27_421881_m3-2DeepCut_resnetNone_DOI_LocomotorFeb14shuffle1_1030000.csv";
filename = "C:\Users\mpanagi\Downloads\2020-01-27_421881_m3-1TDeepCut_resnetNone_DOI_LocomotorFeb14shuffle1_1030000.csv";
%DLC_RawRead
[data, bodyparts] = DLC_RawRead(filename);

% Box Parts
nose = [2:4];
head2 = [5:7];
head1 = [8:10];
lear = [11:13];
rear = [14:16];
mid1 = [17:19];
mid2 = [20:22];
tail = [23:25];
boxtopleft = [26:28];
boxtopmiddle = [29:31];
boxtopright = [32:34];
boxrightmiddle = [35:37];
boxbottomright = [38:40];
boxbottommiddle = [41:43];
boxbottomleft = [44:46];
boxleftmiddle = [47:49];
objecttopleft = [50:52];
objecttopright = [53:55];
objectshaft = [56:58];
object2topleft = [59:61];
object2topright = [62:64];
object2shaft = [65:67];
%
x = 1;
y = 2;
confidence = 3;


%
rows = find(data(:, boxtopleft(confidence)) == 1);
boxtopleft_fixed(x) = median(data(rows, boxtopleft(x)));
boxtopleft_fixed(y) = median(data(rows, boxtopleft(y)));

rows = find(data(:, boxtopright(confidence)) == 1);
boxtopright_fixed(x) = median(data(rows, boxtopright(x)));
boxtopright_fixed(y) = median(data(rows, boxtopright(y)));

rows = find(data(:, boxbottomright(confidence)) == 1);
boxbottomright_fixed(x) = median(data(rows, boxbottomright(x)));
boxbottomright_fixed(y) = median(data(rows, boxbottomright(y)));

rows = find(data(:, boxbottomleft(confidence)) == 1);
boxbottomleft_fixed(x) = median(data(rows, boxbottomleft(x)));
boxbottomleft_fixed(y) = median(data(rows, boxbottomleft(y)));



topdist = sqrt((boxtopright_fixed(x) - boxtopleft_fixed(x))^2 + (boxtopright_fixed(y) - boxtopleft_fixed(y))^2)/42;
bottomdist = sqrt((boxbottomright_fixed(x) - boxbottomleft_fixed(x))^2 + (boxbottomright_fixed(y) - boxbottomleft_fixed(y))^2)/42;
leftdist = sqrt((boxtopleft_fixed(x) - boxbottomleft_fixed(x))^2 + (boxtopleft_fixed(y) - boxbottomleft_fixed(y))^2)/20;
rightdist = sqrt((boxtopright_fixed(x) - boxbottomright_fixed(x))^2 + (boxtopright_fixed(y) - boxbottomright_fixed(y))^2)/20;

px2cm = [topdist + bottomdist + leftdist + rightdist]/4;
distancefromwall = 5;

inner_topleft = [(boxtopleft_fixed(x) +(px2cm*distancefromwall)), (boxtopleft_fixed(y) +(px2cm*distancefromwall))];
inner_topright = [(boxtopright_fixed(x) - (px2cm*distancefromwall)), (boxtopright_fixed(y) +(px2cm*distancefromwall))];
inner_bottomleft = [(boxbottomleft_fixed(x) +(px2cm*distancefromwall)), (boxbottomleft_fixed(y) -(px2cm*distancefromwall))];
inner_bottomright = [(boxbottomright_fixed(x) - (px2cm*distancefromwall)), (boxbottomright_fixed(y) -(px2cm*distancefromwall))];

polygonx = [inner_topleft(x), inner_topright(x), inner_bottomleft(x), inner_bottomright(x)];
polygony = [inner_topleft(y), inner_topright(y), inner_bottomleft(y), inner_bottomright(y)];


% DLC_bodypartsinpolygon.m
occupancy = DLC_bodypartsinpolygon(data, polygonx, polygony);
plot(data(occupancy(:,3), head2(x)), data(occupancy(:,3), head2(y)))


%% image is flipped top<->bottom
plot(data(:, boxtopleft(x)), data(:, boxtopleft(y)))
hold on
plot(data(:, boxtopright(x)), data(:, boxtopright(y)))
plot(data(:, boxbottomleft(x)), data(:, boxbottomleft(y)))
plot(data(:, boxbottomright(x)), data(:, boxbottomright(y)))
hold off
%% Linear interpolation of low confidence data
criterion = 0.999;
[filtered, percentNaNs] = DLC_interpolateLowConfidence(data,criterion);

%% Due to encoding methods, it is necessary to process videos with the
% ffmpeg codec to convert putative frame numbers into time stamps (ts)
% Function is available here: https://uk.mathworks.com/matlabcentral/fileexchange/61235-video-frame-time-stamps
% however you need to install the codec first from here: https://www.ffmpeg.org/
% N.B. you have to provide the filepath to ffmpeg.exe in the function

ffmpegpath = 'C:\Users\mpanagi\Downloads\ffmpeg-20190805-5ac28e9-win64-static\ffmpeg-20190805-5ac28e9-win64-static\bin';
videofilepath = 'PUT IT HERE!!!!';
ts = videoframets(ffmpegpath, videofilepath);

%%
%[distance] = DLC_distancetravelled(DLC_data)

distance = DLC_distancetravelled(filtered);


