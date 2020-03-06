% Raw FCV Tarheel data folders directory and subfolder paths
datadir = 'E:\EmilFristedMScDBMEW\Awake_data\RawData';
folderpaths = {'29\20130227_SAL',
'29\20130301_LY',
'32\20130227_LY',
'32\20130301_SAL',
'34\20130227_LY',
'34\20130301_SAL',
'52\20130911_SAL',
'52\20130913_LY',
'54\20140617_LY',
'54\20140619_SAL',
'55\20140617_SAL',
'55\20140619_LY',
'56\20140617_LY',
'56\20140619_SAL',
'57\20140618_SAL',
'57\20140620_LY',
'58\20140618_LY',
'58\20140620_SAL',
'69\20141209_LY',
'69\20141211_SAL',
'71\20141212_SAL',
'71\20141216_LY',
'72\20141210_LY',
'72\20141212_SAL'};

subfolderspaths = {'29_20130227_vi60\rew_alltrials_uncut',
'29_20130301_vi60\rew_alltrials_uncut',
'32_20130227_vi60\rew_alltrials_uncut',
'32_20130301_vi60\rew_alltrials_uncut',
'34_20130227_vi60\rew_alltrials_uncut',
'34_20130301_vi60\rew_alltrials_uncut',
'0052_20130911_vi60\rew_alltrials_uncut',
'0052_20130913_vi60\rew_alltrials_uncut',
'54_20140617_vi60\rew_alltrials_uncut',
'54_20140619_vi60\rew_alltrials_uncut',
'55_20140617_vi60\rew_alltrials_uncut',
'55_20140619_vi60\rew_alltrials_uncut',
'56_20140617_vi60\rew_alltrials_uncut',
'56_20140619_vi60\rew_alltrials_uncut',
'57_20140618_vi60\rew_alltrials_uncut',
'57_20140620_vi60\rew_alltrials_uncut',
'58_20140618_vi60\rew_alltrials_uncut',
'58_20140620_vi60\rew_alltrials_uncut',
'69_ 20141209_vi60\rew_alltrials_uncut',
'69_20141211_vi60\rew_alltrials_uncut',
'71_20141212_vi60\rew_alltrials_uncut',
'71_20141216_vi60\rew_alltrials_uncut',
'72_20141210_vi60\rew_alltrials_uncut',
'72_20141212_vi60\rew_alltrials_uncut'};


% Included data - Rat Num, Date, ChannelNum, Drug
inclusion = {29,	20130227,	0,	'SAL';
29,	20130301,	0,	'LY';
32,	20130227,	1,	'LY';
32,	20130301,	1,	'SAL';
34,	20130227,	0,	'LY';
34,	20130227,	1,	'LY';
34,	20130301,	0,	'SAL';
34,	20130301,	1,	'SAL';
52,	20130911,	0,	'SAL';
52,	20130911,	1,	'SAL';
52,	20130913,	0,	'LY';
52,	20130913,	1,	'LY';
54,	20140617,	1,	'LY';
54,	20140619,	1,	'SAL';
55,	20140617,	1,	'SAL';
55,	20140619,	1,	'LY';
56,	20140617,	0,	'LY';
56,	20140617,	1,	'LY';
56,	20140619,	0,	'SAL';
56,	20140619,	1,	'SAL';
57,	20140618,	0,	'SAL';
57,	20140618,	1,	'SAL';
57,	20140620,	0,	'LY';
57,	20140620,	1,	'LY';
58,	20140618,	0,	'LY';
58,	20140618,	1,	'LY';
58,	20140620,	0,	'SAL';
58,	20140620,	1,	'SAL';
69,	20141209,	0,	'LY';
69,	20141211,	0,	'SAL';
71,	20141212,	0,	'SAL';
71,	20141216,	0,	'LY';
72,	20141210,	0,	'LY';
72,	20141210,	1,	'LY';
72,	20141212,	0,	'SAL';
72,	20141212,	1,	'SAL'};


% Concatenate all data folders
for i = 1:length(folderpaths)
fulldatapaths{i} = [datadir,'\', folderpaths{i}, '\', subfolderspaths{i}, '\'];
end
fulldatapaths = fulldatapaths';

%% Read Data into Data Array

no_of_channels = 2;

for i = 1:length(folderpaths)
temp = split(folderpaths{i}, '\')';
data(i).subject = temp{1};
temp = split(temp{2}, '_');
data(i).date = temp{1};
data(i).drug = temp{2};

[data(i).TTLs, data(i).ch0_fcv_data, data(i).ch1_fcv_data, data(i).ts] = read_whole_tarheel_session(fulldatapaths{i}, no_of_channels);
end
%% Cut data around relevant TTLs


