% Load Processed data first if necessary, or re-extract from raw data
tic
reExtractData = 0;
reLoadData = 1;

if reExtractData
    TB1_VarRewardAwakeFCV_ExtractData
    
elseif reLoadData
    load("C:\Users\mario\Documents\GitHub\Marios-temp\LY354740_Rat\TB1Awake_VarReward\LY354740_Rat_awake_data.mat")
    
end
toc
%%
close all

% Plot summary of data before saving
%Plot params
lines.point_number = 0;
lines.scan_number = 22;
lines.bg = 0;
lines.plotlines = 1;

limits = [-2.5,3.5];


plotlims = [30:150];
rewarddeliveryscan = 51;
plotts = [1:length(plotlims)] - [rewarddeliveryscan-plotlims(1)+1];


voltagesweep = [[-.4:(1.7/249):1.3] flip([-.4:(1.7/249):1.3])];



%Plot Saline data
subplot(3,2,1)
i = 20;
j = 1;
plotdata = smoothdata(data(i).cut.processed_data{j},2);
plot_fcvdata_Publication(plotdata(:, plotlims),plotts,lines, limits)
            c = colorbar('eastoutside');
            ylabel(c,'Current(nA)')


subplot(3,2,3)
plotpredicted = smoothdata(data(i).chemo.c_predicted{j}(1,:),2);
plot(plotts, plotpredicted(plotlims))
xlim([plotts(1) plotts(end)])
ylim([-1 2])

subplot(3,2,5)
% find peak location in data and extract CV from data

plotCVData = data(i).cut.processed_data{j};
[maxval, maxloc] = max(plotpredicted(rewarddeliveryscan:rewarddeliveryscan+50));
plot(voltagesweep, smooth(plotdata(:,maxloc+rewarddeliveryscan-1)))
xlim([-.4 1.3])
ylim([-1 2])



%Plot LY Data

subplot(3,2,2)
i =19;
%j = 3;
plotdata = smoothdata(data(i).cut.processed_data{j},2);
plot_fcvdata_Publication(plotdata(:, plotlims),plotts,lines, limits)
            c = colorbar('eastoutside');
            ylabel(c,'Current(nA)')
            
subplot(3,2,4)
plotpredicted = smoothdata(data(i).chemo.c_predicted{j}(1,:),2);
plot(plotts, plotpredicted(plotlims))
xlim([plotts(1) plotts(end)])
ylim([-1 2])

subplot(3,2,6)
% find peak location in data and extract CV from data
[maxval, maxloc] = max(plotpredicted(rewarddeliveryscan:rewarddeliveryscan+50));
plot(voltagesweep, smooth(plotdata(:,maxloc+rewarddeliveryscan)))
xlim([-.4 1.3])
ylim([-1 2])





%% Save Colour Plots
% Rat 69 CH0 Reward number 1 reward Magnitude Medium (2 pellets)
% Plot summary of data before saving
%Plot params
lines.point_number = 0;
lines.scan_number = 22;
lines.bg = 0;
lines.plotlines = 1;

limits = [-2.5,3.5];


plotlims = [30:150];
rewarddeliveryscan = 51;
plotts = [1:length(plotlims)] - [rewarddeliveryscan-plotlims(1)+1];


voltagesweep = [[-.4:(1.7/249):1.3] flip([-.4:(1.7/249):1.3])];

figure;

% Saline
i = 20;
j = 1;
plotdata = smoothdata(data(i).cut.processed_data{j},2);
plot_fcvdata_Publication(plotdata(:, plotlims),plotts/10,lines, limits)
xticks = [0:10:length(plotts)];
            c = colorbar('eastoutside');
            ylabel(c,'Current(nA)')
       
Saline_ColourplotData = plotdata;
Saline_cpredicted =  smoothdata(data(i).chemo.c_predicted{j}(1,:),2);
Saline_cpredicted = Saline_cpredicted(plotlims);

Saline_CVmax = smooth(plotdata(:,59));
            
savefolder = 'C:\Users\mario\Documents\GitHub\Marios-temp\LY354740_Rat\TB1Awake_VarReward';
filename = 'LY354740_Rat69_Ch0_SAL_Colourplot.svg';


ax = gca;
saveas(ax,[savefolder, '\',filename])


figure;

% LY
i = 19;
j = 1;
plotdata = smoothdata(data(i).cut.processed_data{j},2);
plot_fcvdata_Publication(plotdata(:, plotlims),plotts/10,lines, limits)
xticks = [0:10:length(plotts)];
            c = colorbar('eastoutside');
            ylabel(c,'Current(nA)')

LY_ColourplotData = plotdata;       
LY_cpredicted =  smoothdata(data(i).chemo.c_predicted{j}(1,:),2);
LY_cpredicted = LY_cpredicted(plotlims);

LY_CVmax = smooth(plotdata(:,61));
 
            
savefolder = 'C:\Users\mario\Documents\GitHub\Marios-temp\LY354740_Rat\TB1Awake_VarReward';
filename = 'LY354740_Rat69_Ch0_LY_Colourplot.svg';


ax = gca;
saveas(ax,[savefolder, '\',filename])

%% Save traces

savefolder = 'C:\Users\mario\Documents\GitHub\Marios-temp\LY354740_Rat\TB1Awake_VarReward';
filename = 'LY354740_Rat69_Ch0_LY_representativeTraces.xlsx';


representative_Cpred = [plotts/10; Saline_cpredicted; LY_cpredicted]';


representative_CV = [voltagesweep;Saline_CVmax';LY_CVmax']';

writematrix(representative_Cpred,[savefolder, '\',filename],'Sheet',1);
writematrix(representative_CV,[savefolder, '\',filename],'Sheet',2);
writematrix(Saline_ColourplotData,[savefolder, '\',filename],'Sheet',3);
writematrix(LY_ColourplotData,[savefolder, '\',filename],'Sheet',4);
