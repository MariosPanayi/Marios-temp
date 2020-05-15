% load('D:\OneDrive\Marios_PaperWriting\LY354740_RatDopamineGlutamateInteractionsFCV\Anaesthetized\05_baseline_pre.mat');

%% Plot final recording from baseline period post injection of Drug i.e. 30 mins post injection
close all
%Trial number
j = 10;

lines.point_number = 0;
lines.scan_number = 21;
lines.bg = 0;
lines.plotlines = 1;

limits = [-6,12];

plotlims = [31:100];
rewarddeliveryscan = 51;
plotts = [1:length(plotlims)] - [rewarddeliveryscan-plotlims(1)+1];

voltagesweep = [[-.4:(1.7/499):1.3] flip([-.4:(1.7/499):1.3])];

limits = [-5,9];
% Candidate pair 1 ~ 3 nA peak
%Sal - {'M1059153','20180731','SAL'}
subplot(3,1,1)
i = 10;
plotdata = smoothdata(data(i).processed.processed_data{j},2);
plot_fcvdata_Publication(plotdata(:,plotlims),plotts/10,lines, limits)
            c = colorbar('eastoutside');
            ylabel(c,'Current(nA)')
title('Saline')

subplot(3,1,2)
plot(smooth(data(i).processed.c_predicted{j}(1,:)))
ylim([-2 5])

subplot(3,1,3)
plot(voltagesweep, plotdata(:,61))
ylim([-2 5])

figure
%LY {'M01043738','20180706','LY'}
subplot(3,1,1)
i =4;
plotdata = smoothdata(data(i).processed.processed_data{j},2);
plot_fcvdata_Publication(plotdata(:,plotlims),plotts/10,lines, limits)
            c = colorbar('eastoutside');
            ylabel(c,'Current(nA)')
title('LY')

subplot(3,1,2)
plot(smooth(data(i).processed.c_predicted{j}(1,:)))
ylim([-2 5])

subplot(3,1,3)
plot(voltagesweep, plotdata(:,61))
ylim([-2 5])

% 
% figure
% 
% limits = [-9,12];
% % Candidate pair 2 - ~7 nA peak
% %Sal
%  i = 7;
% 
% plotdata = smoothdata(data(i).processed.processed_data{j},2);
% plot_fcvdata_Publication(plotdata(:,plotlims),plotts,lines, limits)
% title('Saline')
% 
% figure
% %LY
%  i =3;
%  plotdata = smoothdata(data(i).processed.processed_data{j},2);
% plot_fcvdata_Publication(plotdata(:,plotlims),plotts,lines, limits)
% title('LY')

%%
% Save representative traces

% Saline
%Trial 10 - end of the post injection baseline period
j = 10;

lines.point_number = 0;
lines.scan_number = 21;
lines.bg = 0;
lines.plotlines = 1;

limits = [-6,12];

plotlims = [31:100];
rewarddeliveryscan = 51;
plotts = [1:length(plotlims)] - [rewarddeliveryscan-plotlims(1)+1];

voltagesweep = [[-.4:(1.7/499):1.3] flip([-.4:(1.7/499):1.3])];

limits = [-5,9];
% Candidate pair 1 ~ 3 nA peak
%Sal - {'M1059153','20180731','SAL'}
figure
i = 10;
plotdata = smoothdata(data(i).processed.processed_data{j},2);
plot_fcvdata_Publication(plotdata(:,plotlims),plotts/10,lines, limits)
            c = colorbar('eastoutside');
            ylabel(c,'Current(nA)')

Saline_cpredicted = smooth(data(i).processed.c_predicted{j}(1,plotlims));
Saline_CVmax = plotdata(:,61);

savefolder = 'C:\Users\mario\Documents\GitHub\Marios-temp\LY354740_Rat\Anaesthetized';
filename = 'LY354740_M1059153_SAL_BaselinePost_Trial10_Colourplot.svg';
% filename = 'LY354740_M1059153_SAL_BaselinePost_Trial10_Colourplot.png';
ax = gca;
saveas(ax,[savefolder, '\',filename])


figure
%LY {'M01043738','20180706','LY'}

i =4;
plotdata = smoothdata(data(i).processed.processed_data{j},2);
plot_fcvdata_Publication(plotdata(:,plotlims),plotts/10,lines, limits)
            c = colorbar('eastoutside');
            ylabel(c,'Current(nA)')
            
LY_cpredicted= smooth(data(i).processed.c_predicted{j}(1,plotlims));
LY_CVmax = plotdata(:,61);
savefolder = 'C:\Users\mario\Documents\GitHub\Marios-temp\LY354740_Rat\Anaesthetized';
filename = 'LY354740_M01043738_LY_BaselinePost_Trial10_Colourplot.svg';
% filename = 'LY354740_M01043738_LY_BaselinePost_Trial10_Colourplot.png';

ax = gca;
saveas(ax,[savefolder, '\',filename])

%


%% Save traces

savefolder = 'C:\Users\mario\Documents\GitHub\Marios-temp\LY354740_Rat\Anaesthetized';
filename = 'LY354740_Anaesthetized_representativeTraces.xlsx';


representative_Cpred = [plotts/10; Saline_cpredicted'; LY_cpredicted']';


representative_CV = [voltagesweep;Saline_CVmax';LY_CVmax']';

writematrix(representative_Cpred,[savefolder, '\',filename],'Sheet',1);
writematrix(representative_CV,[savefolder, '\',filename],'Sheet',2);
% writematrix(Saline_ColourplotData,[savefolder, '\',filename],'Sheet',3);
% writematrix(LY_ColourplotData,[savefolder, '\',filename],'Sheet',4);