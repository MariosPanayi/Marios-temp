function AABplot(Means,SEMs,limits,figLabels,YaxisLabel,trials,plotpanel)
 %Plotting function for AAB bar graphs

% Means = Data matrix to plot bar height, with rows x cols = Trials x Groups/Conditions
% SEMs = Data matrix with error bar data in same format as Means
% limits = [0 4]; %y axis limits
% YaxisLabel = 'Distance (30s)';
% trials = 3; %number of trials in the test
% plotpanel = [2 3]; %how to panel plots of separate groups/conditions
% groupLabels = {'wt same','wt diff','het same','het diff','ko same','ko diff'};
% figLabels = reordercats(categorical(groupLabels),groupLabels);
 

%Plot with all conditions on the same graph, new panel for each trial
figure
for i = 1: trials
    subplot(1,trials,i)
    bar(figLabels, Means(i,:))
    hold on
    errorbar(figLabels, Means(i,:), SEMs(i,:), '.')
    title(sprintf('Trial %d', i))
    ylabel(YaxisLabel)
    ylim(limits)
    hold off
end


%Plot with all trials on one graph, different panel per group
figure
for i = 1:size(Means,2)
subplot(plotpanel(1), plotpanel(2),i)
bar(Means(:,i))
hold on
errorbar(Means(:,i), SEMs(:,i), '.')
title(string(figLabels(i)))
ylabel(YaxisLabel)
ylim(limits)
hold off
end
