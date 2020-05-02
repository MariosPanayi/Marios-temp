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
%%
% Create new folder for final data analysis
% Use C_predicted signal [row 1 = DA]
% 0.5s moving window smooth, 10Hz sampling rate so 5 ts = 0.5s
% Calculate AUC, Peak, Latency to peak within 5s of event
% AUC must be taken relative to the background DA, so first determine a
% baseline DA,

% Exclusion criteria for trial data if more than 10% fo the toal data is
% NaNs, or if more than 5% of conseuctive data is NaNs. from:
% Rodeberg, N. T., Sandberg, S. G., Johnson, J. A., Phillips, P. E., & Wightman, R. M. (2017). Hitchhiker's Guide to Voltammetry: Acute and Chronic Electrodes for in Vivo Fast-Scan Cyclic Voltammetry. ACS chemical neuroscience, 8(2), 221–234. https://doi.org/10.1021/acschemneuro.6b00393

calculate_t50 = 0;

    percentNanExclude = 0.1;
    percentNanConsecutiveExclude = 0.05;
for i = 1:length(data)
    for j = 1:length(data(i).cut.data)
        %Smooth data 0.5s filter
        data(i).analysis.smoothtraces{j} = smooth(data(i).chemo.q_cutoff{1,j}(1,:),5);
        data(i).analysis.traces{j} = data(i).chemo.q_cutoff{1,j}(1,:)';
        data(i).analysis.percentNaN(j) = sum(isnan(data(i).chemo.q_cutoff{1,j}(1,:)'))/length(data(i).chemo.q_cutoff{1,j}(1,:)');
        data(i).analysis.c_predicted{j} = data(i).chemo.c_predicted{1,j}(1,:)';
        data(i).analysis.rewardMagnitude(j) = data(i).cut.rewardMagnitude{j};
        data(i).analysis.trialNum(j) =  data(i).cut.trialNum{j};
        
        %Identify 1s pre baseline and 5s post baseline relative to cut parameters
        %To account for NaNs affecting the sum, the mean across the time
        %period is calculated and then multiplied by the rleveant number of ts
        cutbaseline = [(cut_params.time_align(1)-1)*cut_params.sample_rate+1:cut_params.time_align(1)*cut_params.sample_rate];
        cutAUC =  [cut_params.time_align(1)*cut_params.sample_rate+1:(cut_params.time_align(1)+5)*cut_params.sample_rate];
        
        
        tracetime = [(1:length(data(i).analysis.traces{j}))/cut_params.sample_rate];
        data(i).analysis.AUC5s(j) = trapz(data(i).analysis.traces{j}(cutAUC));
        data(i).analysis.BaselineAUC1s(j) = trapz(data(i).analysis.traces{j}(cutbaseline));
        data(i).analysis.AUC5s_baselineCorrected(j) = data(i).analysis.AUC5s(j) - data(i).analysis.BaselineAUC1s(j)*5;
        
        data(i).analysis.percentNaN_reward5s(j) = sum(isnan(data(i).analysis.traces{j}(cutAUC)))/length(data(i).analysis.traces{j}(cutAUC));
        data(i).analysis.percentNaN_consecutive_reward5s(j) = max(consecutiveones(isnan(data(i).analysis.traces{j}(cutAUC))))/length(data(i).analysis.traces{j}(cutAUC));
        
        data(i).analysis.trialexclude(j) =  data(i).analysis.percentNaN_reward5s(j) > percentNanExclude || data(i).analysis.percentNaN_consecutive_reward5s(j) > percentNanConsecutiveExclude;
        %Identify Max signal in 5s and latency of this peak
        [data(i).analysis.peak5s(j), data(i).analysis.latency2peak5s(j)] = max(data(i).analysis.traces{j}(cutAUC));
        %Convert latency to seconds
        data(i).analysis.latency2peak5s(j) = data(i).analysis.latency2peak5s(j)/cut_params.sample_rate;
        
        %t50 analysis when possible
        if calculate_t50
            t50plotfigs = 0;
            t50pk_threshold = 0.85;
            [data(i).analysis.t50.t50_data(j), data(i).analysis.t50.rsq_data(j), data(i).analysis.t50.datapks(j), data(i).analysis.t50.datapklocs(j), data(i).analysis.t50.peak_intercepts(j)] = ...
                aFCV_Analyze_t50(data(i).analysis.traces{j}(cutAUC), t50plotfigs, t50pk_threshold );
        end
        % Separate analysis on c_predicted data to compare with original analysis
        
        tracetime = [(1:length(data(i).analysis.traces{j}))/cut_params.sample_rate];
        data(i).analysis.AUC5s(j) = trapz(data(i).analysis.c_predicted{j}(cutAUC));
        data(i).analysis.BaselineAUC1s(j) = trapz(data(i).analysis.c_predicted{j}(cutbaseline));
        data(i).analysis.AUC5s_baselineCorrected(j) = data(i).analysis.AUC5s(j) - data(i).analysis.BaselineAUC1s(j)*5;
        
        %Identify Max signal in 5s and latency of this peak
        [data(i).analysis.C_peak5s(j), data(i).analysis.C_latency2peak5s(j)] = max(data(i).analysis.c_predicted{j}(cutAUC));
        %Convert latency to seconds
        data(i).analysis.C_latency2peak5s(j) = data(i).analysis.C_latency2peak5s(j)/cut_params.sample_rate;
        
        %t50 analysis when possible
        if calculate_t50
            t50plotfigs = 0;
            t50pk_threshold = 0.85;
            [data(i).analysis.C_t50.t50_data(j), data(i).analysis.C_t50.rsq_data(j), data(i).analysis.C_t50.datapks(j), data(i).analysis.C_t50.datapklocs(j), data(i).analysis.C_t50.peak_intercepts(j)] = ...
                aFCV_Analyze_t50(data(i).analysis.c_predicted{j}(cutAUC), t50plotfigs, t50pk_threshold );
        end
        
    end
end

% Averages post filtering - smoothed then analyzed
for i = 1:length(data)
    
    temp_low_idx = setdiff(find(data(i).analysis.rewardMagnitude == 1), find(data(i).analysis.trialexclude));
    temp_med_idx = setdiff(find(data(i).analysis.rewardMagnitude == 2), find(data(i).analysis.trialexclude));
    temp_hig_idx = setdiff(find(data(i).analysis.rewardMagnitude == 4), find(data(i).analysis.trialexclude));
    
    
    try
        data(i).analysis.average.trace(:,1) = nanmean([data(i).analysis.traces{temp_low_idx}],2);
    catch
        data(i).analysis.average.trace(:,1) = NaN(size(data(i).analysis.traces,1),1);
    end
    try
        data(i).analysis.average.trace(:,2) = nanmean([data(i).analysis.traces{temp_med_idx}],2);
    catch
        data(i).analysis.average.trace(:,1) = NaN(size(data(i).analysis.traces,1),1);
    end
    try
        data(i).analysis.average.trace(:,3) = nanmean([data(i).analysis.traces{temp_hig_idx}],2);
    catch
        data(i).analysis.average.trace(:,1) = NaN(size(data(i).analysis.traces,1),1);
    end
    
    tracetime = [(1:length(data(i).analysis.average.trace))/cut_params.sample_rate];
    for j = 1:size(data(i).analysis.average.trace,2)
        data(i).analysis.average.smoothtrace(:,j) = smooth(data(i).analysis.average.trace(:,j),5);
    end
    
    cutbaseline = [(cut_params.time_align(1)-1)*cut_params.sample_rate+1:cut_params.time_align(1)*cut_params.sample_rate];
    cutAUC =  [cut_params.time_align(1)*cut_params.sample_rate+1:(cut_params.time_align(1)+5)*cut_params.sample_rate];
    
    for j = 1:size(data(i).analysis.average.smoothtrace,2)
        data(i).analysis.average.AUC(j) = trapz( data(i).analysis.average.smoothtrace(cutAUC,j));
        [data(i).analysis.average.peak(j), data(i).analysis.average.latency2peak(:,j)] = max(data(i).analysis.average.smoothtrace(cutAUC,j));
        data(i).analysis.average.latency2peak(j) = data(i).analysis.average.latency2peak(:,j)/cut_params.sample_rate;
    end
    
    
    %t50 analysis when possible
    if calculate_t50
        t50plotfigs = 0;
        t50pk_threshold = 0.85;
        [data(i).analysis.average.t50.t50_data(j), data(i).analysis.average.t50.rsq_data(j), data(i).analysis.average.t50.datapks(j), data(i).analysis.average.t50.datapklocs(j), data(i).analysis.average.t50.peak_intercepts(j)] = ...
            aFCV_Analyze_t50(data(i).analysis.average.smoothtrace(cutAUC,j), t50plotfigs, t50pk_threshold );
    end
end

% C_preds post filtering - smoothed then analyzed
for i = 1:length(data)
    
    temp_low_idx = find(data(i).analysis.rewardMagnitude == 1);
    temp_med_idx = find(data(i).analysis.rewardMagnitude == 2);
    temp_hig_idx = find(data(i).analysis.rewardMagnitude == 4);
    
    
    try
        data(i).analysis.C_pred.trace(:,1) = nanmean([data(i).analysis.c_predicted{temp_low_idx}],2);
    catch
        data(i).analysis.C_pred.trace(:,1) = NaN(size(data(i).analysis.c_predicted,1),1);
    end
    try
        data(i).analysis.C_pred.trace(:,2) = nanmean([data(i).analysis.c_predicted{temp_med_idx}],2);
    catch
        data(i).analysis.C_pred.trace(:,1) = NaN(size(data(i).analysis.c_predicted,1),1);
    end
    try
        data(i).analysis.C_pred.trace(:,3) = nanmean([data(i).analysis.c_predicted{temp_hig_idx}],2);
    catch
        data(i).analysis.C_pred.trace(:,1) = NaN(size(data(i).analysis.c_predicted,1),1);
    end
    
    tracetime = [(1:length(data(i).analysis.C_pred.trace))/cut_params.sample_rate];
    for j = 1:size(data(i).analysis.C_pred.trace,2)
        data(i).analysis.C_pred.smoothtrace(:,j) = smooth(data(i).analysis.C_pred.trace(:,j),5);
    end
    
    cutbaseline = [(cut_params.time_align(1)-1)*cut_params.sample_rate+1:cut_params.time_align(1)*cut_params.sample_rate];
    cutAUC =  [cut_params.time_align(1)*cut_params.sample_rate+1:(cut_params.time_align(1)+5)*cut_params.sample_rate];
    
    for j = 1:size(data(i).analysis.C_pred.smoothtrace,2)
        data(i).analysis.C_pred.AUC(j) = trapz(data(i).analysis.C_pred.smoothtrace(cutAUC,j));
        [data(i).analysis.C_pred.peak(j), data(i).analysis.C_pred.latency2peak(:,j)] = max(data(i).analysis.C_pred.smoothtrace(cutAUC,j));
        data(i).analysis.C_pred.latency2peak(j) = data(i).analysis.C_pred.latency2peak(:,j)/cut_params.sample_rate;
    end
    
    
    %t50 analysis when possible
    if calculate_t50
        t50plotfigs = 0;
        t50pk_threshold = 0.85;
        [data(i).analysis.C_pred.t50.t50_data(j), data(i).analysis.C_pred.t50.rsq_data(j), data(i).analysis.C_pred.t50.datapks(j), data(i).analysis.C_pred.t50.datapklocs(j), data(i).analysis.C_pred.t50.peak_intercepts(j)] = ...
            aFCV_Analyze_t50(data(i).analysis.C_pred.smoothtrace(cutAUC,j), t50plotfigs, t50pk_threshold );
    end
end

%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plot data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% for i = 1:length(data)
for i = 1:length(data)
    
    if data(i).include
        params.fig_title = strcat('Rat', data(i).uniqueID);
        
        sum_colourplot = zeros(size(data(i).cut.processed_data{1}));
        all_IvT = [];
        for j = 1:length(data(i).cut.processed_data)
            if ~ismember(j,params.trial_exclude_list)
                if params.plot_each
                    %plot each colour plot
                    figure
                    subplot(1,3,1)
                    plot_fcvdata(data(i).cut.processed_data{j},data(i).cut.ts{j})
                    c = colorbar('eastoutside');
                    ylabel(c,'Current(nA)')
                    if params.apply_chemometrics
                        title('Chemometric FCV data')
                    else
                        title('Raw FCV data')
                    end
                    
                    %plot I vs T
                    subplot(1,3,2)
                    if params.apply_chemometrics
                        plot(data(i).cut.ts{j},smooth(data(i).chemo.q_cutoff{1,j}(1,:),5),'k')
                        title('Chemometric I vs T');xlabel('Time(s)');ylabel('Current (nA)')
                    else
                        plot(data(i).cut.ts{j},smooth(data(i).cut.processed_data{j}(params.scan_number,:),5),'k')
                        title('I vs T');xlabel('Time(s)');ylabel('Current (nA)')
                    end
                    xlim([min(data(i).cut.ts{j}), max(data(i).cut.ts{j})]);
                    
                    %plot TTLS
                    subplot(1,3,3)
                    plot_TTLs(data(i).cut.TTLs{j}, data(i).cut.ts{j}, params.TTLnames)
                    title('TTLs');xlabel('Time(s)');ylabel('TTLs')
                    
                    figtitle = sprintf('Trial number %d', j);
                    suptitle(params.fig_title)
                end
                
                sum_colourplot = sum_colourplot+data(i).cut.processed_data{j};
                all_IvT(j,:) = data(i).chemo.q_cutoff{1,j}(1,:);
            end
        end
        
        if params.plot_avg
            %final plot, avg colour plot and individual i vs t
            h = figure;
            subplot(1,2,1)
            avg_colourplot = sum_colourplot/length(data(i).cut.processed_data);
            plot_fcvdata(avg_colourplot);
            c = colorbar('eastoutside');
            ylabel(c,'Current(nA)')
            title('Average Colour plot')
            subplot(1,2,2)
            plot(all_IvT')
            hold on
            plot(nanmean(all_IvT),'k','LineWidth', 2)
            title('I vs T');xlabel('Time(s)');ylabel('Current (nA)')
            set(gcf, 'Position', [300, 300, 1300, 500]);
            
            suptitle([params.fig_title]);
            
            figdirectory = 'C:\Users\mario\Documents\GitHub\Marios-temp\LY354740_Rat\AwakeBehaviour_figsforMark';
            figname = [figdirectory,'\' params.fig_title, '.png'];
            saveas(gcf, figname)
            close
        end
    end
    
end

%% Plot Mean of all signals
figure;

for i = 1:length(data)
    if data(i).include
        if strcmp(data(i).drug,"LY")
            hold on
            subplot(1,3,1)
            plot(mean([data(i).analysis.traces{find(data(i).analysis.rewardMagnitude == 1)}],2), 'g')
            title('Low');xlabel('Time(s)');ylabel('Current (nA)')
            hold on
            subplot(1,3,2)
            plot(mean([data(i).analysis.traces{find(data(i).analysis.rewardMagnitude == 2)}],2), 'g')
            title('Medium');xlabel('Time(s)');ylabel('Current (nA)')
            hold on
            subplot(1,3,3)
            plot(mean([data(i).analysis.traces{find(data(i).analysis.rewardMagnitude == 4)}],2), 'g')
            title('High');xlabel('Time(s)');ylabel('Current (nA)')
        elseif strcmp(data(i).drug,"SAL")
            hold on
            subplot(1,3,1)
            plot(mean([data(i).analysis.traces{find(data(i).analysis.rewardMagnitude == 1)}],2), 'b')
            hold on
            subplot(1,3,2)
            plot(mean([data(i).analysis.traces{find(data(i).analysis.rewardMagnitude == 2)}],2), 'b')
            hold on
            subplot(1,3,3)
            plot(mean([data(i).analysis.traces{find(data(i).analysis.rewardMagnitude == 4)}],2), 'b')
        end
    end
end

hold off
j = 0;
k = 0;
for i = 1:length(data)
    if data(i).include
        
        temp_low_idx = setdiff(find(data(i).analysis.rewardMagnitude == 1), find(data(i).analysis.trialexclude));
        temp_med_idx = setdiff(find(data(i).analysis.rewardMagnitude == 2), find(data(i).analysis.trialexclude));
        temp_hig_idx = setdiff(find(data(i).analysis.rewardMagnitude == 4), find(data(i).analysis.trialexclude));
        
        if strcmp(data(i).drug,"SAL")
            j = j + 1;
            
            try
                sal_low(:,j) = mean([data(i).analysis.c_predicted{temp_low_idx}],2);
            catch
                sal_low(:,j) = NaN(1,length(data(i).analysis.c_predicted{1,1}));
            end
            try
                sal_med(:,j) = mean([data(i).analysis.c_predicted{temp_med_idx}],2);
            catch
                sal_med(:,j) = NaN(1,length(data(i).analysis.c_predicted{1,1}));
            end
            try
                sal_hig(:,j) = mean([data(i).analysis.c_predicted{temp_hig_idx}],2);
            catch
                sal_hig(:,j) = NaN(1,length(data(i).analysis.c_predicted{1,1}));
            end
            
        elseif strcmp(data(i).drug,"LY")
            k = k + 1;
            try
                LY_low(:,k) = mean([data(i).analysis.c_predicted{temp_low_idx}],2);
            catch
                LY_low(:,k) = NaN(1,length(data(i).analysis.c_predicted{1,1}));
            end
            try
                LY_med(:,k) = mean([data(i).analysis.c_predicted{temp_med_idx}],2);
            catch
                LY_med(:,k) = NaN(1,length(data(i).analysis.c_predicted{1,1}));
            end
            try
                LY_hig(:,k) = mean([data(i).analysis.c_predicted{temp_hig_idx}],2);
            catch
                LY_hig(:,k) = NaN(1,length(data(i).analysis.c_predicted{1,1}));
            end
            
        end
    end
end





for i = 1:size(sal_low,2)
    sal_low(:,i) = smooth(sal_low(:,i) ,5);
    sal_med(:,i)  = smooth(sal_med(:,i) ,5);
    sal_hig(:,i)  = smooth(sal_hig(:,i) ,5);
end
for i = 1:size(LY_low,2)
    LY_low(:,i)  = smooth(LY_low(:,i) ,5);
    LY_med(:,i)  = smooth(LY_med(:,i) ,5);
    LY_hig(:,i)  = smooth(LY_hig(:,i) ,5);
end


figure;


subplot(1,3,1)
errorbar(nanmean(sal_low,2),nanstd(sal_low')/sqrt(size(sal_low,2)),'k','CapSize',0)
hold on
errorbar(nanmean(LY_low,2),nanstd(LY_low')/sqrt(size(LY_low,2)),'b','CapSize',0)
title('Low');xlabel('Time(s)');ylabel('Current (nA)')
hold off


subplot(1,3,2)
errorbar(nanmean(sal_med,2),nanstd(sal_med')/sqrt(size(sal_med,2)),'k','CapSize',0)
hold on
errorbar(nanmean(LY_med,2),nanstd(LY_med')/sqrt(size(LY_med,2)),'b','CapSize',0)
title('Med');xlabel('Time(s)');ylabel('Current (nA)')
hold off

subplot(1,3,3)
errorbar(nanmean(sal_hig,2),nanstd(sal_hig')/sqrt(size(sal_hig,2)),'k','CapSize',0)
hold on
errorbar(nanmean(LY_hig,2),nanstd(LY_hig')/sqrt(size(LY_hig,2)),'b','CapSize',0)
title('Sal - hig');xlabel('Time(s)');ylabel('Current (nA)')

figure;


subplot(1,3,1)
plot(sal_low,'k')
hold on
plot(LY_low,'b')
title('Low');xlabel('Time(s)');ylabel('Current (nA)')
hold off


subplot(1,3,2)
plot(sal_med, 'k')
hold on
plot(LY_med,'b')
title('Med');xlabel('Time(s)');ylabel('Current (nA)')
hold off

subplot(1,3,3)
plot(sal_hig,'k')
hold on
plot(LY_hig,'b')
title('Sal - hig');xlabel('Time(s)');ylabel('Current (nA)')



%%
%%
tic
k = 0;
for i = 1:length(data)
    for j = 1:length(data(i).analysis.trialNum)
        k = k+1;
        tempstruct(k).subject = data(i).subject;
        tempstruct(k).date = data(i).date;
        tempstruct(k).drug = data(i).drug;
        tempstruct(k).channel = data(i).channel;
        tempstruct(k).uniqueID = data(i).uniqueID;
        tempstruct(k).include = data(i).include;
        tempstruct(k).percentNaN = data(i).analysis.percentNaN(j);
        tempstruct(k).rewardMagnitude = data(i).analysis.rewardMagnitude(j);
        tempstruct(k).trialNum = data(i).analysis.trialNum(j);
        tempstruct(k).BaselineAUC1s = data(i).analysis.BaselineAUC1s(j);
        tempstruct(k).AUC5s = data(i).analysis.AUC5s(j);
        tempstruct(k).AUC5s_baselineCorrected = data(i).analysis.AUC5s_baselineCorrected(j);
        tempstruct(k).percentNaN_reward5s = data(i).analysis.percentNaN_reward5s(j);
        tempstruct(k).percentNaN_consecutive_reward5s = data(i).analysis.percentNaN_consecutive_reward5s(j);
        tempstruct(k).trialexclude = data(i).analysis.trialexclude(j);
        tempstruct(k).peak5s = data(i).analysis.peak5s(j);
        tempstruct(k).latency2peak5s = data(i).analysis.latency2peak5s(j);
        if calculate_t50
            tempstruct(k).t50_data = data(i).analysis.t50.t50_data(j);
            tempstruct(k).t50_rsq_data = data(i).analysis.t50.rsq_data(j);
            tempstruct(k).t50_datapks = data(i).analysis.t50.datapks(j);
            tempstruct(k).t50_datapklocs = data(i).analysis.t50.datapklocs(j);
            tempstruct(k).t50_peak_intercepts = data(i).analysis.t50.peak_intercepts(j);
        end
    end
end

dataTable = struct2table(tempstruct);
savefolder = 'C:\Users\mario\Documents\GitHub\Marios-temp\LY354740_Rat\TB1Awake_VarReward';
filename = 'LY354740_Rat_awake_analysis.xlsx';
writetable(dataTable,[savefolder, '\',filename])

toc
%%

tic
k = 0;
for i = 1:length(data)
    for j = 1:length(data(i).analysis.average.AUC)
        k = k+1;
        tempstructAvg(k).subject = data(i).subject;
        tempstructAvg(k).date = data(i).date;
        tempstructAvg(k).drug = data(i).drug;
        tempstructAvg(k).channel = data(i).channel;
        tempstructAvg(k).uniqueID = data(i).uniqueID;
        tempstructAvg(k).include = data(i).include;
        tempstructAvg(k).rewardMagnitude = j;
        tempstructAvg(k).AUC = data(i).analysis.average.AUC(j);
        tempstructAvg(k).peak = data(i).analysis.average.peak(j);
        tempstructAvg(k).latency2peak = data(i).analysis.average.latency2peak(j);
        if calculate_t50
            tempstructAvg(k).t50_data = data(i).analysis.average.t50.t50_data(j);
            tempstructAvg(k).t50_rsq_data = data(i).analysis.average.t50.rsq_data(j);
            tempstructAvg(k).t50_datapks = data(i).analysis.average.t50.datapks(j);
            tempstructAvg(k).t50_datapklocs = data(i).analysis.average.t50.datapklocs(j);
            tempstructAvg(k).t50_peak_intercepts = data(i).analysis.average.t50.peak_intercepts(j);
        end
        tempstructAvg(k).C_pred_AUC = data(i).analysis.C_pred.AUC(j);
        tempstructAvg(k).C_pred_peak = data(i).analysis.C_pred.peak(j);
        tempstructAvg(k).C_pred_latency2peak = data(i).analysis.C_pred.latency2peak(j);
        if calculate_t50
            tempstructAvg(k).C_pred_t50_data = data(i).analysis.C_pred.t50.t50_data(j);
            tempstructAvg(k).C_pred_t50_rsq_data = data(i).analysis.C_pred.t50.rsq_data(j);
            tempstructAvg(k).C_pred_t50_datapks = data(i).analysis.C_pred.t50.datapks(j);
            tempstructAvg(k).C_pred_t50_datapklocs = data(i).analysis.C_pred.t50.datapklocs(j);
            tempstructAvg(k).C_pred_t50_peak_intercepts = data(i).analysis.C_pred.t50.peak_intercepts(j);
        end
    end
end

dataTable = struct2table(tempstructAvg);
savefolder = 'C:\Users\mario\Documents\GitHub\Marios-temp\LY354740_Rat\TB1Awake_VarReward';
filename = 'LY354740_Rat_awake_analysisAvg.xlsx';
writetable(dataTable,[savefolder, '\',filename])
toc
%% Analyze Magazine frequencies for each session
% Reward TTL = 1
% Mag Entry TTL = 6
% mag Exit TTL = 7
% N.B. TTLs start counting at 0 so add 1 to these for Matlab indices
for i = 1:length(data) 
data(i).MagFrequency = sum(diff(data(i).TTLs(:,7))==1);
%
tempdata(i).subject = data(i).subject;
tempdata(i).date = data(i).date;
tempdata(i).drug = data(i).drug;
tempdata(i).channel = data(i).channel;
tempdata(i).uniqueID = data(i).uniqueID;
tempdata(i).include = data(i).include;
tempdata(i).MagFrequency = data(i).MagFrequency;
end
behaviouraldataTable = struct2table(tempdata);
savefolder = 'C:\Users\mario\Documents\GitHub\Marios-temp\LY354740_Rat\TB1Awake_VarReward';
filename = 'LY354740_Rat_awake_behaviour.xlsx';
writetable(behaviouraldataTable,[savefolder, '\',filename])
