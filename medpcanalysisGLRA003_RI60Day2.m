close all
clear all
clc
% \1 = TrialChange
% \2 = RewardDelivery
% \3 = MagazineEntry
% \4 = MagazineExit
% \5 = LeftLeverResponse
% \6 = LeftLeverResponseEnd
% \7 = RightLeverResponse
% \8 = RightLeverResponseEnd
% \9 = LeftNP
% \10 = LeftNPEnd
% \11 = RightNP
% \12 = RightNPEnd
% \13 = CS1On
% \14 = CS1Off
% \15 = CS2On
% \16 = CS2Off
% \17 = ITI start


FILEPATH = 'C:\Users\mpanagi\Documents\GitHub\Marios-temp\GLRA003\GLRA003_RI60Day2.xlsx';
[~,~,dataId]= xlsread(FILEPATH,2);
totalSubj = size(dataId,1)/2;

for subj = 1:totalSubj

    try
        
        
        
        %read data file
        [num,txt,raw]= xlsread(FILEPATH);
        %NaN indices
        index = find(isnan(num(:,1)));
        index = [1;index];
        
        
        
        % locate data associated between NaNs
        % data = num(1:index(1)-1,:);
        data = num(index(subj)+1:index(subj+1)-1,:);
        
        if sum(data(:,1)==5) > sum(data(:,1)==7)
            activeLP = 5;
        else
            activeLP = 7;
        end
        
        
        %Get subject details from sheet 2
        %{'MSN','StartDate','StartTime','Experiment','Group','Box','Subject','Session_Length','Total_MagFrq','Total_MagDur','TotalLP','MaxTrial','TotalEvents'}
        
        [~,~,dataId]= xlsread(FILEPATH,2);
        program = dataId(subj*2, 1);
        program = char(program);
        
        genotype = dataId(subj*2, 5);
        genotype = char(genotype);
        if genotype(1) == 'K'
            genotype = 'KO';
        else
            genotype = 'WT';
        end
        
        mouseid = dataId(subj*2, 7);
        mouseid = char(mouseid);
        totalLP = dataId(subj*2, 11);
        details = [genotype '_' mouseid '_' program];
        
        % % %Scatter plot of RightLP, and mag entries vs LP
        % figure
        % % scatter(data(find(data(:,1)==activeLP),2),data(find(data(:,1)==activeLP),1));
        % scatter(data(find(data(:,1)==2|data(:,1)==activeLP),2),data(find(data(:,1)==2|data(:,1)==activeLP),1));
        % title('Event times');
        % xlabel('Time within session (s)')
        % ylabel('activeLP =LP, 2 = Reward')
        
        % %Histograms
        nbins = 80;
        
        %LP
        figure
        % suptitle('Inter-Response-Intervals')
        
        subplot(2,2,1)
        data1 = data(find(data(:,1)==activeLP),2);
        data2 = data1(2:size(data1));
        lpDiff = data2-data1(1:size(data1)-1);
        histogram(lpDiff,nbins);
        title('LP IRI');
        xlabel('seconds')
        
        subplot(2,2,3)
        plot(data1(1:size(data1)-1),lpDiff)
        title('LP IRI - across session');
        xlabel('LP time within session')
        ylabel('IRI (s)')
        
        %Mag entry analysis - check for extra mag entry or exit at start or end of session
        magEntry = data(find(data(:,1)==3),2);
        magExit = data(find(data(:,1)==4),2);
        
        if size(magEntry,1) > size(magExit,1)
            magEntry(size(magEntry,1)) = [];
        elseif size(magEntry,1) < size(magExit,1)
            magExit(1) = [];
        end
        
        subplot(2,2,2)
        magDiff = magExit-magEntry;
        histogram(magDiff,nbins);
        title('Mag Durations');
        xlabel('seconds')
        
        subplot(2,2,4)
        plot(magEntry,magDiff)
        title('Mag Duration - across session');
        xlabel('magEntry time within session')
        ylabel('IRI (s)')
        
        
        
        
        %trials 10 seconds either side of a reward
        time_pre = 10;
        time_post = 10;
        rewardTime = data(find(data(:,1)==2),2);
        trialStart = rewardTime-time_pre;
        trialEnd = rewardTime+time_post;
        numTrials = length(rewardTime);
        
        i = 1; %trialnumber
        rasterDataLP = [];
        rasterDataMagEntry = [];
        rasterDataMagExit = [];
        for i = [1:numTrials]
            trial = data(data(:,2) >= trialStart(i) & data(:,2) <= trialEnd(i),:);
            
            trial_LP(:,2) = trial(trial(:,1) == activeLP, 2);
            trial_LP(:,2) = trial_LP(:,2) - rewardTime(i);
            trial_LP(:,1) = i;
            rasterDataLP = [rasterDataLP; trial_LP];
            trial_LP = [];
            
            trial_MagEntry(:,2) = trial(trial(:,1) == 3, 2);
            trial_MagEntry(:,2) = trial_MagEntry(:,2) - rewardTime(i);
            trial_MagEntry(:,1) = i;
            rasterDataMagEntry = [rasterDataMagEntry; trial_MagEntry];
            trial_MagEntry = [];
            
            trial_MagExit(:,2) = trial(trial(:,1) == 4, 2);
            trial_MagExit(:,2) = trial_MagExit(:,2) - rewardTime(i);
            trial_MagExit(:,1) = i;
            rasterDataMagExit = [rasterDataMagExit; trial_MagExit];
            trial_MagExit = [];
            
        end
        
        figure
        suptitle('Responses aligned to reward delivery')
        scatter(rasterDataLP(:,2),rasterDataLP(:,1), 15, 'v', 'k')
        hold on
        scatter(rasterDataMagEntry(:,2),rasterDataMagEntry(:,1), '.','b')
        scatter(rasterDataMagExit(:,2),rasterDataMagExit(:,1),'.', 'r')
        legend('LP', 'MagEntry', 'MagExit')
        xlabel('Time (s) relative to reward delivery (t = 0)')
        y = ylabel('Trial Number');
        set(legend,'Location','northeastoutside');
        set(gca,'Ydir','reverse')
        hold off
        
        
        %first response post reward [not indicating what the response is...]
        %Reward intervals
        nbins = 8;
        figure
        % suptitle('Post-Reward Responding')
        
        subplot(2,2,2)
        data5 = data(find(data(:,1)==2),2);
        data6 = data5(2:size(data5));
        rewardDiff = data6-data5(1:size(data5)-1);
        histogram(rewardDiff,nbins);
        title('Inter Reward Intervals');
        xlabel('seconds')
        
        
        subplot(2,2,1)
        nbins = 10;
        postReward = data(find(data(:,1)==2)+1,:);
        histogram(postReward(:,2)-rewardTime,nbins);
        title('IRI - 1st response post-reward')
        xlabel('seconds')
        
        subplot(2,2,3)
        ind = find(postReward(:,1)==3);
        histogram(postReward(ind,2)-rewardTime(ind),nbins);
        title('IRI - 1st Response MagEntry ')
        xlabel('seconds')
        
        subplot(2,2,4)
        ind = find(postReward(:,1)==activeLP);
        histogram(postReward(ind,2)-rewardTime(ind),nbins);
        title('IRI - 1st Response LP')
        xlabel('seconds')
        
        %Probability of response -LP-> MagEntry
        lp_magEntry = data(find(data(:,1)==3|data(:,1)==activeLP),:);
        lp_magEntry_n2 = lp_magEntry(2:size(lp_magEntry,1),:);
        lp_magEntry(size(lp_magEntry,1),:) = [];
        
        lp_magEntry_diff = zeros(size(lp_magEntry));
        lp_magEntry_diff(:,2) = lp_magEntry_n2(:,2) - lp_magEntry(:,2);
        
        %mag-mag
        lp_magEntry_diff((lp_magEntry(:,1)==3) & (lp_magEntry_n2(:,1)==3),1) = 1;
        %mag-lever
        lp_magEntry_diff((lp_magEntry(:,1)==3) & (lp_magEntry_n2(:,1)==activeLP),1) = 2;
        %lever-mag
        lp_magEntry_diff((lp_magEntry(:,1)==activeLP) & (lp_magEntry_n2(:,1)==3),1) = 3;
        %lever-lever
        lp_magEntry_diff((lp_magEntry(:,1)==activeLP) & (lp_magEntry_n2(:,1)==activeLP),1) = 4;
        
        freqTable = tabulate(lp_magEntry_diff(:,1));
        
        figure
        % suptitle('Inter-Response-Intervals for LP/Mag chains')
        subplot(2,2,1)
        histogram(lp_magEntry_diff(lp_magEntry_diff(:,1) == 1, 2))
        title('MagEntry-MagEntry IRI')
        xlabel(sprintf('Seconds \n Mean %.2f - Median %.2f ', mean(lp_magEntry_diff(lp_magEntry_diff(:,1) == 1, 2)), median(lp_magEntry_diff(lp_magEntry_diff(:,1) == 1, 2))))
        ylabel(sprintf('%d occurrences (%.2f %%)', freqTable(1,2), freqTable(1,3)))
        
        subplot(2,2,2)
        histogram(lp_magEntry_diff(lp_magEntry_diff(:,1) == 2, 2))
        title('MagEntry-LP IRI')
        xlabel(sprintf('Seconds \n Mean %.2f - Median %.2f ', mean(lp_magEntry_diff(lp_magEntry_diff(:,1) == 2, 2)), median(lp_magEntry_diff(lp_magEntry_diff(:,1) == 2, 2))))
        ylabel(sprintf('%d occurrences (%.2f %%)', freqTable(2,2), freqTable(2,3)))
        
        subplot(2,2,4)
        histogram(lp_magEntry_diff(lp_magEntry_diff(:,1) == 3, 2))
        title('LP-MagEntry IRI')
        xlabel(sprintf('Seconds \n Mean %.2f - Median %.2f ', mean(lp_magEntry_diff(lp_magEntry_diff(:,1) == 3, 2)), median(lp_magEntry_diff(lp_magEntry_diff(:,1) == 3, 2))))
        ylabel(sprintf('%d occurrences (%.2f %%)', freqTable(3,2), freqTable(3,3)))
        
        subplot(2,2,3)
        histogram(lp_magEntry_diff(lp_magEntry_diff(:,1) == 4, 2))
        title('LP-LP IRI')
        xlabel(sprintf('Seconds \n Mean %.2f - Median %.2f ', mean(lp_magEntry_diff(lp_magEntry_diff(:,1) == 4, 2)), median(lp_magEntry_diff(lp_magEntry_diff(:,1) == 4, 2))))
        ylabel(sprintf('%d occurrences (%.2f %%)', freqTable(4,2), freqTable(4,3)))
        
        
        % %Bout analysis - Unfinished
        %
        % boutStart = 0;
        % bout_Num = 0;
        % for i = [1: size(data,1)]
        %
        %     if boutStart
        %         if data(i,1) == 3
        %             boutStart = 0;
        %         end
        %
        %     elseif data(i,1) == activeLP && ~boutStart
        %         boutStart = 1;
        %         bout_Num = bout_Num+1;
        %     end
        % end
        %
        % totalLP = sum(data(:,1) == activeLP);
        
        figlist = findobj('type','figure');
        fullscreen = get(0, 'ScreenSize');
        
        fignames = {'Inter Response Intervals', 'Aligned Responses', 'Post Reward Response Intervals', 'Response Chains'};
        for i = 1:length(figlist)
            saveas(figure(i), ['GLRA003_RI60Day2_Fig' num2str(i) '_' genotype '_' mouseid '_' fignames{1,i} '.png'])
        end
        
    catch
        
    end
    close all
    %     clearvars -except subj
end
