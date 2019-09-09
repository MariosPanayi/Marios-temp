function [distanceToData, LL, DATA] = launchSimRLonPavlovianBlockingTask(plotFigure, condition, init, alpha, alpha2, alpha3, alpha4, alpha5, motiv2, factorVtoMagEntries)
    %% INPUT
    %       condition = 1 (saline) or 0 (muscimol)
    %       init = initial stimulus values (V_0)
    %       alpha = learning rate
    %       alpha2 = forgetting rate
    %       alpha3 = learning rate for no-reward
    %       alpha4 = learning rate stage 2 (different motivation)
    %       alpha5 = learning rate during muscimol injection
    %       motiv2 = boost in motivation (V bonus) at beginning of stage 2
    %       factorVtoMagEntries = this is multiplied to V to determine the number of magazine entries
    
    %% FIXED TASK PARAMETERS
    nbStim = 4;
    nbDayS1 = 4; % 1-4 Stage 1
    nbDayIn = 6; % 5-10 Stage 1+infusion
    nbDayPE = 1; % 11 pre-exposure
    nbDayS2 = 3; % 12-14 Stage 2
    nbDayTe = 1; % 15 Test
    nbDays = nbDayS1 + nbDayIn + nbDayPE + nbDayS2 + nbDayTe;
    nbTrialSta1 = 16; % nb trials (all with stim A) per day for Stage 1
    nbTrialPreE = 4; % nb trials per stim during pre exposure (B or D) on day 11
    nbTrialSta2 = 8; % nb trials for each compound (AB or CD) per day for Stage 2
    nbTrialTest = 8; % nb trials per blocked cue (B or D) per day for Test
    
    %% IF WE WANT TO PLOT SOME FIGURES
    %plotFigure = false;
    %factorVtoMagEntries = 5; % this is multiplied to V to determine the number of magazine entries
    
    %% DATA INPUT STRUCTURE
    % 1 - day
    % 2 - trial
    % 3 - nb magazine entries
    % 4:7 - stim (format : 1010)
    % 8 - reward
    % 9 - model predicted value
    % 10 - model reward prediction error
    % 11-14 - model stim values
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% INIT DATA FIRST VERSION
    DATA = [];
    % data for Stage 1 + Pre-Exposure with B and D + Stage 2 with AB and CD +
    % Test with B and D
    for nd=1:nbDays % loop over days
        switch (nd)
            case {1,2,3,4,5,6,7,8,9,10}
                nbt = nbTrialSta1; % Stage 1
                Vmatrix = [ones(nbt,1) zeros(nbt,nbStim-1)]; % A
                reward = 1;
            case 11
                nbt = 2 * nbTrialPreE; % Pre-Exposure
                Vmatrix = [[zeros(nbTrialPreE,1) ones(nbTrialPreE,1) zeros(nbTrialPreE,nbStim-2)] ; ... % B
                           [zeros(nbTrialPreE,nbStim-1) ones(nbTrialPreE,1)]]; % D
                reward = 0;
            case {12,13,14}
                nbt = 2 * nbTrialSta2;  % Stage 2
                Vmatrix = [[ones(nbTrialSta2,2) zeros(nbTrialSta2,nbStim-2)] ; ... % AB
                           [zeros(nbTrialSta2,nbStim-2) ones(nbTrialSta2,2)]]; % CD
                reward = 1;
            otherwise
                nbt = 2 * nbTrialTest; % Test
                Vmatrix = [[zeros(nbTrialTest,1) ones(nbTrialTest,1) zeros(nbTrialTest,nbStim-2)] ; ... % B
                           [zeros(nbTrialTest,nbStim-1) ones(nbTrialTest,1)]]; % D
                reward = 0;
        end
        DATA = [DATA ; zeros(nbt, 8)];
        DATA(end-nbt+1:end,1) = nd;
        DATA(end-nbt+1:end,2) = (1:nbt)';
        DATA(end-nbt+1:end,3) = round(DATA(end-nbt+1:end,8) * 10);
        DATA(end-nbt+1:end,4:7) = Vmatrix;
        DATA(end-nbt+1:end,8) = reward;
    end
    
%     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%     %% INIT DATA SECOND VERSION
%     % simulate the model on exactly the series of trials experienced by the
%     % animals
%     To do later with the experimental data.
%     
%     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%     %% loading behavioral data for all subjects
%     load(['rat_data_baseline_raw-copie.csv']);
%     DATA = rat_data_baseline_raw_copie;
%     clear rat_data_baseline_raw_copie;
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% TASK simulation
    [LL, DATA] = simRLonPavlovianBlockingTask(condition, DATA, nbStim, init, alpha, alpha2, alpha3, alpha4, alpha5, motiv2);
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% DATA POST-PROCESSING
    % organizing data to plot (V values) and compute distance to
    % experimental data curves
    dataStimA = DATA(DATA(:,4)==1|DATA(:,5)==1,:);
    V_A = dataStimA(:,3+nbStim+3+1);
    V_A_4 = V_A([(nbTrialSta1:nbTrialSta1:nbTrialSta1*(nbDayS1+nbDayIn)) (nbTrialSta1*(nbDayS1+nbDayIn)+nbTrialPreE:nbTrialPreE:nbTrialSta1*(nbDayS1+nbDayIn)+nbTrialPreE*nbDayPE) (nbTrialSta1*(nbDayS1+nbDayIn)+nbTrialPreE*nbDayPE+nbTrialSta2:nbTrialSta2:nbTrialSta1*(nbDayS1+nbDayIn)+nbTrialPreE*nbDayPE+nbTrialSta2*(nbDayS2))]');
    dataStimB = DATA(DATA(:,4)==1|DATA(:,5)==1,:);
    V_B = dataStimB(:,3+nbStim+3+2);
    V_B_4 = V_B([(nbTrialSta1:nbTrialSta1:nbTrialSta1*(nbDayS1+nbDayIn)) (nbTrialSta1*(nbDayS1+nbDayIn)+nbTrialPreE:nbTrialPreE:nbTrialSta1*(nbDayS1+nbDayIn)+nbTrialPreE*nbDayPE) (nbTrialSta1*(nbDayS1+nbDayIn)+nbTrialPreE*nbDayPE+nbTrialSta2:nbTrialSta2:nbTrialSta1*(nbDayS1+nbDayIn)+nbTrialPreE*nbDayPE+nbTrialSta2*(nbDayS2))]');
    V_B_pred = dataStimB(:,3+nbStim+2);
    dataStimC = DATA((DATA(:,4)==1&DATA(:,5)==0)|DATA(:,7)==1,:);
    V_C = dataStimC(:,3+nbStim+3+3);
    V_C_4 = V_C([(nbTrialSta1:nbTrialSta1:nbTrialSta1*(nbDayS1+nbDayIn)) (nbTrialSta1*(nbDayS1+nbDayIn)+nbTrialPreE:nbTrialPreE:nbTrialSta1*(nbDayS1+nbDayIn)+nbTrialPreE*nbDayPE) (nbTrialSta1*(nbDayS1+nbDayIn)+nbTrialPreE*nbDayPE+nbTrialSta2:nbTrialSta2:nbTrialSta1*(nbDayS1+nbDayIn)+nbTrialPreE*nbDayPE+nbTrialSta2*(nbDayS2))]');
    dataStimD = DATA((DATA(:,4)==1&DATA(:,5)==0)|DATA(:,7)==1,:);
    V_D = dataStimD(:,3+nbStim+3+4);
    V_D_4 = V_D([(nbTrialSta1:nbTrialSta1:nbTrialSta1*(nbDayS1+nbDayIn)) (nbTrialSta1*(nbDayS1+nbDayIn)+nbTrialPreE:nbTrialPreE:nbTrialSta1*(nbDayS1+nbDayIn)+nbTrialPreE*nbDayPE) (nbTrialSta1*(nbDayS1+nbDayIn)+nbTrialPreE*nbDayPE+nbTrialSta2:nbTrialSta2:nbTrialSta1*(nbDayS1+nbDayIn)+nbTrialPreE*nbDayPE+nbTrialSta2*(nbDayS2))]');
    V_D_pred = dataStimD(:,3+nbStim+2);
    V_B_mean = zeros(nbDays,1);
    V_D_mean = zeros(nbDays,1);
    for nd=1:nbDays
        switch nd
            case {1,2,3,4,5,6,7,8,9,10} % Stage 1
                V_B_mean(nd) = mean(V_B_pred((nd-1)*nbTrialSta1+1:nd*nbTrialSta1)); 
                V_D_mean(nd) = mean(V_D_pred((nd-1)*nbTrialSta1+1:nd*nbTrialSta1));
            case 11 % Pre-Exposure
                V_B_mean(nd) = mean(V_B_pred((nbDayS1+nbDayIn)*nbTrialSta1+(nd-nbDayS1-nbDayIn-1)*nbTrialPreE+1:(nbDayS1+nbDayIn)*nbTrialSta1+(nd-nbDayS1-nbDayIn)*nbTrialPreE)); 
                V_D_mean(nd) = mean(V_D_pred((nbDayS1+nbDayIn)*nbTrialSta1+(nd-nbDayS1-nbDayIn-1)*nbTrialPreE+1:(nbDayS1+nbDayIn)*nbTrialSta1+(nd-nbDayS1-nbDayIn)*nbTrialPreE));
            case {12,13,14} % Stage 2
                V_B_mean(nd) = mean(V_B_pred((nbDayS1+nbDayIn)*nbTrialSta1+nbDayPE*nbTrialPreE+(nd-nbDayS1-nbDayIn-nbDayPE-1)*nbTrialSta2+1:(nbDayS1+nbDayIn)*nbTrialSta1+nbDayPE*nbTrialPreE+(nd-nbDayS1-nbDayIn-nbDayPE)*nbTrialSta2)); 
                V_D_mean(nd) = mean(V_D_pred((nbDayS1+nbDayIn)*nbTrialSta1+nbDayPE*nbTrialPreE+(nd-nbDayS1-nbDayIn-nbDayPE-1)*nbTrialSta2+1:(nbDayS1+nbDayIn)*nbTrialSta1+nbDayPE*nbTrialPreE+(nd-nbDayS1-nbDayIn-nbDayPE)*nbTrialSta2));
            otherwise % Test
                V_B_mean(nd) = mean(V_B_pred((nbDayS1+nbDayIn)*nbTrialSta1+nbDayPE*nbTrialPreE+nbDayS2*nbTrialSta2+(nd-nbDayS1-nbDayIn-nbDayPE-nbDayS2-1)*nbTrialTest+1:(nbDayS1+nbDayIn)*nbTrialSta1+nbDayPE*nbTrialPreE+nbDayS2*nbTrialSta2+(nd-nbDayS1-nbDayIn-nbDayPE-nbDayS2)*nbTrialTest)); 
                V_D_mean(nd) = mean(V_D_pred((nbDayS1+nbDayIn)*nbTrialSta1+nbDayPE*nbTrialPreE+nbDayS2*nbTrialSta2+(nd-nbDayS1-nbDayIn-nbDayPE-nbDayS2-1)*nbTrialTest+1:(nbDayS1+nbDayIn)*nbTrialSta1+nbDayPE*nbTrialPreE+nbDayS2*nbTrialSta2+(nd-nbDayS1-nbDayIn-nbDayPE-nbDayS2)*nbTrialTest));
        end
    end
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% FIGURES
    if (plotFigure)
        % figure parameters
        if (condition == 1)
            figure
            idx = 0; % model V for Saline condition
            yLabFig = 'Saline';
            markerColor = 'w';
            condColor = 'k';
        else
            idx = 4; % model V for Muscimol condition
            yLabFig = 'Muscimol';
            markerColor = 'k';
            condColor = 'r';
        end

        % Stage 1
        subplot(3,4,idx+1:idx+2)
        plot(1:nbDayS1,V_A_4(1:nbDayS1),'k','LineWidth',2)
        hold on
        plot(nbDayS1+2:nbDayS1+nbDayIn+1,V_A_4(nbDayS1+1:nbDayS1+nbDayIn),'k','LineWidth',2)
        legend('model V','Location','southeast')
        axis([0 nbDayS1+nbDayIn+2 -0.01 1.01])
        xticks([1:nbDayS1 nbDayS1+2:nbDayS1+nbDayIn+1])
        xticklabels([1:nbDayS1 nbDayS1+1:nbDayS1+nbDayIn])
        yticks([0:0.2:1])
        %xlabel('Day','FontSize',14)
        ylabel(yLabFig,'FontSize',14)
        if (condition == 1)
            title('Stage 1','FontSize',18)
        end
        % Stage 2
        subplot(3,4,idx+3)
        plot(1:nbDayS2,[V_A_4(nbDayS1+nbDayIn+nbDayPE+1:nbDayS1+nbDayIn+nbDayPE+nbDayS2) V_B_4(nbDayS1+nbDayIn+nbDayPE+1:nbDayS1+nbDayIn+nbDayPE+nbDayS2)],'-o','LineWidth',2)
        hold on
        plot(1:nbDayS2,[V_C_4(nbDayS1+nbDayIn+nbDayPE+1:nbDayS1+nbDayIn+nbDayPE+nbDayS2) V_D_4(nbDayS1+nbDayIn+nbDayPE+1:nbDayS1+nbDayIn+nbDayPE+nbDayS2)],'--^','LineWidth',2)
        if (condition == 1)
            legend('A','B','C','D','Location','west')
        end
        axis([0 nbDayS2+1 -0.01 1.01])
        xticks(1:nbDayS2)
        xticklabels(nbDayS1+nbDayIn+nbDayPE+1:nbDayS1+nbDayIn+nbDayPE+nbDayS2)
        yticks([0:0.2:1])
        %xlabel('Day','FontSize',14)
        if (condition == 1)
            title('Stage 2','FontSize',18)
        end
        % Test
        subplot(3,4,idx+4)
        plot(1:nbTrialTest*nbDayTe,[V_A(end-nbTrialTest*nbDayTe+1:end) V_B(end-nbTrialTest*nbDayTe+1:end)],'-o','LineWidth',2)
        hold on
        plot(1:nbTrialTest*nbDayTe,[V_C(end-nbTrialTest*nbDayTe+1:end) V_D(end-nbTrialTest*nbDayTe+1:end)],'--^','LineWidth',2)
        %legend('A','B','C','D','Location','east')
        axis([0 nbTrialTest*nbDayTe+1 -0.01 1.01])
        xticks(1:nbTrialTest*nbDayTe)
        yticks([0:0.2:1])
        if (condition == 1)
            xlabel('Trial','FontSize',14)
        end
        if (condition == 1)
            title('Test','FontSize',18)
        end


        %% number of magazine entries for Saline condition
        % Stage 1
        subplot(3,4,9:10)
        if (condition == 0)
            hold on
        end
        plot(1:nbDayS1,V_B_mean(1:nbDayS1)*factorVtoMagEntries,'-sk','LineWidth',2,'MarkerFaceColor',markerColor)
        hold on
        plot(121:122,[-2 -2],'-sk','LineWidth',2,'MarkerFaceColor',[0 0 0])
        plot(nbDayS1+2:nbDayS1+nbDayIn+1,V_B_mean(nbDayS1+1:nbDayS1+nbDayIn)*factorVtoMagEntries,'-sk','LineWidth',2,'MarkerFaceColor',markerColor)
        %legend([yLabFig ': A'],'Location','southeast')
        legend('Saline: A','Muscimol: A','Location','southeast')
        axis([0 nbDayS1+nbDayIn+2 -1 6])
        xticks([1:nbDayS1 nbDayS1+2:nbDayS1+nbDayIn+1])
        xticklabels([1:nbDayS1 nbDayS1+1:nbDayS1+nbDayIn])
        yticks(-1:6)
        xlabel('Day','FontSize',14)
        ylabel('Magazine Entries','FontSize',14)
        % Stage 2
        subplot(3,4,11)
        if (condition == 0)
            hold on
        end
        plot(1:nbDayS2,V_B_mean(nbDayS1+nbDayIn+nbDayPE+1:nbDayS1+nbDayIn+nbDayPE+nbDayS2)*factorVtoMagEntries,'-ok','LineWidth',2,'MarkerFaceColor',markerColor)
        hold on
        plot(1:nbDayS2,V_D_mean(nbDayS1+nbDayIn+nbDayPE+1:nbDayS1+nbDayIn+nbDayPE+nbDayS2)*factorVtoMagEntries,'--^k','LineWidth',2,'MarkerFaceColor',markerColor)
        plot(121:122,[-2 -2],'-ok','LineWidth',2,'MarkerFaceColor',[0 0 0])
        plot(121:122,[-2 -2],'--^k','LineWidth',2,'MarkerFaceColor',[0 0 0])
        legend('Saline: AB','Saline: CD','Muscimol: AB','Muscimol: CD','Location','southeast')
        axis([0 nbDayS2+1 -1 6])
        xticks(1:nbDayS2)
        xticklabels(nbDayS1+nbDayIn+nbDayPE+1:nbDayS1+nbDayIn+nbDayPE+nbDayS2)
        yticks(-1:6)
        xlabel('Day','FontSize',14)
        % Test
        subplot(3,4,12)
        if (condition == 0)
            hold on
        end
        bar((1-condition)*3+1,V_B_mean(nbDays),'LineWidth',2,'EdgeColor',condColor,'FaceColor',condColor)
        hold on
        bar((1-condition)*3+2,V_D_mean(nbDays),'LineWidth',2,'EdgeColor',condColor,'FaceColor','w')
        legend('B','D','Location','northwest')
        axis([0 6 0 1])
        xticks([1.5 4.5])
        xticklabels({'Saline','Muscimol'})
        yticks(-1:6)

        FigHandle = gcf;
        set(FigHandle, 'Position', [100, 10, 700, 500]);
    end % end of if (plotFigure)
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% DISTANCE TO EXPERIMENTAL DATA
    distanceToData = 0;
    
    if (condition == 1) % saline
        % Stage 1 pre-infusion
%         distanceToData = distanceToData + abs(V_B_mean(1)*factorVtoMagEntries - 0); % day 1
% %         distanceToData = distanceToData + abs(V_B_mean(nbDayS1)*factorVtoMagEntries - 0.5); % day 4
% %         % Stage 1 infusion
% %         distanceToData = distanceToData + abs(V_B_mean(nbDayS1+1)*factorVtoMagEntries - 0.5); % day 5
%         distanceToData = distanceToData + abs(V_B_mean(nbDayS1+nbDayIn)*factorVtoMagEntries - 2.2); % day 10
        % Stage 2
        distanceToData = distanceToData + abs(V_B_mean(nbDayS1+nbDayIn+nbDayPE+1)*factorVtoMagEntries - 4.1); % day 12
%         distanceToData = distanceToData + abs(V_B_mean(nbDayS1+nbDayIn+nbDayPE+2)*factorVtoMagEntries - 4.2); % day 13
        distanceToData = distanceToData + abs(V_B_mean(nbDayS1+nbDayIn+nbDayPE+3)*factorVtoMagEntries - 4.4); % day 14
        distanceToData = distanceToData + abs(V_D_mean(nbDayS1+nbDayIn+nbDayPE+1)*factorVtoMagEntries - 3); % day 12
%         distanceToData = distanceToData + abs(V_D_mean(nbDayS1+nbDayIn+nbDayPE+2)*factorVtoMagEntries - 4.1); % day 13
        distanceToData = distanceToData + abs(V_D_mean(nbDayS1+nbDayIn+nbDayPE+3)*factorVtoMagEntries - 4.5); % day 14
        % Test
        distanceToData = distanceToData + abs(V_B_mean(nbDays) - 0.2); % test
        distanceToData = distanceToData + abs(V_D_mean(nbDays) - 1.2); % test
    else % muscimol
        % Stage 1 pre-infusion
%         distanceToData = distanceToData + abs(V_B_mean(1)*factorVtoMagEntries - 0); % day 1
% %         distanceToData = distanceToData + abs(V_B_mean(nbDayS1)*factorVtoMagEntries - 0.3); % day 4
% %         % Stage 1 infusion
% %         distanceToData = distanceToData + abs(V_B_mean(nbDayS1+1)*factorVtoMagEntries - 0); % day 5
%         distanceToData = distanceToData + abs(V_B_mean(nbDayS1+nbDayIn)*factorVtoMagEntries - 1); % day 10
        % Stage 2
        distanceToData = distanceToData + abs(V_B_mean(nbDayS1+nbDayIn+nbDayPE+1)*factorVtoMagEntries - 3); % day 12
%         distanceToData = distanceToData + abs(V_B_mean(nbDayS1+nbDayIn+nbDayPE+2)*factorVtoMagEntries - 3.9); % day 13
        distanceToData = distanceToData + abs(V_B_mean(nbDayS1+nbDayIn+nbDayPE+3)*factorVtoMagEntries - 3.7); % day 14
        distanceToData = distanceToData + abs(V_D_mean(nbDayS1+nbDayIn+nbDayPE+1)*factorVtoMagEntries - 2.4); % day 12
%         distanceToData = distanceToData + abs(V_D_mean(nbDayS1+nbDayIn+nbDayPE+2)*factorVtoMagEntries - 4); % day 13
        distanceToData = distanceToData + abs(V_D_mean(nbDayS1+nbDayIn+nbDayPE+3)*factorVtoMagEntries - 4.7); % day 14
        % Test
        distanceToData = distanceToData + abs(V_B_mean(nbDays) - 0.7); % test
        distanceToData = distanceToData + abs(V_D_mean(nbDays) - 1.5); % test
    end
    
end