function [likelihood, DATA] = simRLonPavlovianBlockingTask( condition, DATA, nbStim, init, alpha, alpha2, alpha3, alpha4, alpha5, motiv2 )
    %% INPUT
    %       condition = 1 (saline) or 0 (muscimol)
    %       DATA = contains one line per trial (see structure below)
    %       nbStim = total number of stim used in the task (fixed to 4)
    %       init = initial stimulus values (V_0)
    %       alpha = learning rate
    %       alpha2 = forgetting rate
    %       alpha3 = learning rate for no-reward
    %       alpha4 = learning rate stage 2 (different motivation)
    %       alpha5 = learning rate during muscimol injection
    %       motiv2 = boost in motivation (V bonus) at beginning of stage 2

    %% DATA INPUT STRUCTURE
    % 1 - day
    % 2 - trial
    % 3 - nb magazine entries
    % 4:7 - stim (format : 1010)
    % 8 - reward
    % 9 - model predicted value
    % 10 - model reward prediction error
    % 11-14 - model stim values
    
    %% INIT VARIABLES
    threshold = 1e-6;
    maxV = 1; maxMagEnt = 10;
    nbTrialTotal = size(DATA(:,1),1);
    nbDays = size(unique(DATA(:,1)),1);
    
    %% INIT MF-RL MODEL
    V = zeros(nbTrialTotal+1,nbStim); % MF-values for each stim
    V(1,:) = ones(1,nbStim) * init; % initial value
    RPE = zeros(nbTrialTotal,1); % for each stim
    proba = zeros(nbTrialTotal,2); % proba to visit magazine
    LL = zeros(nbTrialTotal,2); % loglikelihoods of data/reward given the model
    
    %% INIT EXPERIMENT
    day = 1;
    for nt=1:nbTrialTotal
        %% FORGETTING IN CASE OF SESSION CHANGE
        if (DATA(nt,1) ~= day)
            day = DATA(nt,1);
            if (day == 12)
                V(nt,:) = V(nt,:) + motiv2; % boost in motivation (saliene?) for compounds in Stage 2
            end
            if (~condition&&(day>=5)&&(day<=10)) % muscimol injection
%                 if (day==5) % 2 days have passed, one more step of
%                 forgetting (WRONG !)
%                     V(nt,1) = (1 - alpha2) * V(nt,1);
%                 end
                V(nt,1) = (1 - alpha2) * V(nt,1); % day-to-day forgetting
            end
        end
        
        %% OBSERVATION PHASE
        % what happend for the subject
        stim = DATA(nt,4:4+nbStim-1); % presented stimuli
        ratProba = DATA(nt,3) / maxMagEnt; % nb magazine entries / max possible nb entries
        reward = DATA(nt,4+nbStim);
        
        %% DECISION-MAKING PHASE
        % the proba of the model to visit the magazine for a certain
        % amount of time depends on the sum over presented stim values
        % (written 'predictedV' hereafter)
        listStim = (1:nbStim) .* stim; % list of stim that are present
        listStim(listStim==0) = []; % removing zeros from the list
        predictedV = max(threshold,sum(V(nt,listStim)));
        proba(nt,:) = [predictedV/maxV (1-(predictedV/maxV))];
        LL(nt,:) = [log(1-abs(proba(nt,1)-ratProba)) log(proba(nt,1))]; % storing log likelihoods
        
        %% REINFORCEMENT LEARNING PHASE
        % computing the reward prediction error
        RPE(nt,1) = reward - predictedV;
        % updating stim values
        if (reward == 1)
            if (~condition&&(day>=5)&&(day<=10)) % Stage 1 muscimol injection
                V(nt+1,:) = V(nt,:) + alpha5 * RPE(nt,1) * stim;
            else
                if ((day>=12)&&(day<=14)) % Stage 2
                    V(nt+1,:) = V(nt,:) + alpha4 * RPE(nt,1) * stim;
                else
                    V(nt+1,:) = V(nt,:) + alpha * RPE(nt,1) * stim;
                end
            end
        else % pre-exposure and test phase (reward == 0 during extinction)
            V(nt+1,:) = V(nt,:) + alpha3 * RPE(nt,1) * stim;
        end
        
        %% LOGS
        DATA(nt,4+nbStim+1) = predictedV;
        DATA(nt,4+nbStim+2) = RPE(nt,1);
        DATA(nt,4+nbStim+2+1:4+nbStim+2+nbStim) = V(nt,:); % value of each stim
    end
    
    %% evaluation of the fit
    likelihood = exp(sum(LL)/nbTrialTotal); % LL on all trials
    
     % NEW for memory clearing, should be removed if one wants the function to return these variables
     clear V;
     clear LL;
     clear proba;
     clear RPE;
     %clear DATA;
end
