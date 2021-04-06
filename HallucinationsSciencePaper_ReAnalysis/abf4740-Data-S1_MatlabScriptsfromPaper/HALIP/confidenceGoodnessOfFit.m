function [confidenceGof]=ConfidenceGoodnessOfFit(data,simData,varargin)

p=inputParser;
validTable = @(x) istable(x) & sum(ismember(x.Properties.VariableNames,{'evidence','choice','confidence','outcome'}))==4;
p.addRequired('data', validTable);
p.addRequired('simData', validTable);
p.addParameter('nPermutations',1000); %number of permutations for significance r values
p.addParameter('windows',1); %window for smoothing predictions for trialwise calculations
p.addParameter('binNumbers',3); %binning for bin wise correlations
p.addParameter('allMinDataPoints',[0]); %binning for bin wise correlations
p.addParameter('sampleOption','down',@(x) any(validatestring(x,{'down','up'})));

p.parse(data,simData,varargin{:});
for fn=fieldnames(p.Results)'
    eval([fn{1} '=p.Results.(fn{1});']);
end


%% calculates goodness of fit based on grouping into evidence and correctness
if unique(data.realEvidence)~=unique(simData.realEvidence)
    warning('realEvidence in data and simData is different. Revise!')
end
g=1;

%% loops over matched and raw confidence values
varNames={'confidence','confidenceRaw'};
confidenceNames=varNames(ismember(varNames,data.Properties.VariableNames));
allBinNumber=nan(length(windows)*length(confidenceNames)+length(binNumbers)*length(confidenceNames),1);
for cn=1:length(confidenceNames)
    confidenceName=confidenceNames{cn};
    
    %% trialwise goodness of fit
    simTable=grpstats(simData,{'realEvidence','outcome'},'mean','dataVars',confidenceName);
    confidenceTrialIdx=find(~isnan(data{:,confidenceName}));
    
    for w=1:length(windows)
        window=windows(w);
        x=nan(size(confidenceTrialIdx));
        y=x;
        for c=1:length(confidenceTrialIdx)
            dataIdx=confidenceTrialIdx(c);
            %mean simulated confidence to correlated with observed confidence for rsquare
            %simIdx=ismember(simTable(:,{'realEvidence','outcome'}),data(dataIdx,{'realEvidence','outcome'}));
            simIdx=abs(simTable.realEvidence-data.realEvidence(dataIdx))<=window&simTable.outcome==data.outcome(dataIdx);%include all trials that are close in realEvidence
            x(c)=data{dataIdx,confidenceName};
            if sum(simIdx)>0
                y(c)=sum(simTable{simIdx,strcat('mean_',confidenceName)}.*simTable.GroupCount(simIdx))./sum(simTable.GroupCount(simIdx));
            end
        end
        nanIdx=isnan(y);

        % calculate correlation
        if length(x(~nanIdx))==length(y(~nanIdx))
            confidenceGof(g,:)=correlationXY(x,y,ones(size(x)),nPermutations,'trialwise',confidenceName);
            allBinNumber(g)=window;
            g=g+1;
        end
    end
end
for cn=1:length(confidenceNames)
    confidenceName=confidenceNames{cn};
    
    for b=1:length(binNumbers)
        binNumber=binNumbers(b);
        binEdgesConfidence=prctile(data{:,confidenceName},[0:100/binNumber:100]);
        binEdgesEvidence=[min(data.realEvidence(data.oddtrial))-20*eps,max(data.realEvidence(data.oddtrial))+20*eps,prctile(data.realEvidence(~data.oddtrial),[0:100/binNumber:100])];
        
        %% resample data
        if any(strcmp(data.Properties.VariableNames,'catchtrial'))
            switch p.Results.sampleOption
                case 'down'
                    %             data=data(data.catchtrial==1,:);
                    idx=data.catchtrial==1;%only include catch trial to have sample equally from correct and error trials
                case 'up'
                    OmissionIdx = (data.catchtrial==1&data.outcome==1);
                    FillIdx = (data.catchtrial==0&data.outcome==1);
                    data.confidence(FillIdx)=randsample(data.confidence(OmissionIdx),sum(FillIdx),true);
                    data.confidenceRaw(FillIdx)=randsample(data.confidenceRaw(OmissionIdx),sum(FillIdx),true);
                    idx=true(height(data),1);
            end
        else
            idx=true(height(data),1);
        end
        
        %data
        dataBins=discretize(data{idx,confidenceName},binEdgesConfidence);
        x=grpstats(data.outcome(idx),dataBins,'mean');
        nx=grpstats(data{idx,confidenceName},dataBins,'length');
        
        %simulation
        simBins=discretize(simData{:,confidenceName},binEdgesConfidence);
        y=grpstats(simData.outcome,simBins,'mean');
        
        %get correlation measures
        if length(x)==length(y)
            confidenceGof(g,:)=correlationXY(x,y,nx,nPermutations,'calibration',confidenceName);
            allBinNumber(g)=binNumber;
            g=g+1;
        end
        
        if any(strcmp(data.Properties.VariableNames,'catchtrial'))
            switch p.Results.sampleOption
                case 'up'
                    data.confidence(FillIdx)=nan;
                    data.confidenceRaw(FillIdx)=nan;
            end
        end
        
        %% vevaiometric correlation
        for mpd=1:length(allMinDataPoints)
            minDataPoints=allMinDataPoints(mpd);
            %binning
            dataBins=discretize(data.realEvidence,binEdgesEvidence).*round(data.outcome-.5);%multiplies bins for error with -1 and bins for correct with 1
            simBins=discretize(simData.realEvidence,binEdgesEvidence).*round(simData.outcome-.5);%
            
            %data
            inclIdx=ismember(dataBins,unique(simBins));
            x=grpstats(data{inclIdx,confidenceName},dataBins(inclIdx),'nanmean');
            nx=grpstats(data{inclIdx,confidenceName},dataBins(inclIdx),'length');
            
            %simulation
            inclIdx=ismember(simBins,unique(dataBins));
            y=grpstats(simData{inclIdx,confidenceName},simBins(inclIdx),'nanmean');
            
            %sufficient numbers
            x=x(nx>minDataPoints);
            y=y(nx>minDataPoints);
            nx=nx(nx>minDataPoints);
            
            %get correlation measures
            if length(x)==length(y)&&~isempty(x)
                confidenceGof(g,:)=correlationXY(x,y,nx,nPermutations,'vevaiometric',confidenceName,minDataPoints);
                allBinNumber(g)=binNumber;
                g=g+1;
            end
        end
        %% conditioned correlation
        if any(strcmp(data.Properties.VariableNames,'catchtrial'))
            switch p.Results.sampleOption
                case 'down'
                    idx=data.catchtrial==1;%only include catch trial to have sample equally from correct and error trials
                case 'up'
                    OmissionIdx = (data.catchtrial==1&data.outcome==1);
                    FillIdx = (data.catchtrial==0&data.outcome==1);
                    data.confidence(FillIdx)=randsample(data.confidence(OmissionIdx),sum(FillIdx),true);
                    data.confidenceRaw(FillIdx)=randsample(data.confidenceRaw(OmissionIdx),sum(FillIdx),true);
                    idx=true(height(data),1);
            end
        end
        %bins
        divideBins=round((data{:,confidenceName}>=prctile(data{idx,confidenceName},50))-.5);
        dataBins=discretize(data.realEvidence,binEdgesEvidence).*divideBins;% multiplies bins for low confidence with -1 and bins for high confidence with 1
        divideBins=round((simData{:,confidenceName}>=prctile(simData{:,confidenceName},50))-.5);
        simBins=discretize(simData.realEvidence,binEdgesEvidence).*divideBins;
        
        %data
        inclIdx=ismember(dataBins,unique(simBins))&idx;
        x=grpstats(data.choice(inclIdx),dataBins(inclIdx),'nanmean');
        nx=grpstats(data.choice(inclIdx),dataBins(inclIdx),'length');
        
        %simulation
        inclIdx=ismember(simBins,unique(dataBins));
        y=grpstats(simData.choice(inclIdx),simBins(inclIdx),'nanmean');
        
        %correlation
        if length(x)==length(y)
            confidenceGof(g,:)=correlationXY(x,y,nx,nPermutations,'conditioned',confidenceName);
            allBinNumber(g)=binNumber;
            g=g+1;
        end
    end
end
confidenceGof=[confidenceGof,table(allBinNumber,'VariableNames',{'binning'})];
end
function confidenceGof=correlationXY(x,y,nx,nPermutations,signature,confidenceName,minDataPoints)
if nargin<7
    minDataPoints=0;
end
%correlation
[~,gof]=fit(y,x,'poly1','exclude',isnan(y)|isnan(x));
ft=fitoptions('Weights',nx,'exclude',isnan(y)|isnan(x));
[~,weightedgof]=fit(y,x,'poly1',ft);
weightedOLS=sum((x-y).^2.*nx)./sum(nx);
OLS=sum((x-y).^2)./length(x);
[rp, pp]=corr(x(~isnan(y)),y(~isnan(y)),'type','Pearson');
np=length(x(~isnan(y)));

% permutation test
idx=find(~isnan(y));
for perms=1:nPermutations
    randIdx=idx(randperm(length(idx)));
    [rshuffle(1,perms)]=corr(x(idx),y(randIdx),'type','Pearson');
end
pshuffle=min(sum(rshuffle(1,:)-rp<0),sum(rshuffle(1,:)-rp>0))./nPermutations;
%assemble
rTable=cell2table(num2cell([rp' pp' np' pshuffle']),'VariableNames',{'PearsonR','PearsonP','PearsonN','PearsonP_Permutation'});
rTable=[rTable, table(rshuffle,'VariableNames',{'PearsonR_Permutation'})];
weightedTable=struct2table(weightedgof);
weightedTable.Properties.VariableNames=cellfun(@(x) strcat('weighted',upper(x(1)),x(2:end)),weightedTable.Properties.VariableNames,'uni',0);
confidenceGof=[cell2table({signature,confidenceName,minDataPoints},'VariableNames',{'signature','confidenceName','minDataPoints'}),...
    rTable,struct2table(gof),weightedTable,table(OLS),table(weightedOLS)];
end
