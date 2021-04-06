function [h]=conditionedPlot(data,varargin)
%% plots the conditioned pscychometric
% inputs
% - data: a table of real or simulated data containing the variables 'evidence',
%        'confidence' and 'outcome', if there is a variable 'percept', data
%        is plotted as a fit, if there is no variable 'percept', data is 
%        plotted as data points
% option:
% 'dataMarker','dataColors','dataLineStyle' Marker and color of data points used for fit
% 'fitLineWidth','fitColors' Width and color of fit
% 'binEdges' Edges for binning data
% 'minDataPoints' if a data point is the average of less than this value,
%                 it is omitted from plotting
%
% Katharina Schmack, Cold Spring Harbor Laboratory March 2019, schmack@cshl.edu

p=inputParser;

validTable = @(x) istable(x) & sum(ismember(x.Properties.VariableNames,{'evidence','choice','confidence','outcome'}))==4;
p.addRequired('data', validTable);
p.addParameter('dataMarker','.');
p.addParameter('dataMarkerSize',20);

p.addParameter('dataColors',getColor({'conditionedHigh','conditionedLow'}),@(x) validateattributes(x,{'numeric'},{'ncols',3,'nrows',2}));
p.addParameter('dataLineWidth',1.5);
p.addParameter('dataLineStyle','none');
p.addParameter('fitMarker','none');
p.addParameter('fitMarkerSize',10);
p.addParameter('fitColors',getColor({'conditionedHigh','conditionedLow'}),@(x) validateattributes(x,{'numeric'},{'ncols',3,'nrows',2}));
p.addParameter('fitLineWidth',2);
p.addParameter('fitLineStyle','-');
p.addParameter('binEdges',-1:.2:1);
p.addParameter('minDataPoints',5);

p.addParameter('xLabel','Evidence');
p.addParameter('yLabel','Choice');
p.addParameter('axesHandle',[]);
p.addParameter('legend','on',@(x) any(validatestring(x,{'on','off'})));
p.addParameter('prctileCut',50);
p.addParameter('YPercent','on',@(x) any(validatestring(x,{'on','off'})));
p.addParameter('groupVariable','none');
p.addParameter('errorMode','sem');
p.addParameter('errorSide','two');
p.addParameter('sampleOption','down',@(x) any(validatestring(x,{'down','up'})));
p.addParameter('correctVarianceX',false);
p.addParameter('correctVarianceY',false);

p.parse(data,varargin{:});


%% first create figure if no axes handle is given
if isempty(p.Results.axesHandle)
    figure;
    h=subplot(1,1,1);
    hold(h,'on');
else
    h=p.Results.axesHandle;
    if ~ishold(h);hold(h,'on');end
end

%% resample data
if any(strcmp(data.Properties.VariableNames,'catchtrial'))
switch p.Results.sampleOption
    case 'down'
        data=data(data.catchtrial==1,:);%only include catch trial to have sample equally from correct and error trials
    case 'up'
        OmissionIdx = (data.catchtrial==1&data.outcome==1);
        FillIdx = (data.catchtrial==0&data.outcome==1);
        data.confidence(FillIdx)=randsample(data.confidence(OmissionIdx),sum(FillIdx),true);
end
end

% correct Variance if wished for
if p.Results.correctVarianceX
    data=cosineau(data,p.Results.groupVariable,'evidence');
    data.evidence=data{:,end};
end
if p.Results.correctVarianceY
    data=cosineau(data,p.Results.groupVariable,'outcome');
    data.outcome=data{:,end};
end


%% prepare figure 
xmin=min(data.evidence)-.05*range(data.evidence);
xmax=max(data.evidence)+.05*range(data.evidence);
set(h,'xlim',[xmin xmax]);
switch p.Results.YPercent
    case 'on'
        h.YLabel.String=[p.Results.yLabel '(%)'];
    case 'off'
        h.YLabel.String=p.Results.yLabel;
end

h.XLabel.String=p.Results.xLabel;
title(h,'Conditioned Psychometric')

%% prepare formatting depending on whether fitted or real data will be plotted
formatCommand=['set(lineHdl,''Color'',p.Results.Colors(k,:),''Marker'',p.Results.Marker,''MarkerSize'',p.Results.MarkerSize,''LineWidth'',p.Results.LineWidth,''LineStyle'',p.Results.LineStyle);'];
if ismember('percept', data.Properties.VariableNames)
    %formatCommand=strrep(formatCommand,'p.Results.Colors(k,:)','[p.Results.Colors(k,:) .5]');
    formatCommand=strrep(formatCommand,'p.Results.','p.Results.fit');
    legStrings={'model high confidence','model low confidence'};
    workAround=colorGradient(p.Results.fitColors(1,:),[1 1 1],4);%transparency does not work for errorbar
    fitColors(1,:)=workAround(3,:);
    workAround=colorGradient(p.Results.fitColors(2,:),[1 1 1],4);%transparency does not work for errorbar
    fitColors(2,:)=workAround(3,:);
    formatCommand=strrep(formatCommand,'p.Results.fitColors','fitColors');

else
    formatCommand=strrep(formatCommand,'p.Results.','p.Results.data');
    legStrings={'high confidence','low confidence'};
end

%% bin and plot data
bins=discretize(data.evidence,p.Results.binEdges);
for k=1:2%loop over high and low confidence
    if k==1
        inclIdx=data.confidence>prctile(data.confidence,p.Results.prctileCut);
    elseif k==2
        inclIdx=data.confidence<prctile(data.confidence,100-p.Results.prctileCut);
    end
    if strcmp(p.Results.groupVariable,'none')
        x=grpstats(data.evidence(inclIdx),bins(inclIdx),'mean');
        y=grpstats(data.choice(inclIdx),bins(inclIdx),'mean');
        ye(:,1)=zeros(size(y));
        ye(:,2)=zeros(size(y));
        
        n=grpstats(data.evidence(inclIdx),bins(inclIdx),'length');
        plotIdx=n>p.Results.minDataPoints;
    else
        data.bins=bins;
        resTable=grpstats(data(inclIdx,:),{p.Results.groupVariable,'bins'},{'mean'},'DataVars',{'choice','evidence'});
        resTable.Properties.VariableNames=strrep(resTable.Properties.VariableNames,'mean_','');
        resTable(resTable.GroupCount<p.Results.minDataPoints,:)=[];
        sumTable=grpstats(resTable,{'bins'},{'mean',p.Results.errorMode},'DataVars',{'evidence','choice'});        
        
        x=sumTable.mean_evidence;
        y=sumTable.mean_choice;
        ye=zeros(length(y),2);
        ye(:,k)=sumTable{:,sprintf('%s_choice',p.Results.errorMode)};%std_outcome;
        plotIdx=true(size(y));
    end
    
    switch p.Results.YPercent
        case 'on'
            y=y*100;
            ye=ye.*100;
            ylimits=[0 100];
        case 'off'
            ylimits=[0 1];
    end
    switch p.Results.errorSide
        case 'two'
            lineHdl=errorbar(h,x(plotIdx),y(plotIdx),ye(plotIdx,k),'CapSize',0);
        case 'one'
            lineHdl=errorbar(h,x(plotIdx),y(plotIdx),ye(plotIdx,2),ye(plotIdx,1),'CapSize',0);
    end
    eval(formatCommand);
        clear('x','x','n','ye');

end

%% legend (if specified)
if strcmp(p.Results.legend,'on')
    hLegend = findobj(h, 'Type', 'Legend');
    if isempty(hLegend)
        hLegend = legend(h,legStrings,'Location','Best');
        hLegend.Box = 'off';
        hLegend.ItemTokenSize=[10 12];
    else
        hLegend.String = [hLegend.String legStrings];
    end
end

end


