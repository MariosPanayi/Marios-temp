function [h]=vevaiometricPlot(data,varargin)
%% plots the vevaiometric
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
% Katharina Schmack, Cold Spring Harbor, March 2019, schmack@cshl.edu

p=inputParser;
validTable = @(x) istable(x) & sum(ismember(x.Properties.VariableNames,{'evidence','choice','confidence','outcome'}))==4;
p.addRequired('data', validTable);
p.addParameter('dataMarker','.');
p.addParameter('dataMarkerSize',20);

p.addParameter('dataColors',getColor({'correct','error'}),@(x) validateattributes(x,{'numeric'},{'ncols',3,'nrows',2}));

p.addParameter('dataLineWidth',1.5);
p.addParameter('dataLineStyle','none');
p.addParameter('fitMarker','none');
p.addParameter('fitMarkerSize',10);
p.addParameter('fitColors',getColor({'correct','error'}),@(x) validateattributes(x,{'numeric'},{'ncols',3,'nrows',2}));
p.addParameter('fitLineWidth',2);
p.addParameter('fitLineStyle','-');
p.addParameter('binEdges',[-1:.2:1]);
p.addParameter('minDataPoints',0);
p.addParameter('annotate',[]);
p.addParameter('plotSize',false);

p.addParameter('xLabel','Evidence');
p.addParameter('yLabel','Confidence');
p.addParameter('axesHandle',[]);
p.addParameter('legend','on',@(x) any(validatestring(x,{'on','off'})));
p.addParameter('groupVariable','none');
p.addParameter('errorMode','sem');
p.addParameter('errorSide','two');
p.addParameter('correctVarianceX',false);
p.addParameter('correctVarianceY',false);
p.addParameter('twoTone',false);

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

%% bin data and do figure stuff
xmin=min(data.evidence)-.05*range(data.evidence);
xmax=max(data.evidence)+.05*range(data.evidence);
set(h,'xlim',[xmin xmax]);
h.YLabel.String=p.Results.yLabel;
h.XLabel.String=p.Results.xLabel;
title(h,'Vevaiometric')



%% prepare formatting depending on whether fitted or real data will be plotted
if p.Results.plotSize&&~ismember('confidenceRaw', data.Properties.VariableNames)
    formatCommand=['set(lineHdl,''MarkerFaceColor'',p.Results.Colors(k,:),''MarkerEdgeColor'',p.Results.Colors(k,:),''Marker'',p.Results.Marker,''LineWidth'',p.Results.LineWidth);'];
else
    formatCommand=['set(lineHdl,''Color'',p.Results.Colors(k,:),''Marker'',p.Results.Marker,''MarkerSize'',p.Results.MarkerSize,''LineWidth'',p.Results.LineWidth,''LineStyle'',p.Results.LineStyle);'];
end
if ismember('percept', data.Properties.VariableNames)
    formatCommand=strrep(formatCommand,'p.Results.','p.Results.fit');
    workAround=colorGradient(p.Results.fitColors(1,:),[1 1 1],4);%transparency does not work for errorbar
    fitColors(1,:)=workAround(3,:);
    workAround=colorGradient(p.Results.fitColors(2,:),[1 1 1],4);%transparency does not work for errorbar
    fitColors(2,:)=workAround(3,:);
    formatCommand=strrep(formatCommand,'p.Results.fitColors','fitColors');

    legStrings={'correct','error'};
else
    formatCommand=strrep(formatCommand,'p.Results.','p.Results.data');
    legStrings={'correct','error'};
end

% correct Variance if wished for
if p.Results.correctVarianceX
    data=cosineau(data,p.Results.groupVariable,'evidence');
    data.evidence=data{:,end};
end
if p.Results.correctVarianceY
    data=cosineau(data,p.Results.groupVariable,'confidence');
    data.confidence=data{:,end};
end

bins=discretize(data.evidence,p.Results.binEdges);
for k=1:2%loop over correct and incorrect
    if p.Results.dataColors(k,:)~=[1 1 1]
        inclIdx=data.outcome==rem(k,2);%1-correct for k==1, 0-error for k==2
        if strcmp(p.Results.groupVariable,'none')
            x=grpstats(data.evidence(inclIdx),bins(inclIdx),'mean');
            y=grpstats(data.confidence(inclIdx),bins(inclIdx),'mean');
            ye(:,1)=zeros(size(y));
            ye(:,2)=zeros(size(y));
            n=grpstats(data.confidence(inclIdx),bins(inclIdx),'length');
            plotIdx=n>p.Results.minDataPoints;
        else
            data.bins=bins;
            resTable=grpstats(data(inclIdx,:),{p.Results.groupVariable,'bins'},{'mean'},'DataVars',{'confidence','evidence'});
            resTable.Properties.VariableNames=strrep(resTable.Properties.VariableNames,'mean_','');
            resTable(resTable.GroupCount<p.Results.minDataPoints,:)=[];
            sumTable=grpstats(resTable,{'bins'},{'mean',p.Results.errorMode},'DataVars',{'evidence','confidence'});
            
            x=sumTable.mean_evidence;
            y=sumTable.mean_confidence;
            ye=zeros(length(y),2);
            ye(:,k)=sumTable{:,sprintf('%s_confidence',p.Results.errorMode)};%std_outcome;            
            plotIdx=true(size(y));
        end
        
    
        if p.Results.plotSize
            ns=rescale(n(plotIdx),p.Results.dataMarkerSize*10,50*p.Results.dataMarkerSize);
            lineHdl=scatter(h,x(plotIdx),y(plotIdx),ns);%,'Marker',p.Results.dataMarker,'MarkerFaceColor',p.Results.dataColor,'MarkerEdgeColor',p.Results.dataColor)
        else
            switch p.Results.errorSide
                case 'two'
                    lineHdl=errorbar(h,x(plotIdx),y(plotIdx),ye(plotIdx,k),'CapSize',0);
                case 'one'
                    lineHdl=errorbar(h,x(plotIdx),y(plotIdx),ye(plotIdx,2),ye(plotIdx,1),'CapSize',0);
            end
        end
        eval(formatCommand);
        clear('x','y','ye');
    end
end

%final figure stuff
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

%annotate
if ~isempty(p.Results.annotate)
    str=sprintf('r_{2}=%2.2f',p.Results.annotate.rsquare);
    xpos=get(h,'xlim')+diff((get(h,'xlim'))*.7);xpos=xpos(1);
    ypos=get(h,'ylim')+diff((get(h,'ylim'))*.2);ypos=ypos(1);
    text(h,xpos,ypos,str,'FontSize',8,'HorizontalAlignment','left')
end




