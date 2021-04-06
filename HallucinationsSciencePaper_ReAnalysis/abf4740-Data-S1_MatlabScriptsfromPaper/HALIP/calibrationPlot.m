function [h]=calibrationPlot(data,varargin)
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
% Katharina Schmack, Cold Spring Harbor Laboratory, March 2019, schmack@cshl.edu

p=inputParser;
validTable = @(x) istable(x) & sum(ismember(x.Properties.VariableNames,{'evidence','choice','confidence','outcome'}))==4;
p.addRequired('data', validTable);

% plot parameters
p.addParameter('dataMarker','.');
p.addParameter('dataMarkerSize',20);
p.addParameter('dataColors',getColor({'calibration'}),@(x) validateattributes(x,{'numeric'},{'ncols',3,'nrows',1}));
p.addParameter('dataLineWidth',1.5);
p.addParameter('dataLineStyle','none');
p.addParameter('fitMarker','none');
p.addParameter('fitMarkerSize',10);
p.addParameter('fitColors',getColor({'calibration'}),@(x) validateattributes(x,{'numeric'},{'ncols',3,'nrows',1}));
p.addParameter('fitLineWidth',2);
p.addParameter('fitLineStyle','-');
p.addParameter('binEdges',[]);
p.addParameter('binNumber',5);

p.addParameter('minDataPoints',5);
p.addParameter('plotSize',false);

p.addParameter('xLabel','Confidence');
p.addParameter('yLabel','Accuracy');
p.addParameter('axesHandle',[]);
p.addParameter('legend','off');
p.addParameter('YPercent','on',@(x) any(validatestring(x,{'on','off'})));
p.addParameter('sampleOption','down',@(x) any(validatestring(x,{'down','up'})));
p.addParameter('groupVariable','none');
p.addParameter('errorMode','sem');
p.addParameter('errorSide','two');
p.addParameter('correctVarianceX',false);
p.addParameter('correctVarianceY',false);
p.parse(data,varargin{:});


%% first create figure if no axes handle is given
if isempty(p.Results.axesHandle)
    figure;
    h=subplot(1,1,1);
    hold(h);
else
    h=p.Results.axesHandle;
    if ~ishold(h);hold(h,'on');end
end

%% bin data and do figure stuff
xmin=min(data.confidence)-.05*range(data.confidence);
xmax=max(data.confidence)+.05*range(data.confidence);
set(h,'xlim',[xmin xmax]);
h.YLabel.String=p.Results.yLabel;
h.XLabel.String=p.Results.xLabel;
title(h,'Calibration')

%% prepare formatting depending on whether fitted or real data will be plotted
if p.Results.plotSize
    formatCommand=['set(lineHdl,''MarkerFaceColor'',p.Results.Colors(1,:),''MarkerEdgeColor'',p.Results.Colors(1,:),''Marker'',p.Results.Marker,''LineWidth'',p.Results.LineWidth);'];
else
    formatCommand=['set(lineHdl,''Color'',p.Results.Colors(1,:),''Marker'',p.Results.Marker,''MarkerSize'',p.Results.MarkerSize,''LineWidth'',p.Results.LineWidth,''LineStyle'',p.Results.LineStyle);'];
end


if ismember('percept', data.Properties.VariableNames)
    workAround=colorGradient(p.Results.fitColors,[1 1 1],4);%transparency does not work for errorbar
    fitColor=workAround(2,:);
    formatCommand=strrep(formatCommand,'p.Results.','p.Results.fit');
    formatCommand=strrep(formatCommand,'p.Results.fitColors(1,:)','fitColor');
else
    formatCommand=strrep(formatCommand,'p.Results.','p.Results.data');
end

%% downsample data
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
    data=cosineau(data,p.Results.groupVariable,'confidence');
    data.confidence=data{:,end};
end
if p.Results.correctVarianceY
    data=cosineau(data,p.Results.groupVariable,'outcome');
    data.outcome=data{:,end};
end

%% bin data
%use 12.5 percentils if no binEdges are specified
if isempty(p.Results.binEdges)
    binEdges=prctile(data{:,'confidence'},[0:100/p.Results.binNumber:100]);
    
else %otherwise use binEdges
    binEdges=p.Results.binEdges;
end
bins = discretize(data.confidence,binEdges);

%% plot data
if strcmp(p.Results.groupVariable,'none')
    x=grpstats(data.confidence,bins,'mean');
    y=grpstats(data.outcome,bins,'mean');
    ye=zeros(size(y));
    n=grpstats(data.confidence,bins,'length');
    plotIdx=n>p.Results.minDataPoints;
    
else
    data.bins=bins;
    
    resTable=grpstats(data,{p.Results.groupVariable,'bins'},{'mean'},'DataVars',{'confidence','outcome'});
    resTable.Properties.VariableNames=strrep(resTable.Properties.VariableNames,'mean_','');
    resTable(resTable.GroupCount<p.Results.minDataPoints,:)=[];
    sumTable=grpstats(resTable,{'bins'},{'mean',p.Results.errorMode},'DataVars',{'outcome','confidence'});
    
    x=sumTable.mean_confidence;
    y=sumTable.mean_outcome;
    ye=sumTable{:,sprintf('%s_outcome',p.Results.errorMode)};%std_outcome;
    plotIdx=true(size(y));
end

switch p.Results.YPercent
    case 'on'
        y=y*100;
        ye=ye*100;
end
if p.Results.plotSize
    ns=rescale(n(plotIdx),p.Results.dataMarkerSize*10,50*p.Results.dataMarkerSize);
    lineHdl=scatter(h,x(plotIdx),y(plotIdx),ns);%,'Marker',p.Results.dataMarker,'MarkerFaceColor',p.Results.dataColor,'MarkerEdgeColor',p.Results.dataColor)
else
    switch p.Results.errorSide
        case 'two'
            lineHdl=errorbar(h,x(plotIdx),y(plotIdx),ye(plotIdx),'CapSize',0);
        case 'one'
            lineHdl=errorbar(h,x(plotIdx),y(plotIdx),ye(plotIdx),zeros(size(ye(plotIdx))),'CapSize',0);
        case 'zero'
            lineHdl=errorbar(h,x(plotIdx),y(plotIdx),zeros(size(ye(plotIdx))),'CapSize',0);
            
    end
end
eval(formatCommand);

%% add legend if specified
if ~strcmp(p.Results.legend,'off')
    [hLegend] = legend(h,p.Results.legend,'Location','SouthEast');
    hLegend.Box = 'off';
    hLegend.ItemTokenSize=[10 30];
end




