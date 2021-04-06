function [h]=PsychometricPlot(fitresult,data,varargin)
%% plots a psychometric curve
% inputs (required)
% - fitresult: either a fit object returned by the matlab fit routine (e.g.
% as produced by PsychometricModel.m) or table with simulated data
% inputs (optional)
% - data:   some real data with evidence and response (if not provided,
%           only fitted curve will be plotted) 
%           NOTE: if data contains a variable whose name starts
%           with 'std_' or 'sem_', error bars are automatically plotted
% inputs (options: see below for explanation)
% 'dataMarker','dataColor' Marker and color of data points used for fit
% 'oddMarker','oddColor' Marker and color of data points not included in
% fit
% 'fitLineWidth','fitLineColor' Width and color of fit
% 'binEdges' Edges for binning data, if not specified binned into
% 10%prctiles
% 'exclude' Index to data that was excluded from fit and that will be
% marked in plot
% 'annotate' structure with goodness of fit measures that will be printed
% in figure

%% parse inputs
p=inputParser;
p.addRequired('fitresult',@(x) isobject(x)|istable(x));
validTable = @(x) istable(x) & sum(ismember(x.Properties.VariableNames,{'evidence','choice'}))==2;
p.addOptional('data',table,validTable);
% plot options
p.addParameter('dataMarker','.');
p.addParameter('dataMarkerSize',10);
p.addParameter('dataColor',[0.2,0.2,0.2]);
p.addParameter('dataFaceColor',[0.2,0.2,0.2]);
p.addParameter('dataEdgeColor',[0.2,0.2,0.2]);
p.addParameter('dataLineStyle','none');
p.addParameter('dataLineWidth',1.5);
p.addParameter('oddMarker','s'); % data that was excluded from fit (data.oddtrial==1)
p.addParameter('oddColor',[0.2,0.2,0.2]);
p.addParameter('oddLineStyle','none');
p.addParameter('fitLineWidth',2);
p.addParameter('fitLineColor',[.8,.8,.8]);
p.addParameter('fitLineStyle','-');

p.addParameter('plotSize',false);

% binning
p.addParameter('binEdges',[]);%if empty, binEdges are based on prctiles of evidence
p.addParameter('fitBinEdges',[]);
p.addParameter('minPoints',0); % minimum of data points at each bin to be included in plot

% axis
p.addParameter('axesHandle',[]);% handle to axes for plot, if empty, new figure is generated

% labels
p.addParameter('xLabel','evidence');% 
p.addParameter('yLabel','choice 1');

% legend
p.addParameter('annotate',[]);%if empty, fit parameters are not 
p.addParameter('legend','on',@(x) any(validatestring(x,{'on','off'}))); %legend
p.addParameter('YPercent','on',@(x) any(validatestring(x,{'on','off'}))); % Y axis in percent or in proportion

p.parse(fitresult,data,varargin{:});

%% prepare data
if isempty(data)
    x = [];
    y =[];
else
    x = data.evidence;
    y = data.choice;
end

switch p.Results.YPercent
    case 'on'
        y=y*100;
        if any(strcmp(data.Properties.VariableNames,'sem_choice'))
            data.sem_choice=data.sem_choice*100;
        end
        if any(strcmp(data.Properties.VariableNames,'std_choice'))
            data.sem_choice=data.std_choice*100;
        end
        
        ylimits=[0 100];
    case 'off'
        ylimits=[0 1];
end


if ismember('oddtrial', data.Properties.VariableNames)
    exclude=data.oddtrial;
else
    exclude=false(size(x));
end

%% first create figure if no axes handle is given
if isempty(p.Results.axesHandle)
    figure;
    h=subplot(1,1,1);
    hold(h);
else
    h=p.Results.axesHandle;
    if ~ishold(h);hold(h);end
end

%% bin data into 10% if no Edges specified
if isempty(p.Results.binEdges)
    binEdges=[prctile(x,[0:10:100]) max(x)+eps];
else
    binEdges=p.Results.binEdges;
end

%% bin fitted data data into 1% if no Edges specified
if isempty(p.Results.fitBinEdges)
    fitBinEdges=[prctile(x,[0:1:100]) max(x)+eps];
else
    fitBinEdges=p.Results.fitBinEdges;
end

%% first plot fit
if istable(fitresult)
    xsim = fitresult.evidence;
    ysim = fitresult.choice;
    bins=discretize(xsim,fitBinEdges);
    inclIdx=~isnan(xsim)&~isnan(ysim);
    xfit=grpstats(xsim(inclIdx),bins(inclIdx),'mean');
    yfit=grpstats(ysim(inclIdx),bins(inclIdx),'mean');    
else
    xfit=linspace(min(fitBinEdges)-.1*range(fitBinEdges),max(fitBinEdges)+.1*range(fitBinEdges),1E5);
    yfit=feval(fitresult,xfit);
end
if strcmp(p.Results.YPercent,'on')
    yfit=yfit*100;
end
plot(h,xfit,yfit,'LineStyle',p.Results.fitLineStyle,'Color',p.Results.fitLineColor,'LineWidth',p.Results.fitLineWidth);
legstr={'fit'};

%% second plot data included in fit
if ~isempty(x)&&~isempty(y)
    bins=discretize(x,binEdges);
    inclIdx=~exclude;
    xdata=grpstats(x(inclIdx),bins(inclIdx),'mean');
    ydata=grpstats(y(inclIdx),bins(inclIdx),'mean');
    ndata=grpstats(x(inclIdx),bins(inclIdx),'length');
    if any(strcmp(data.Properties.VariableNames,'sem_choice'))
        sem=data.sem_choice;
        errorbar(h,xdata,ydata,sem,'CapSize',0,'Marker',p.Results.dataMarker,'Color',p.Results.dataColor,'MarkerFaceColor',p.Results.dataFaceColor,'MarkerEdgeColor',p.Results.dataEdgeColor,'LineStyle',p.Results.dataLineStyle,'MarkerSize',p.Results.dataMarkerSize,'LineWidth',p.Results.dataLineWidth)
    end
    if p.Results.plotSize
        scatter(h,xdata,ydata,rescale(ndata,100,1000),'Marker',p.Results.dataMarker,'MarkerFaceColor',p.Results.dataFaceColor,'MarkerEdgeColor',p.Results.dataEdgeColor,'LineWidth',p.Results.dataLineWidth)
    else
        plot(h,xdata(ndata>p.Results.minPoints),ydata(ndata>p.Results.minPoints),'Color',p.Results.dataColor,'Marker',p.Results.dataMarker,'MarkerFaceColor',p.Results.dataFaceColor,'MarkerEdgeColor',p.Results.dataEdgeColor,'LineStyle',p.Results.dataLineStyle,'MarkerSize',p.Results.dataMarkerSize,'LineWidth',p.Results.dataLineWidth)
    end
    legstr={'fit','data'};
    
    %% third plot data not included in fit (data.oddtrial==1)
    inclIdx=logical(exclude);
    if sum(inclIdx)>0
        xdata=grpstats(x(inclIdx),bins(inclIdx),'mean');
        ydata=grpstats(y(inclIdx),bins(inclIdx),'mean');
        ndata=grpstats(x(inclIdx),bins(inclIdx),'length');
        if p.Results.plotSize
            scatter(h,xdata,ydata,ndata*5E3./length(x),'Marker',p.Results.oddMarker,'MarkerFaceColor',p.Results.oddColor,'MarkerEdgeColor',p.Results.oddColor)
        else
            plot(h,xdata,ydata,'Marker',p.Results.oddMarker,'MarkerFaceColor',p.Results.oddColor,'MarkerEdgeColor',p.Results.oddColor,'LineStyle',p.Results.oddLineStyle)
        end
        legstr={'fit','data','excluded'};
    end
end

% axes cosmetics
xmin=min(x)-.1*range(x);
xmax=max(x)+.1*range(x);
h.XLim=[xmin xmax];
h.YLim=ylimits;
h.YLabel.String=p.Results.yLabel;
h.XLabel.String=p.Results.xLabel;
h.Title.String='Psychometric';

% legend
if strcmp(p.Results.legend,'on')
    lg=legend(h,legstr,'Location','SouthEast');
    lg.Box='off';
    lg.ItemTokenSize=[10 30];
end

% annotation
if ~isempty(p.Results.annotate)
    str=sprintf('m=%2.2f\ns=%2.2f\nr_{2}=%2.2f',fitresult.m,fitresult.sigma,p.Results.annotate.rsquare);
    xpos=get(h,'xlim')+diff((get(h,'xlim'))*.7);xpos=xpos(1);
    ypos=get(h,'ylim')+diff((get(h,'ylim'))*.2);ypos=ypos(1);
    text(h,xpos,ypos,str,'FontSize',8,'HorizontalAlignment','left')
end

%final figure stuff
end

