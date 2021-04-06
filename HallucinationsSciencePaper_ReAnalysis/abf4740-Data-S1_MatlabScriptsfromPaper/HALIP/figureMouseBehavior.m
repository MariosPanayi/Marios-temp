% This script plots behavioral data from humans (c.f. Figure 03 "Mice
% accurately report hallucination-like perception") from % "Striatal
% Dopamine Mediates Hallucination-Like Perception in Mice" by Schmack et
% al. 2020.
%
% KS, January 2021, Cold Spring Harbor schmack@cshl.edu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A - psychometric (group)
% B-D - confidence signatures (example mouse)
% G - confidence fit (group)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT NOTE: Some of the analyses include stochastic simulations (B-G).
% Therefore, results can vary slightly from run to run. For exactly
% repeatable results, set the random generator (e.g. 'randomSeedNumber=1')
% to a fixed value.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear;
randomSeedNumber=now;

%% prelude
% initialize figure and caption
set(0,'defaultAxesFontName','Calibri','defaultTextFontName','Calibri','defaultFigureColor','w','DefaultLegendAutoUpdate','off','defaultAxesTickDir','out','defaultAxesTickDirMode', 'manual','defaultAxesTickLength',[.02 .02]);%set defaults
dataName='mouseData.mat';
figureName='results\FigureMouseBehavior.pdf';
captionName='results\StatsMouseBehavior.txt';
if ~exist('.\results','dir')
    mkdir('results');
end
fid=fopen(captionName,'w');

% layout
cols=5;rows=1;%columns and rows in figure
wid_cm=18;%figure width in cm; Science either 12cm or 5.5cm
px2cm = 1/96 * 2.54;
figHdl=figure('Name','HALIPs in Mice','NumberTitle','off','Position',[0 100 ceil(wid_cm/px2cm) ceil(wid_cm/px2cm)/cols*rows],'Visible','on');
t=tiledlayout(rows,cols,'TileSpacing','compact','Padding','compact');
k=0;

% plot options
minDataPoints=0;
binNumber=5; % for binning calibration curve (binNumber+1) as well vevaiometric and conditioned Psychometric (binNumber)

%% get & prepare data
load(dataName,'trialTab','sessionTab');
[trialTab,thisSessionTab]=selectExperiment(trialTab,sessionTab,'behavior');

%% A- psychometric
% prepare subplot
k=k+1;
h(k)=nexttile;

%prepare group data
binEdges=[-40,-20,-10:10:35];
trialTab.bins=discretize(trialTab.evidence,binEdges);
subIdx=~isnan(trialTab.choice);
resTab=grpstats(trialTab(subIdx,:),{'bins','subjectId'},'nanmean','dataVars',{'evidence','choice'});
resTab.Properties.VariableNames=strrep(resTab.Properties.VariableNames,'nanmean_','mean_');
data=grpstats(resTab,{'bins'},{'mean','std'},'dataVars',{'mean_evidence','mean_choice'});
data.Properties.VariableNames=strrep(data.Properties.VariableNames,'mean_','');

%fit and plot psychometric
[fitresult,gof] = psychometricModel(data,'genModel','guessGaussian');
psychometricPlot(fitresult,data,'axesHandle',h(k),...
    'xLabel','Signal-to-noise (dB)','yLabel','Signal choice (%)','legend','off','binEdges',...
    binEdges,'dataMarkerSize',10,'oddMarker','p','YPercent','on');

%axes cosmetics
h(k).XTick=[-40,0,20];
h(k).XTickLabel={'No signal','0','20'};
h(k).YTick=[0 50 100];
h(k).YLabel.Units='normalized';
h(k).Title.String='';

% add text for sample size
addSampleSize(trialTab,h(k));

%caption content
fprintf(fid,'%%%%%%%%%%%%%%%%%%%%%%Mouse Behavior%%%%%%%%%%%%%%%%%%%%%%\n');
fprintf(fid,'%d mice, %d sessions, %d trials\n',length(unique(trialTab.subjectId)),height(unique(trialTab(:,{'sessionId','subjectId'}))),height(trialTab));
fprintf(fid,'False Alarm Rate %2.0f%s %s %2.0f [mean %s SD]\n',data.choice(1)*100,'%',char(177),data.std_choice(1)*100,char(177));
fprintf(fid,'Variance explained by psychometric %2.2f%s\n',gof.rsquare*100,'%');
clear('data');

%% B-E statistical confidence and time investments
% loop over all subjects to fit confidence model
exampleSubject='K11';
subjects=unique(trialTab.subjectId);
allConfidenceGof=table;%preallocate

% simulate or load
promptMessage = sprintf('Need statistical confidence simulation for each subject.\nLoad existing simulation or perform new simulation?\n(If existing simulation is not found, new simulation is performed in any case.)'); 
button = questdlg(promptMessage, 'Load', 'Load', 'Simulate', 'Load');
if ~exist('confidenceSimulation','dir')
    mkdir('confidenceSimulation');
end

for s=1:length(subjects)
    confidencePath=fullfile('confidenceSimulation',[sprintf(subjects{s}) '.mat']);
    if ~exist(confidencePath,'file')||strcmpi(button,'Simulate')
        %% print progress
        fprintf('Simulating statistical confidence %s (%d/%d)...',subjects{s},s,length(subjects));
        
        %% prepare data
        subIdx=strcmp(trialTab.subjectId,subjects{s})&~isnan(trialTab.choice);
        data=trialTab(subIdx,:);
        data=cosineau(data,'sessionId','confidence');    % remove session variance in confidence
        data.confidence=data.cosineau_confidence_by_sessionId;%rename for confidence model
        
        %% predict statistical confidence
        % infer "evidence level" for no signal trials
        data.oddtrial=data.evidence<-30;
        [fitresult,~] = psychometricModel(data,'genModel','gaussian');
        oddEvidence=norminv(mean(data.choice(data.oddtrial==1)),fitresult.m,fitresult.sigma);%set non-fitted noise trials to predicted evidence
        
        % run model simulation
        randomseed=rng(randomSeedNumber);%for reproducibility, save seed random generator
        [simDataBoot,confidenceRawData] = confidenceModel(data,fitresult,'type','bootstrap',...
            'genModel','gaussian','plotMatch',false,'perceptBin','equalFill','nsim',1E2,'oddEvidence',oddEvidence);
        
        % organize data to be clear about evidence (realEvidence: assume -40dB
        % at no-signal trials, evidence: assume evidence estimated by
        % oddEvidence)
        data.confidenceRaw=confidenceRawData; % confidence in probability units
        data.realEvidence=data.evidence; %-40 noise evidence
        data.evidence=data.evidence;% inferred noise evidence
        data.evidence(data.oddtrial)=repmat(oddEvidence,sum(data.oddtrial),1);
        
        %% calculate fit between statistical confidence and time investments
        [confidenceGof]=confidenceGoodnessOfFit(data,simDataBoot,'nPermutations',1000,'binNumbers',[3],'sampleOption','up');
        %% save
        save(confidencePath,'data','simDataBoot','confidenceGof','randomseed');
        
        %% print progress
        fprintf('Done\n');
        
    elseif strcmpi(button,'Load')
        %% load goodness of fit only
        fprintf('Loading statistical confidence %s (%d/%d)...',subjects{s},s,length(subjects));
        load(confidencePath,'confidenceGof');
        fprintf('Done\n');
    end
    idx=strcmp(confidenceGof.signature,'vevaiometric')&strcmp(confidenceGof.confidenceName,'confidenceRaw');
    allConfidenceGof=[allConfidenceGof;confidenceGof(idx,:)];
    
    %% B-D plot calibration, vevaiometric and conditioned psychometric for example subject
    if ismember(subjects(s),exampleSubject)
        %% load full data if necessary
        if strcmpi(button,'Load')
            load(confidencePath,'data','simDataBoot');
        end
        %% B - calibration
        % prepare panel
        k=k+1;
        h(k)=nexttile;
        
        % bin data
        binEdgesConfidence=prctile(data{:,'confidence'},[0:100/(binNumber+1):100]);
        oddIdx=data.evidence<-20;
        binEdgesEvidence=[min(data.evidence(oddIdx))-10*eps,max(data.evidence(oddIdx))+10*eps,prctile(data.evidence(~oddIdx),[0:100/(binNumber-1):100])];
        
        % plot calibration
        calibrationPlot(simDataBoot,'axesHandle',h(k),'binEdges',0:.1:10,'minDataPoints',minDataPoints,'legend','off');
        calibrationPlot(data,'axesHandle',h(k),'binEdges',binEdgesConfidence,'plotSize',false,'minDataPoints',minDataPoints,...
            'YPercent','on','xlabel','Invested time (s)','ylabel','Accuracy (%)','legend','off','dataMarkerSize',10,'dataLineWidth',1.2,...
            'groupVariable','sessionId','errorMode','std','sampleOption','up');
        
        %axes cosmetics
        h(k).XLim=[2.5 7.5];
        h(k).Title.String='';
        h(k).YLim=[34,100];
        h(k).YLabel.Units='normalized';
        g=findall(h(k),'Type','errorbar');
        g(1).YNegativeDelta(1)=0;
        
        % add sample size
        hdl=addSampleSize(data,h(k),'southeast',2);
        for ah=1:length(hdl)
            hdl(ah).Position(1)=1.1;
        end
        
        %% C - vevaiometric
        % prepare panel
        k=k+1;
        h(k)=nexttile;
        
        %plot model
        vevaiometricPlot(simDataBoot,'axesHandle',h(k),'legend','off','binEdges',-40:2.5:40,'fitLineStyle','-',...
            'fitColors',getColor({'correct','error'}));
        
        %plot data in signal colors
        vevaiometricPlot(data,'axesHandle',h(k),'legend','off','binEdges',binEdgesEvidence,'plotSize',false,...
            'dataColors',getColor({'hit','miss'}),...
            'minDataPoints',minDataPoints,'xLabel','Signal-to-noise (dB)','yLabel','Invested time (s)','dataMarkerSize',10,'dataLineWidth',1.2,...
            'groupVariable','sessionId','errorMode','std','errorSide','two');
        
        %plot data in no-signal colors
        vevaiometricPlot(data,'axesHandle',h(k),'legend','off','binEdges',binEdgesEvidence,'plotSize',false,...
            'dataColors',getColor({'correctReject','falseAlarm'}),...
            'minDataPoints',minDataPoints,'xLabel','Signal-to-noise (dB)','yLabel','Invested time (s)','dataMarkerSize',10,'dataLineWidth',1.2,...
            'groupVariable','sessionId','errorMode','std','errorSide','two');
        
        %remove data points with 'wrong' colors (hits in no-signal colors
        %etc)
        a=findobj(h(k),'Color',getColor({'hit'}));
        a.YData(1)=nan;
        a=findobj(h(k),'Color',getColor({'correctReject'}));
        a.YData(2:end)=nan;
        a=findobj(h(k),'Color',getColor({'miss'}));
        a.YData(1)=nan;
        a=findobj(h(k),'Color',getColor({'falseAlarm'}));
        a.YData(2:end)=nan;
        
        %add legend (extra function below, because complex)
        vevaiometricLegend(h(k));
        
        %axes cosmetics
        a=findobj(h(k),'Color',getColor({'correctReject'}));
        h(k).XTick=[a(1).XData(1),0,20];
        h(k).XTickLabel={'No signal','0','20'};
        h(k).Title.String='';
        h(k).YLim(1)=3.5;
        
        %% D- conditioned psychometric
        % prepare panel
        k=k+1;
        h(k)=nexttile;
        
        % plot model
        conditionedPlot(simDataBoot,'axesHandle',h(k),'legend','off','binEdges',-40:2:40,'fitLineStyle','-','prctileCut',50);
        
        % plot data
        conditionedPlot(data,'axesHandle',h(k),'legend','off','binEdges',binEdgesEvidence,'minDataPoints',minDataPoints,...
            'YPercent','on','xLabel','Signal-to-noise (dB)','yLabel','Signal choice','prctileCut',50,'dataMarkerSize',10,'dataLineWidth',1.2,...
            'groupVariable','sessionId','errorMode','std','errorSide','two','sampleOption','up');
        
        %axes cosmetics
        a=get(h(k),'Children');
        h(k).XTick=[a(1).XData(1),0:20:20];
        h(k).XTickLabel={'No signal','0','20'};
        h(k).Title.String='';
        h(k).YLabel.Units='normalized';
        
        % legend (manual, automatic does not work nicely)
        a=findobj(h(k),'Marker','.');
        [~,highIdx]=min([a(1).Color(1),a(2).Color(1)]);
        highProperties=a(highIdx);
        [~,lowIdx]=max([a(1).Color(1),a(2).Color(1)]);
        lowProperties=a(lowIdx);
        text(h(k),1.1,.96-.74,'High time inv.','FontSize',8,'Color',highProperties.Color,'Units','normalized','HorizontalAlignment','right');
        text(h(k),1.1,.82-.74,'Low time inv.','FontSize',8,'Color',lowProperties.Color,'Units','normalized','HorizontalAlignment','right');
    end
end

%% E summary confidence data
% prepare panel
k=k+1;
h(k)=nexttile;

% prepare data
thisConfidenceGof=allConfidenceGof;
ev=(thisConfidenceGof.PearsonR).^2;
sev=nanmedian(thisConfidenceGof.PearsonR_Permutation.^2,2);
y=[ev;sev];
x=[zeros(size(ev));ones(size(ev))];

% plot beeswarm plot
beeswarm(x,y,'Colormap',getColor({'neutralBold','neutralLight'}),'use_current_axes',0,'dot_size',.45,'MarkerFaceAlpha',1,'Marker','o','corral_style','none',...
    'sort_style','no_sort');

%axes cosmetics
h(k).XTick=[0 1];
h(k).YAxis.Label.String='Variance expl. (R^2)';
h(k).XTickLabel={'Predicted\newlineconfidence','Shuffle'};
h(k).XTickLabelRotation=0;

%add sample size
hdl=addSampleSize(trialTab,h(k),'northeast',2);
for ah=1:length(hdl)
    hdl(ah).Position(1)=1.05;
end
delete(hdl(2));

%% add confidence statistics to caption
fprintf(fid,'Statistical confidence\n');
rsquare=allConfidenceGof{:,'PearsonR'};
p=allConfidenceGof{:,'PearsonP_Permutation'};
fprintf(fid,'explained variance = %2.0f%s (%2.0f-%2.0f) [median (range)]\n',median(rsquare.^2)*100,'%',min(rsquare.^2)*100,max(rsquare.^2)*100);
fprintf(fid,'p<0.05 n=%d, p<0.01 n=%d, p<0.001 n=%d\n',sum(p<0.05),sum(p<0.01),sum(p<0.001));

%% manual optimizations
%% letters
%put letters
toAnnotate=[1:5];
for a=1:length(toAnnotate)
    k=toAnnotate(a);
    an(a)=annotation('textbox','String',char('A'-1+a));
    an(a).Position=[h(k).OuterPosition(1) h(k).OuterPosition(2)+h(k).OuterPosition(4) 0 0];
    an(a).HorizontalAlignment='center';
    an(a).VerticalAlignment='middle';
    an(a).EdgeColor='none';
    an(a).Margin=0;
    an(a).FontWeight='bold';
    an(a).FontSize=10;
end

% reposition letters
x=[0.015 .21 .41 .59 .78];
y=[.955 .5 .345];
for a=1:length(an)
    k=toAnnotate(a);
    an(a).Position=[h(k).OuterPosition(1) h(k).OuterPosition(2)+h(k).OuterPosition(4) 0 0];
    [~,xIdx]=min(abs(x-an(a).Position(1)));
    [~,yIdx]=min(abs(y-an(a).Position(2)));
    an(a).Position=[x(xIdx) y(yIdx) 0 0];
end

%% labels
% move x label up
ns=[1:k];
for n=ns
    h(n).XLabel.Color='w';
    h(n).XLabel.Units='normalized';
    if rem(n,cols)==1
        t(n)=text(h(n),0.98,-.24,h(n).XLabel.String,'FontSize',h(n).XLabel.FontSize,'FontWeight','Normal','HorizontalAlignment','right','VerticalAlignment','top','units','normalized','Color',h(k).XAxis.Color);
    else
        t(n)=text(h(n),0.5,-.24,h(n).XLabel.String,'FontSize',h(n).XLabel.FontSize,'FontWeight','Normal','HorizontalAlignment','center','VerticalAlignment','top','units','normalized','Color',h(k).XAxis.Color);
    end
end

k='E'-64;
h(k).XTickLabel{1}='';
h(k).XTickLabel{2}='';
t(1)=text(h(k),-.1,-.14,'Predicted','Units','Data','HorizontalAlignment','Center','FontSize',h(k).XAxis.FontSize,'Color',h(k).XAxis.Color);
t(2)=text(h(k),-.1,-.29,'Confidence','Units','Data','HorizontalAlignment','Center','FontSize',h(k).XAxis.FontSize,'Color',h(k).XAxis.Color);
t(3)=text(h(k),1.1,-.14,'Shuffle','Units','Data','HorizontalAlignment','Center','FontSize',h(k).XAxis.FontSize,'Color',h(k).XAxis.Color);

%% save figure and caption
set(figHdl,'Visible','on');
exportgraphics(figHdl,figureName,'Resolution',900);
fprintf('Saved plots to %s.\n',figureName)
close(figHdl);

fprintf('Saved statistics to %s.\n',captionName)
fclose(fid);

function vevaiometricLegend(h,x,y)
if nargin<3
    y=[.22,.08];
    if nargin<2
        x=[.02,.02];
    end
end
word='xxCorrect';
string='';
colors=[getColor('correctReject');getColor('hit');repmat(getColor('correct'),length(word)-2,1)];

for c=1:size(colors,1)
    string=[string,sprintf('%scolor[rgb]{%2.4f,%2.4f,%2.4f}%s','\',colors(c,:),word(c))];
end
string=strrep(string,'x','\bullet');
text(h,x(1),y(1),string,'FontSize',8,'Interpreter','tex','Units','normalized','HorizontalAlignment','left');

word='xxError';
string='';
colors=[getColor('falseAlarm');getColor('miss');repmat(getColor('error'),length(word)-2,1)];

for c=1:size(colors,1)
    string=[string,sprintf('%scolor[rgb]{%2.4f,%2.4f,%2.4f}%s','\',colors(c,:),word(c))];
end
string=strrep(string,'x','\bullet');

text(h,x(2),y(2),string,'FontSize',8,'Interpreter','tex','Units','normalized','HorizontalAlignment','left');
end