% This script plots behavioral data from mice (c.f. Figure 02A-H
% "Hallucination-like perceptions are increased by hallucination-related
% manipulations in mice") from % "Striatal Dopamine Mediates
% Hallucination-Like Perception in Mice" by Schmack et al. 2020.
%
% KS, January 2021, Cold Spring Harbor schmack@cshl.edu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A - expectation effect on false alarm rate (group)
%% B - expectation effect on false alarm confidence (group)
%% C - expectation effect on high-confidence false alarm confidence (group)
%% D-F - expectation effect on confidence calibration (example subject)
%% G - expectation effect on fit between statistical confidence and time investments (group)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT NOTE: Some of the analyses include stochastic simulations (C-G).
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
figureName='results\FigureMouseExpectations.pdf';
captionName='results\StatsMouseExpectations.txt';
if ~exist('results','dir')
    mkdir('results');
end
fid=fopen(captionName,'w');

% layout
cols=4;rows=2;%columns and rows in figure
wid_cm=13;%figure width in cm; Science either 12cm or 5.5cm
px2cm = 1/96 * 2.54;
figHdl=figure('Name','Expectations & HALIPs','NumberTitle','off','Position',[0 100 ceil(wid_cm/px2cm) ceil(wid_cm/px2cm)/cols*rows],'Visible','on');
tiledlayout(rows,cols,'TileSpacing','compact','Padding','compact');k=0;

% plot options
colors=getColor({'biasLow','biasMedium','biasHigh'});
minDataPoints=0;
binNumber=5;

%% get & prepare data
load(dataName,'trialTab','sessionTab');
[trialTab,thisSessionTab]=selectExperiment(trialTab,sessionTab,'expectations');

% create variable for false alarm rate and false alarm confidence
signalIdx=trialTab.evidence>-20;
nofaIdx=signalIdx|trialTab.choice==0;

% falseAlarmRate contains choices (0/1)*100 for no-signal trials and nans for signal
% trials. This is for calculating the false rate in %.
trialTab.falseAlarmRate=trialTab.choice.*100;
trialTab.falseAlarmRate(signalIdx)=nan;

% falseAlarmConfidence contains time investments for no-signal trials and nans for signal
% trials. This is for calculating the false confidence.
trialTab.falseAlarmConfidence=trialTab.confidence;
trialTab.falseAlarmConfidence(nofaIdx)=nan;

% split false alarms into high and low confidence false alarm based on
% median (as in conditioned psychometric)
% first upsample data to account for incomplete sampling on correct trials
% (time investment is obtained on correct catch trials only)
randomseed=rng(randomSeedNumber);%for reproducibility
OmissionIdx = (trialTab.catchtrial==1&trialTab.outcome==1);
FillIdx = (trialTab.catchtrial==0&trialTab.outcome==1);
trialTab.confidence(FillIdx)=randsample(trialTab.confidence(OmissionIdx),sum(FillIdx),true);

medianConfidence=prctile(trialTab{:,'confidence'},50);
confHighIdx=trialTab.confidence>=medianConfidence;
confLowIdx=trialTab.confidence<medianConfidence;
trialTab.highFalseAlarmRate=nan(height(trialTab),1);
trialTab.lowFalseAlarmRate=nan(height(trialTab),1);
trialTab.highFalseAlarmRate(confHighIdx)=trialTab.falseAlarmRate(confHighIdx);
trialTab.lowFalseAlarmRate(confLowIdx)=trialTab.falseAlarmRate(confLowIdx);


%% calculate group means and statistitcs
[sumTable,resTable,strings]=groupMeanSem(trialTab,{'subjectId'},{'blockBias'},{'falseAlarmRate','falseAlarmConfidence','highFalseAlarmRate','lowFalseAlarmRate'},0);

% print out statistics
fprintf(fid,'%%%%%%%%%%%%%%%%%%%%%%Expectation Effect on HALIPs%%%%%%%%%%%%%%%%%%%%%%\n');
fprintf(fid,'%d mice, %d sessions, %d trials\n',length(unique(trialTab.subjectId)),length(unique(trialTab.sessionId)),height(trialTab));
fprintf(fid,'ANOVA false alarms: main effect signal proportion %s\n',strings{1});
fprintf(fid,'ANOVA false alarm confidence: main effect  signal proportion %s\n',strings{2});
fprintf(fid,'ANOVA false alarm (high): main effect  signal proportion %s\n',strings{3});
fprintf(fid,'ANOVA false alarm (low): main effect  signal proportion %s\n',strings{4});

%% A/C false alarm rate/high confidence false alarm rate
hlVars={'falseAlarmRate','highFalseAlarmRate'};
hlYLabel={'FA rate (%)',{'High invested time FA rate (%)'}};
for hl=1:2
    hlVar=sprintf('%s',hlVars{hl});
    if hl==2 %make space for confidence
        k=k+1;
        h(k)=nexttile;
        hold on;
    end
    % prepare panel
    k=k+1;
    h(k)=nexttile;
    hold on;
    
    % plot bars
    for l=1:3
        x=sumTable{l,'blockBias'};
        y=sumTable{l,['mean_',hlVar]};
        bar(h(k),x,y,'facecolor',colors(l,:),'barwidth',.2);
    end
    
    %significance
    astXCor={[.5 .7],[.3 .5],[.3 .7]};
    astPval=[sumTable{sumTable.blockBias==.5,(strcat(hlVar, "_pairwise_pval_0.7"))},...
        sumTable{sumTable.blockBias==.3,(strcat(hlVar, "_pairwise_pval_0.5"))},...
        sumTable{sumTable.blockBias==.3,(strcat(hlVar, "_pairwise_pval_0.7"))}];
    sigHdl=sigstar(astXCor([2 1 3]),astPval([2 1 3]),1);
    formatSigHdl(sigHdl,h(k));
    
    % single lines (to get scaling)
    p1=plot(h(k),reshape([resTable.blockBias],8,3)',reshape([resTable{:,['corrected_nanmean_',hlVar]}],8,3)','Color',getColor({'singleLine'}),'LineWidth',1,'Marker','none','LineStyle','-');
    for px=1:length(p1)
        p1(px).Color(4)=0.7;%set transparency
    end
    
    % axis cosmetics
    set(h(k),'xlim',[0.15 .85],'xtick',[.3 .5 .7],'xticklabel',{'0.3','0.5','0.7'})
    h(k).XLabel.String='Signal proportion';
    h(k).Clipping='off';
    h(k).YLabel.String=hlYLabel{hl};
    
    % add sample size
    if hl==1
        hdl{k}=addSampleSize(trialTab,h(k),'southeast');
        for hd=1:length(hdl{k})
            hdl{k}(hd).Color=[.9 .9 .9];
        end
    end
end

%% B false alarm confidence
k=k-1;
axes(h(k));
hold on;

% plot dots
for l=1:3
    x=sumTable{l,'blockBias'};
    y=sumTable{l,'mean_falseAlarmConfidence'};
    s=zeros(size(y));
    errorbar(h(k),x,y,s,'color',colors(l,:),'MarkerFaceColor',colors(l,:),'LineStyle',...
        'none','Marker','o','MarkerSize',5,'LineWidth',1);
end

% axes cosmetics
set(h(k),'xlim',[0.15 .85],'xtick',[.3 .5 .7],'xticklabel',{'0.3','0.5','0.7'})
h(k).XLabel.String='Signal proportion';
h(k).Clipping='off';
h(k).YLim=[4.2 4.9];

%signficance
astCor={[.3 .5],[.5 .7],[.3 .7]};
astPval=[sumTable{sumTable.blockBias==.3,("falseAlarmConfidence_pairwise_pval_0.5")},...
    sumTable{sumTable.blockBias==.5,("falseAlarmConfidence_pairwise_pval_0.7")},...
    sumTable{sumTable.blockBias==.3,("falseAlarmConfidence_pairwise_pval_0.7")}];
sigHdl=sigstar(astCor([2 1 3]),astPval([2 1 3]),1);
delete(sigHdl(1,:));
sigHdl(1,:)=[];
formatSigHdl(sigHdl,h(k));

% plot single lines
p1=plot(h(k),reshape([resTable.blockBias],8,3)',reshape([resTable.corrected_nanmean_falseAlarmConfidence],8,3)','Color',getColor({'singleLine'}),'LineWidth',1,'Marker','none','LineStyle','-');
uistack(p1,'bottom');
for px=1:length(p1)
    p1(px).Color(4)=.7;%set transparency
end

% axis cosmetics
h(k).YLabel.String={'FA invested time (s)'};
h(k).Clipping='off';


%% D psychometric curves
% prepare panel
k=k+2;
h(k)=nexttile;
hold on;

% fit and plot psychometrics
binEdges=[-45,prctile(trialTab.evidence(trialTab.evidence>-15),[0:100/4:100])];
trialTab.bins=discretize(trialTab.evidence,binEdges);
blocks=[.3 .5 .7];
for kb=1:length(blocks)
    subIdx=trialTab.blockBias==blocks(kb)&~isnan(trialTab.choice);
    resTab=grpstats(trialTab(subIdx,:),{'bins','subjectId'},{'nanmean'},'dataVars',{'evidence','choice'});
    resTab.Properties.VariableNames=strrep(resTab.Properties.VariableNames,'nanmean_','mean_');
    data=grpstats(resTab,{'bins'},{'mean','std'},'dataVars',{'mean_evidence','mean_choice'});
    data.Properties.VariableNames=strrep(data.Properties.VariableNames,'mean_','');
    data.Properties.VariableNames=strrep(data.Properties.VariableNames,'std_','sem_');
    
    %fit psychometric
    [fitresult,gof] = psychometricModel(data,'genModel','guessGaussian');
    
    %plot pychometric
    psychometricPlot(fitresult,data,'axesHandle',h(k),...
        'xLabel','Signal-to-noise (dB)','yLabel','Signal choice (%)','legend','off','binEdges',...
        binEdges,'dataMarkerSize',12,'oddMarker','p','YPercent','on',...
        'fitLineColor',[colors(kb,:) .7],'fitLineWidth',1,'dataColor',colors(kb,:),'dataFaceColor',colors(kb,:),...
        'dataEdgeColor',colors(kb,:));
    
    % axes cosmetics, make error bars one-sided
    a=findobj(h(k),'type','errorbar','color',colors(kb,:));
    if kb==1
        a.YPositiveDelta=zeros(size(a.YNegativeDelta));
    elseif kb==2
        a.YPositiveDelta=zeros(size(a.YNegativeDelta));
        a.YNegativeDelta=zeros(size(a.YNegativeDelta));
    elseif kb==3
        a.YNegativeDelta=zeros(size(a.YNegativeDelta));
    end
    a.LineWidth=1.2;
end

%figure cosmetics
h(k).Title.String='';
h(k).XTick=[-40,0,20];
h(k).XTickLabel={'No signal','0','20'};
h(k).YTick=[0 50 100];

% legend (manual, if automatic does not work nicely)
a=findall(h(k),'Marker','none');
[~,sortIdx]=sort([a(1).Color(2),a(2).Color(2),a(2).Color(3)]);
strings={'0.7','0.5','0.3'};
ypos=.95;
lineSpace=.13;
for n=1:length(sortIdx)
    properties=a(sortIdx(n));
    text(h(k),.1,ypos,strings{n},'FontSize',8,'Color',properties.Color,'Units','normalized');
    ypos=ypos-lineSpace;
end


%% statistical confidence and time investmets
% ask whether to load or simulate
promptMessage = sprintf('Need statistical confidence simulation for each subject.\nLoad existing simulation or perform new simulation?\n(If existing simulation is not found, new simulation is performed in any case.)'); 
button = questdlg(promptMessage, 'Load', 'Load', 'Simulate', 'Load');
if ~exist('.\confidenceSimulation','dir')
    mkdir('confidenceSimulation');
end

% settings
exampleSubject='K17';
subjects=unique(trialTab.subjectId);
blocks=[.3 .5 .7];
allConfidenceGof=table;
counter=0;

for s=1:length(subjects)
    subIdx=ismember(trialTab.subjectId,subjects(s));
    
    for b=1:length(blocks)
        counter=counter+1;
        confidencePath=fullfile('confidenceSimulation',sprintf('%s_proportion%d.mat',subjects{s},blocks(b)*100));
        if ~exist(confidencePath,'file')||strcmpi(button,'Simulate')
            %% print progress
            fprintf('Simulating statistical confidence %s proportion %d (%d/%d)...',subjects{s},blocks(b)*100,counter,length(subjects)*length(blocks));
            
            blockIdx=ismember(trialTab.blockBias,blocks(b));
            data=trialTab(subIdx&blockIdx,:);
            
            %% predict statistical confidence
            % infer "evidence level" for no signal trials
            data.oddtrial=data.evidence<-30;
            [fitresult,~] = psychometricModel(data,'genModel','gaussian');
            oddEvidence=norminv(nanmean(data.choice(data.oddtrial==1)),fitresult.m,fitresult.sigma);%set non-fitted noise trials to predicted evidence
            
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
            
            %% assess fit between statistical confidence and time investments
            [confidenceGof]=confidenceGoodnessOfFit(data,simDataBoot,'nPermutations',1000,'binNumbers',[3],'sampleOption','up');
            
            %% save
            save(confidencePath,'data','simDataBoot','confidenceGof','randomseed');
            
            %% print progress
            fprintf('Done\n');
            
        elseif strcmpi(button,'Load')
            % load goodness of fit only
            fprintf('Loading statistical confidence %s proportion %d (%d/%d)...',subjects{s},blocks(b)*100,counter,length(subjects)*length(blocks));
            load(confidencePath,'confidenceGof');
            fprintf('Done\n');
        end
        idx=strcmp(confidenceGof.signature,'vevaiometric')&strcmp(confidenceGof.confidenceName,'confidenceRaw');
        confidenceGof.subjectId=repmat(subjects(s),height(confidenceGof),1);
        confidenceGof.blockBias=repmat(blocks(b),height(confidenceGof),1);
        allConfidenceGof=[allConfidenceGof;confidenceGof(idx,:)];
        
        %% plot calibration example subject
        if ismember(subjects(s),exampleSubject)
            if strcmpi(button,'Load')
                % load full data
                load(confidencePath,'data','simDataBoot');
            end
            % prepare panel
            k=k+1;
            h(k)=nexttile;
            
            %plot
            binEdgesConfidence=prctile(data{:,'confidence'},[0:100/binNumber:100]);
            calibrationPlot(simDataBoot,'axesHandle',h(k),'binEdges',3:.1:20,'minDataPoints',0,'legend','off',...
                'fitColors',colors(b,:));
            calibrationPlot(data,'axesHandle',h(k),'binEdges',binEdgesConfidence,'plotSize',false,'minDataPoints',minDataPoints,...
                'YPercent','on','xlabel','Invested time (s)','ylabel','Accuracy (%)','legend','off','dataMarkerSize',10,'dataLineWidth',1.2,...
                'sampleOption','up','dataColors',colors(b,:),...
                'groupVariable','sessionId','errorMode','std','sampleOption','up','correctVarianceX',true,'correctVarianceY',false);
            
            %axes stuff
            h(k).XLim=[2 7.9];
            h(k).Title.String='';
            h(k).YTick=[50,75,100];
            
            %annotation
            ypos=.95;
            text(h(k),.1,ypos,sprintf('%2.1f',blocks(b)),'FontSize',8,'Color',colors(b,:),'Units','normalized');
            
            % sample size
            hdl{k}=addSampleSize(data,h(k),'southeast',2);
        end
    end
end
%% Fig 2D: confidence metrics
subjects=unique(trialTab.subjectId);
allConfidenceGof=table;%preallocate
allSlope=table;
for s=1:length(subjects)
    for b=[.3 .5 .7]
        if b==0.3
            resfile=fullfile('C:\Users\Katharina\Data\BpodData\',subjects{s},'block30_confidenceModel.mat');
        elseif b==0.5
            resfile=fullfile('C:\Users\Katharina\Data\BpodData\',subjects{s},'block50_confidenceModel.mat');
        elseif b==0.7
            resfile=fullfile('C:\Users\Katharina\Data\BpodData\',subjects{s},'block70_confidenceModel.mat');
        end
        load(resfile,'confidenceGof','data');        
        confidenceGof.subjectId=repmat(subjects(s),height(confidenceGof),1);
        confidenceGof.blockBias=repmat(b,height(confidenceGof),1);
        allConfidenceGof=[allConfidenceGof;confidenceGof];
        
    end
    
end

%% H - summary plot
sigIdx=strcmp(allConfidenceGof.signature,'vevaiometric')&strcmp(allConfidenceGof.confidenceName,'confidenceRaw');%&allConfidenceGof.binning==gofBins;
block30Idx=allConfidenceGof.blockBias==0.3;
block50Idx=allConfidenceGof.blockBias==0.5;
block70Idx=allConfidenceGof.blockBias==0.7;

fitTable=allConfidenceGof(sigIdx,{'PearsonR','subjectId','blockBias'});
fitTable.subject=cellfun(@(x) str2double(x(2:end)),fitTable.subjectId);
fitTable.modelFit=fitTable.PearsonR.^2;
[sumTable,resTable,strings]=groupMeanSem(fitTable,{'subject'},{'blockBias'},{'modelFit'},0);
fprintf(fid,'Model fit confidence: signal proportion %s\n',strings{1});

% prepare panel
k=k+1;
h(k)=nexttile;

% prepare data
ev=allConfidenceGof{sigIdx&block30Idx,'PearsonR'}.^2;
ev2=allConfidenceGof{sigIdx&block50Idx,'PearsonR'}.^2;
ev3=allConfidenceGof{sigIdx&block70Idx,'PearsonR'}.^2;

y=[ev;ev2;ev3];%[ev;sev;ev2;sev2];
x=[zeros(size(ev));ones(size(ev2));ones(size(ev3))+1];%[zeros(size(ev));ones(size(ev));zeros(size(ev))+2.5;ones(size(ev))+2.5];

% plot beeswarm
beeswarm(x,y,'Colormap',colors(1:3,:),'use_current_axes',0,'dot_size',.45,'MarkerFaceAlpha',1,'Marker','o','corral_style','none',...
    'sort_style','no_sort');

% axes cosmetics
h(k).XLim=[-.5 2.5];
h(k).XTick=[0 1 2];
h(k).YAxis.Label.String='Variance expl.  (R^2)';
h(k).XAxis.Label.String='Signal proportion';
h(k).XTickLabel={'0.3','0.5','0.7'};
h(k).YLim=[0 1];
h(k).Clipping='off';

%sigstar
astXCor={[0 1],[1 2],[0 2]};
astPval=[sumTable{sumTable.blockBias==0.3,("modelFit_pairwise_pval_0.5")},...
    sumTable{sumTable.blockBias==0.5,("modelFit_pairwise_pval_0.7")},...
    sumTable{sumTable.blockBias==0.7,("modelFit_pairwise_pval_0.3")}];
sigHdl=sigstar(astXCor([2 1 3]),astPval([2 1 3]),1);
formatSigHdl(sigHdl,h(k));

% add sample size
hdl{k}=addSampleSize(trialTab,h(k),'southeast');

% axes cosmetics again
h(k).YLim(2)=1.13;

%% figure finishing
%% letters
% put
toAnnotate=[1:8];
for a=1:length(toAnnotate)
    k=toAnnotate(a);
    an(a)=annotation('textbox','String',char('A'+a-1));
    an(a).Position=[h(k).OuterPosition(1) h(k).OuterPosition(2)+h(k).OuterPosition(4) 0 0];
    an(a).HorizontalAlignment='center';
    an(a).VerticalAlignment='middle';
    an(a).EdgeColor='none';
    an(a).Margin=0;
    an(a).FontWeight='bold';
    an(a).FontSize=10;
end

% re-position letters
x=[0.025 .25 .49 .725];
y=[.97 .72 .485 .235];
for a=1:length(an)
    k=toAnnotate(a);
    an(a).Position=[h(k).OuterPosition(1) h(k).OuterPosition(2)+h(k).OuterPosition(4) 0 0];
    [~,xIdx]=min(abs(x-an(a).Position(1)));
    [~,yIdx]=min(abs(y-an(a).Position(2)));
    an(a).Position=[x(xIdx) y(yIdx) 0 0];
end

%% axes limits
for k=['B']-64
    h(k).YLim=[4.13 4.83];
end
for k=['E','F','G']-64
    h(k).XLim=[2.4 7.8];
    h(k).XTick=[4 6];
    h(k).YLim=[45 100];
end
for k=['H']-64
    h(k).YLim(1)=-.08;
end

%% text alignment
k=['A']-64;
for p=1:2
    hdl{k}(p).Position(1)=.92;
    hdl{k}(p).FontWeight='bold';
end
k=['I']-64;
for p=1:2
    hdl{k}(p).Position(1)=.86;
    hdl{k}(p).FontWeight='bold';
end
for k=['E','F','G']-64
    for p=1:2
        hdl{k}(p).Position(1)=1;
    end
end

%% labels
% push y labels closer to ticks without losing out on space
ns=['E','D','H']-64;
rightShift=[.05,.03,.01];
for k=ns
    h(k).YLabel.Units='normalized';
    h(k).YLabel.Color=h(k).YAxis.Color;
    t(k)=text(h(k),h(k).YLabel.Position(1)+rightShift(ns==k),h(k).YLabel.Position(2),h(k).YLabel.String,'Rotation',90,...
        'FontSize',h(k).YLabel.FontSize,'Color',h(k).YLabel.Color,...
        'HorizontalAlignment',h(k).YLabel.HorizontalAlignment,...
        'VerticalAlignment',h(k).YLabel.VerticalAlignment,'Units','normalized');
    h(k).YLabel.Color='w';
end

% put label in two lines without messing up with layout
for k=['C']-64
    h(k).YLabel.Units='normalized';
    h(k).YLabel.String=strrep(h(k).YLabel.String,'High invested','');
    h(k).YLabel.Color='w';
    labelhdl{k}(1)=text(h(k),h(k).YLabel.Position(1)-.14,h(k).YLabel.Position(2),'High invested',...
        'Rotation',90,'Units','normalized','Color',h(k).YAxis.Color,...
        'VerticalAlignment',h(k).YLabel.VerticalAlignment,'HorizontalAlignment',h(k).YLabel.HorizontalAlignment,...
        'FontSize',h(k).YLabel.FontSize);
    labelhdl{k}(2)=text(h(k),h(k).YLabel.Position(1)+.04,h(k).YLabel.Position(2),'time FA rate (%)',...
        'Rotation',90,'Units','normalized','Color',h(k).YAxis.Color,...
        'VerticalAlignment',h(k).YLabel.VerticalAlignment,'HorizontalAlignment',h(k).YLabel.HorizontalAlignment,...
        'FontSize',h(k).YLabel.FontSize);
end

% push x labels closer to ticks without losing out on space
ns=[1:rows*cols-1];
for k=ns
    h(k).XLabel.Color='w';
    h(k).XLabel.Units='normalized';
    t(k)=text(h(k),0.5,-.22,h(k).XLabel.String,'FontSize',h(k).XLabel.FontSize,'FontWeight','Normal','HorizontalAlignment','center','VerticalAlignment','top','units','normalized','Color',h(k).XAxis.Color);
end

%% save figure and caption
set(figHdl,'Visible','on');
exportgraphics(figHdl,figureName,'Resolution',900);
fprintf('Saved plots to %s.\n',figureName)
close(figHdl);

fprintf('Saved statistics to %s.\n',captionName)
fclose(fid);

