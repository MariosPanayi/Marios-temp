% This script plots behavioral data from mice (c.f. Figure 02I-O
% "Hallucination-like perceptions are increased by hallucination-related
% manipulations in mice") from % "Striatal Dopamine Mediates
% Hallucination-Like Perception in Mice" by Schmack et al. 2020.
%
% KS, January 2021, Cold Spring Harbor schmack@cshl.edu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A - ketamine effect on false alarm rate (group)
%% B - ketamine effect on false alarm confidence (group)
%% C - ketamine effect on high-confidence false alarm confidence (group)
%% D - ketamine effect on psychometric choice behavior (group)
%% E-F - ketamine effect on confidence calibration (example subject)
%% G - ketamine effect on fit between statistical confidence and time investments (group)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT NOTE: Some of the analyses include stochastic simulations (C,
% E-G). Therefore, results can vary slightly from run to run. For exactly
% repeatable results, set the random generator (e.g. 'randomSeedNumber=1')
% to a fixed value.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear;
randomSeedNumber=now;

%% prelude
% initialize figure and caption
set(0,'defaultAxesFontName','Calibri','defaultTextFontName','Calibri','defaultFigureColor','w','DefaultLegendAutoUpdate','off','defaultAxesTickDir','out','defaultAxesTickDirMode', 'manual','defaultAxesTickLength',[.02 .02]);%set defaults
dataName='mouseData.mat';
figureName='results\FigureMouseKetamine.pdf';
captionName='results\StatsMouseKetamine.txt';
if ~exist('results','dir')
    mkdir('results');
end
fid=fopen(captionName,'w');

% layout
cols=4;rows=2;%columns and rows in figure
wid_cm=13;%figure width in cm; Science either 12cm or 5.5cm
px2cm = 1/96 * 2.54;
figHdl=figure('Name','Ketamine & HALIPs','NumberTitle','off','Position',[0 100 ceil(wid_cm/px2cm) ceil(wid_cm/px2cm)/cols*rows],'Visible','on');
tiledlayout(rows,cols,'TileSpacing','compact','Padding','compact');k=0;

% plot options
colors=getColor({'ketamineLow','ketamineHigh'});
minDataPoints=0;
binNumber=5;

%% get & prepare data
load(dataName,'trialTab','sessionTab');
[trialTab,thisSessionTab]=selectExperiment(trialTab,sessionTab,'ketamine');

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
[sumTable,resTable,strings]=groupMeanSem(trialTab,{'subjectId'},{'ketamine'},{'falseAlarmRate','falseAlarmConfidence','highFalseAlarmRate','lowFalseAlarmRate'},0);

% print out statistics
fprintf(fid,'%%%%%%%%%%%%%%%%%%%%%%Ketamine Effect on HALIPs%%%%%%%%%%%%%%%%%%%%%%\n');
fprintf(fid,'%d mice, %d sessions, %d trials\n',length(unique(trialTab.subjectId)),length(unique(trialTab.sessionId)),height(trialTab));
fprintf(fid,'ANOVA false alarms: main effect ketamine %s\n',strings{1});
fprintf(fid,'ANOVA false alarm confidence: main effect  ketamine %s\n',strings{2});
fprintf(fid,'ANOVA false alarm (high): main effect  ketamine %s\n',strings{3});
fprintf(fid,'ANOVA false alarm (low): main effect  ketamine %s\n',strings{4});

%% A/C false alarm rate/high-confidence false alarm rate
hlVars={'falseAlarmRate','highFalseAlarmRate'};
hlYLabel={'FA rate (%)',{'High invested time FA rate (%)'}};
subN=sumTable.GroupCount(1);

for hl=1:2
    hlVar=sprintf('%s',hlVars{hl});
    if hl==2 %make space for confidence
        k=k+1;
        h(k)=nexttile;
        hold on;
    end
    k=k+1;
    h(k)=nexttile;
    hold on;
    
    % plot bars
    for l=1:2
        x=sumTable{l,'ketamine'};
        y=sumTable{l,['mean_',hlVar]};
        bar(h(k),x,y,'facecolor',colors(l,:),'barwidth',1);
    end
    
    %significance
    astXCor={[0 1 ]};
    astPval=sumTable{sumTable.ketamine==0,(strcat(hlVar, "_pairwise_pval_1.0"))};
    sigHdl=sigstar(astXCor,astPval);
    formatSigHdl(sigHdl,h(k));
    
    % single lines (to get scaling)
    p1=plot(h(k),reshape([resTable.ketamine],subN,2)',reshape([resTable{:,['corrected_nanmean_',hlVar]}],subN,2)','Color',getColor({'singleLine'}),'LineWidth',1,'Marker','none','LineStyle','-');
    for px=1:length(p1)
        p1(px).Color(4)=0.7;%set transparency
    end
    
    
    % axis cosmetics
    set(h(k),'xlim',[-.9 1.9],'xtick',[0 1],'xticklabel',{'Veh','Ket'});
    h(k).XLabel.String='Drug condition';
    h(k).Clipping='off';
    h(k).YLabel.String=hlYLabel{hl};
    
    if hl==1
        hdl{k}=addSampleSize(trialTab,h(k),'southeast');
        for hd=1:length(hdl{k})
            hdl{k}(hd).Color=[.9 .9 .9];
        end
    end
end

%% B - FA confidence
k=k-1;
axes(h(k));
hold on;

% plot single lines
p1=plot(h(k),reshape([resTable.ketamine],subN,2)',reshape([resTable.corrected_nanmean_falseAlarmConfidence],subN,2)','Color',getColor({'singleLine'}),'LineWidth',1,'Marker','none','LineStyle','-');
for px=1:length(p1)
    p1(px).Color(4)=.7;%set transparency
end

% plot dots
for l=1:2
    x=sumTable{l,'ketamine'};
    y=sumTable{l,'mean_falseAlarmConfidence'};
    s=zeros(size(y));
    eHdl=errorbar(h(k),x,y,s,'color',colors(l,:),'MarkerFaceColor',colors(l,:),'LineStyle',...
        'none','Marker','o','MarkerSize',5,'LineWidth',1);
end

%axis cosmetics
set(h(k),'xlim',[-.45 1.45],'xtick',[0 1],'xticklabel',{'Veh','Ket'})
h(k).XLabel.String='Drug condition';
h(k).YLabel.String={'FA invested time (s)'};
h(k).Clipping='off';

%signficance
astXCor={[0 1 ]};
astPval=sumTable{sumTable.ketamine==0,("falseAlarmConfidence_pairwise_pval_1.0")};
sigHdl=sigstar(astXCor,astPval);
formatSigHdl(sigHdl,h(k));


%% D - psychometric choice behavior
k=k+2;
h(k)=nexttile;
hold on;

% fit and plot psychometrics
binEdges=[-45,prctile(trialTab.evidence(trialTab.evidence>-15),[0:100/4:100])];
trialTab.bins=discretize(trialTab.evidence,binEdges);
ketamine=[0 1];
for kb=1:length(ketamine)
    subIdx=trialTab.ketamine==ketamine(kb)&~isnan(trialTab.choice);
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
    a=findobj(h(k),'type','errorbar','color',colors(kb,:));
    if kb==1
        a.YPositiveDelta=zeros(size(a.YNegativeDelta));
    elseif kb==2
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
strings={'Vehicle','Ketamine'};
ypos=1;
lineSpace=.13;
for n=2:-1:1
    text(h(k),.1,ypos,strings{n},'FontSize',8,'Color',colors(n,:),'Units','normalized');
    ypos=ypos-lineSpace;
end
ph=nexttile;
ph.Visible='off';


%% E-G statistical confidence and time investmets
% ask whether to load or simulate
promptMessage = sprintf('Need statistical confidence simulation for each subject.\nLoad existing simulation or perform new simulation?\n(If existing simulation is not found, new simulation is performed in any case.)'); 
button = questdlg(promptMessage, 'Load', 'Load', 'Simulate', 'Load');
if ~exist('.\confidenceSimulation','dir')
    mkdir('confidenceSimulation');
end

% set parameters
exampleSubject='K18';
subjects=unique(trialTab.subjectId);
ketamine=[0 1];
strings={'Vehicle','Ketamine'};
allConfidenceGof=table;
counter=0;
for s=1:length(subjects)
    subIdx=ismember(trialTab.subjectId,subjects(s));
    
    for ket=1:length(ketamine)
        counter=counter+1;
        confidencePath=fullfile('confidenceSimulation',sprintf('%s_ketamine%d.mat',subjects{s},ketamine(ket)));
        if ~exist(confidencePath,'file')||strcmpi(button,'Simulate')
            %% print progress
            fprintf('Simulating statistical confidence %s ketamine %d (%d/%d)...',subjects{s},ketamine(ket),counter,length(subjects)*length(ketamine));
            
            %% select data
            ketIdx=ismember(trialTab.ketamine,ketamine(ket));
            data=trialTab(subIdx&ketIdx,:);
            
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
            fprintf('Loading statistical confidence %s ketamine %d (%d/%d)...',subjects{s},ketamine(ket),counter,length(subjects)*length(ketamine));
            load(confidencePath,'confidenceGof');
            fprintf('Done\n');
        end
        idx=strcmp(confidenceGof.signature,'vevaiometric')&strcmp(confidenceGof.confidenceName,'confidenceRaw');
        confidenceGof.subjectId=repmat(subjects(s),height(confidenceGof),1);
        confidenceGof.ketamine=repmat(ketamine(ket),height(confidenceGof),1);
        allConfidenceGof=[allConfidenceGof;confidenceGof(idx,:)];
        
        %% E-F plot calibration (example subject)
        if ismember(subjects(s),exampleSubject)
            if strcmpi(button,'Load')
                load(confidencePath,'data','simDataBoot');
            end
            % prepare subpanel
            k=k+1;
            h(k)=nexttile;
            
            % bin and plot
            binEdgesConfidence=prctile(data{:,'confidence'},[0:100/binNumber:100]);
            calibrationPlot(simDataBoot,'axesHandle',h(k),'binEdges',3:.1:20,'minDataPoints',0,'legend','off',...
                'fitColors',colors(ket,:));
            calibrationPlot(data,'axesHandle',h(k),'binEdges',binEdgesConfidence,'plotSize',false,'minDataPoints',minDataPoints,...
                'YPercent','on','xlabel','Invested time (s)','ylabel','Accuracy (%)','legend','off','dataMarkerSize',10,'dataLineWidth',1.2,...
                'sampleOption','up','dataColors',colors(ket,:),...
                'groupVariable','sessionId','errorMode','std','sampleOption','up','correctVarianceX',true,'correctVarianceY',false);
            
            %axes cosmetics
            h(k).XLim=[2 7.9];
            h(k).Title.String='';
            h(k).YTick=[50,75,100];
            
            % legend (manual, because automatic looks horrible)
            ypos=1.01;
            legHdl{k}=text(h(k),.05,ypos,strings{ket},'FontSize',8,'Color',colors(ket,:),'Units','normalized');
            
            % add sample size
            hdl{k}=addSampleSize(data,h(k),'southeast',2);
            for hd=1:length(hdl{k})
                hdl{k}(hd).Position(1)=1;
            end
        end
    end
end

%% G fit of statistical confidence under ketamine and vehicle
plaIdx=allConfidenceGof.ketamine==0;
ketIdx=allConfidenceGof.ketamine==1;
fitTable=allConfidenceGof(:,{'PearsonR','subjectId','ketamine'});
fitTable.subject=cellfun(@(x) str2double(x(2:end)),fitTable.subjectId);
fitTable.modelFit=fitTable.PearsonR.^2;
[sumTable,~,strings]=groupMeanSem(fitTable,{'subject'},{'ketamine'},{'modelFit'},0);
fprintf(fid,'Model fit confidence: ketamine %s\n',strings{1});

% prepare plot
k=k+1;
h(k)=nexttile;

% prepare data
ev=allConfidenceGof{plaIdx,'PearsonR'}.^2;
ev2=allConfidenceGof{ketIdx,'PearsonR'}.^2;

% beeswarm
y=[ev;ev2];
x=[zeros(size(ev));ones(size(ev2))];
beeswarm(x,y,'Colormap',colors,'use_current_axes',0,'dot_size',.45,'MarkerFaceAlpha',1,'Marker','o','corral_style','none',...
    'sort_style','no_sort');

%axis cosmetics
h(k).XTick=[0 1];
h(k).YLim=[0 1];
h(k).YAxis.Label.String='Variance expl. (R^2)';
h(k).XAxis.Label.String='Drug condition';
h(k).XTickLabel={'Veh','Ket'};
h(k).Title.String='';
h(k).Clipping='off';

%signficance
astXCor={[0 1]};
astPval=[sumTable{sumTable.ketamine==0,("modelFit_pairwise_pval_1.0")}];
sigHdl=sigstar(astXCor,astPval);
formatSigHdl(sigHdl,h(k));

%sample size
hdl{k}=addSampleSize(trialTab,h(k),'southeast',2);


%% figure finishing
% put letters
toAnnotate=[1:7];
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
x=[0.025 .24 .49 .71];
y=[.97 .72 .485 .235];
for a=1:length(an)
    k=toAnnotate(a);
    an(a).Position=[h(k).OuterPosition(1) h(k).OuterPosition(2)+h(k).OuterPosition(4) 0 0];
    [~,xIdx]=min(abs(x-an(a).Position(1)));
    [~,yIdx]=min(abs(y-an(a).Position(2)));
    an(a).Position=[x(xIdx) y(yIdx) 0 0];
end


% labels
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

% retouch axis limits
for k=['A','B','C']-64
    h(k).XLim=[-.75 1.75];
end

for k=['E','F']-64
    h(k).XLim=[2.4 7.8];
    h(k).XTick=[4 6];
    h(k).YLim=[45 108];
end

%align text
k=['A']-64;
for p=1:2
    hdl{k}(p).Position(1)=.86;
    hdl{k}(p).FontWeight='bold';
end
for k=['E','F']-64
    legHdl{k}.Position(1)=0.05;
    legHdl{k}.Position(2)=0.95;
end

% push y labels closer to ticks without losing out on space
ns=['D']-64;
rightShift=[.03];
for k=ns
    h(k).YLabel.Units='normalized';
    h(k).YLabel.Color=h(k).YAxis.Color;
    t(k)=text(h(k),h(k).YLabel.Position(1)+rightShift(ns==k),h(k).YLabel.Position(2),h(k).YLabel.String,'Rotation',90,...
        'FontSize',h(k).YLabel.FontSize,'Color',h(k).YLabel.Color,...
        'HorizontalAlignment',h(k).YLabel.HorizontalAlignment,...
        'VerticalAlignment',h(k).YLabel.VerticalAlignment,'Units','normalized');
    h(k).YLabel.Color='w';
end


%% push x labels closer to ticks without losing out on space
ns=[1:rows*cols-1];
for k=ns
    h(k).XLabel.Color='w';
    h(k).XLabel.Units='normalized';
    t(k)=text(h(k),0.5,-.22,h(k).XLabel.String,'FontSize',h(k).XLabel.FontSize,'FontWeight','Normal','HorizontalAlignment','center','VerticalAlignment','top','units','normalized','Color',h(k).XAxis.Color);
end

%% 2- save figure and caption
set(figHdl,'Visible','on');
exportgraphics(figHdl,figureName,'Resolution',900);
fprintf('Saved plots to %s.\n',figureName)
close(figHdl);

fprintf('Saved statistics to %s.\n',captionName)
fclose(fid);

