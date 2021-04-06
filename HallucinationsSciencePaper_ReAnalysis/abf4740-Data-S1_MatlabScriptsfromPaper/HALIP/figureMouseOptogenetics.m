% This script plots behavioral data from mice (c.f. Figure 06 "Dopamine in
% tail of striatum is causal for hallucination-like perceptions ") from %
% "Striatal Dopamine Mediates Hallucination-Like Perception in Mice" by
% Schmack et al. 2020.
%
% KS, January 2021, Cold Spring Harbor schmack@cshl.edu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A - optogenetics effect on false alarm rate (group)
% B - optogenetics effect on false alarm confidence (group)
% C - optogenetics effect on high-confidence false alarm confidence (group)
% D - optogenetics effect on pscyhometric choice behavior (group)
% E-F - optogenetics effect on confidence calibration (example subject)
% G - optogenetics effect on fit between statistical confidence and time investments (group)
% H - haloperidol rescue of optogenetics effect on false alarm rate (group)
% I - haloperidol rescue of optogenetics effect on false alarm confidence (group)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT NOTE: Some of the analyses include stochastic simulations (C, E-G).
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
figureName='results\FigureMouseOptogenetics.pdf';
captionName='results\StatsMouseOptogenetics.txt';
if ~exist('results','dir')
    mkdir('results');
end
fid=fopen(captionName,'w');

% layout
colFactor=6;
rowFactor=1;
cols=4*colFactor;rows=3;%columns and rows in figure
wid_cm=13.6;%figure width in cm; Science either 12cm or 5.5cm
px2cm = 1/96 * 2.54;
figHdl=figure('Name','Optogenetics & HALIPs','NumberTitle','off','Position',[0 100 ceil(wid_cm/px2cm) ceil(wid_cm/px2cm)/(cols/colFactor)*rows],'Visible','on');
figHdl.Position(4)=402;
tiledlayout(rows,cols,'TileSpacing','compact','Padding','compact');
k=0;

% plot options
colors=getColor({'optoOff','optoOn'});
minDataPoints=0;
binNumber=5;

%% get & prepare data
load(dataName,'trialTab','sessionTab');
[trialTab,sessionTab]=selectExperiment(trialTab,sessionTab,'optogenetics');

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
[sumTable,resTable,strings]=groupMeanSem(trialTab,{'subjectId'},{'optogenetics'},{'falseAlarmRate','falseAlarmConfidence','highFalseAlarmRate','lowFalseAlarmRate'},0);

% print out statistics
fprintf(fid,'%%%%%%%%%%%%%%%%%%%%%%Optogenetics Effect on HALIPs%%%%%%%%%%%%%%%%%%%%%%\n');
fprintf(fid,'%d mice, %d sessions, %d trials\n',length(unique(trialTab.subjectId)),length(unique(trialTab.sessionId)),height(trialTab));
fprintf(fid,'ANOVA false alarm rate: main effect optogenetics %s\n',strings{1});
fprintf(fid,'ANOVA false alarm confidence: main effect optogenetics %s\n',strings{2});
fprintf(fid,'ANOVA false alarm (high): main effect optogenetics %s\n',strings{3});
fprintf(fid,'ANOVA false alarm (low): main effect optogenetics %s\n',strings{4});

% place holder
ph=nexttile([rowFactor colFactor]);
ph.Visible='off';

%% A/C false alarm rate/high-confidence false alarm rate
hlVars={'falseAlarmRate','highFalseAlarmRate'};
hlYLabel={'FA rate (%)',{'High invested time FA rate (%)'}};
subN=sumTable.GroupCount(1);

for hl=1:2
    hlVar=sprintf('%s',hlVars{hl});
    if hl==2 %make space for confidence
        k=k+1;
        h(k)=nexttile([rowFactor colFactor]);
        hold on;
    end
    k=k+1;
    h(k)=nexttile([rowFactor colFactor]);
    hold on;
    
    % plot bars
    for l=1:2
        x=sumTable{l,'optogenetics'};
        y=sumTable{l,['mean_',hlVar]};
        bar(h(k),x,y,'facecolor',colors(l,:),'barwidth',1);
    end
    
    %significance
    astXCor={[0 1 ]};
    astPval=sumTable{sumTable.optogenetics==0,(strcat(hlVar, "_pairwise_pval_1.0"))};
    sigHdl=sigstar(astXCor,astPval);
    formatSigHdl(sigHdl,h(k));
    
    % single lines (to get scaling)
    p1=plot(h(k),reshape([resTable.optogenetics],subN,2)',reshape([resTable{:,['corrected_nanmean_',hlVar]}],subN,2)','Color',getColor({'singleLine'}),'LineWidth',1,'Marker','none','LineStyle','-');
    for px=1:length(p1)
        p1(px).Color(4)=0.7;%set transparency
    end
    
    % axis cosmetics
    set(h(k),'xlim',[-.9 1.9],'xtick',[0 1],'xticklabel',{'Off','On'});
    h(k).XLabel.String='Laser';
    h(k).Clipping='off';
    h(k).YLabel.String=hlYLabel{hl};
    
    if hl==1
        hdl{k}=addSampleSize(trialTab,h(k),'southeast');
        for hd=1:length(hdl{k})
            hdl{k}(hd).Color=[.9 .9 .9];
        end
    end
end

%% B - false alarm confidence
% prepare panel
k=k-1;
axes(h(k));
hold on;

% plot single lines
p1=plot(h(k),reshape([resTable.optogenetics],subN,2)',reshape([resTable.corrected_nanmean_falseAlarmConfidence],subN,2)','Color',getColor({'singleLine'}),'LineWidth',1,'Marker','none','LineStyle','-');
for px=1:length(p1)
    p1(px).Color(4)=.7;%set transparency
end

% plot dots
for l=1:2
    x=sumTable{l,'optogenetics'};
    y=sumTable{l,'mean_falseAlarmConfidence'};
    s=zeros(size(y));
    eHdl=errorbar(h(k),x,y,s,'color',colors(l,:),'MarkerFaceColor',colors(l,:),'LineStyle',...
        'none','Marker','o','MarkerSize',5,'LineWidth',1);
end

%axis cosmetics
set(h(k),'xlim',[-.45 1.45],'xtick',[0 1],'xticklabel',{'Off','On'})
h(k).XLabel.String='Laser';
h(k).YLabel.String={'FA invested time (s)'};
h(k).Clipping='off';

%signficance
astXCor={[0 1 ]};
astPval=sumTable{sumTable.optogenetics==0,("falseAlarmConfidence_pairwise_pval_1.0")};
sigHdl=sigstar(astXCor,astPval);
formatSigHdl(sigHdl,h(k));


%% D - psychometric choice behavior
% prepare panel
k=k+2;
h(k)=nexttile([rowFactor colFactor]);
hold on;

% fit and plot psychometrics
binEdges=[-45,prctile(trialTab.evidence(trialTab.evidence>-15),[0:100/4:100])];
trialTab.bins=discretize(trialTab.evidence,binEdges);
optogenetics=[0 1];
for kb=1:length(optogenetics)
    
    %prepare data
    subIdx=trialTab.optogenetics==optogenetics(kb)&~isnan(trialTab.choice);
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
strings={'Laser Off','Laser On'};
ypos=1;
lineSpace=.13;
for n=2:-1:1
    text(h(k),.1,ypos,strings{n},'FontSize',8,'Color',colors(n,:),'Units','normalized');
    ypos=ypos-lineSpace;
end


%% G - statistical confidence and time investmets
% ask whether to load or simulate
promptMessage = sprintf('Need statistical confidence simulation for each subject.\nLoad existing simulation or perform new simulation?\n(If existing simulation is not found, new simulation is performed in any case.)');
button = questdlg(promptMessage, 'Load', 'Load', 'Simulate', 'Load');
if ~exist('.\confidenceSimulation','dir')
    mkdir('confidenceSimulation');
end

% set parameters
exampleSubject='K29';
subjects=unique(trialTab.subjectId);
optogenetics=[0 1];
strings={'Laser Off','Laser On'};
allConfidenceGof=table;
counter=0;
for s=1:length(subjects)
    subIdx=ismember(trialTab.subjectId,subjects(s));
    
    for opt=1:length(optogenetics)
        confidencePath=fullfile('confidenceSimulation',sprintf('%s_optogenetics%d.mat',subjects{s},optogenetics(opt)));
        counter=counter+1;
        if ~exist(confidencePath,'file')||strcmpi(button,'Simulate')
            %% print progress
            fprintf('Simulating statistical confidence %s laser %d (%d/%d)...',subjects{s},optogenetics(opt),counter,length(subjects)*length(optogenetics));
            
            %% select data
            optIdx=ismember(trialTab.optogenetics,optogenetics(opt));
            data=trialTab(subIdx&optIdx,:);
            
            %% predict statistical confidence
            % infer "evidence level" for no signal trials
            data.oddtrial=data.evidence<-30;
            [fitresult,~] = psychometricModel(data,'genModel','gaussian');
            oddEvidence=norminv(nanmean(data.choice(data.oddtrial==1)),fitresult.m,fitresult.sigma);%set non-fitted noise trials to predicted evidence
            
            % run model simulation
            randomseed=rng(randomSeedNumber);%for reproducibility, set seed random generator to a fixed value
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
            fprintf('Loading statistical confidence %s laser %d (%d/%d)...',subjects{s},optogenetics(opt),counter,length(subjects)*length(optogenetics));
            load(confidencePath,'confidenceGof');
            fprintf('Done\n');
        end
        idx=strcmp(confidenceGof.signature,'vevaiometric')&strcmp(confidenceGof.confidenceName,'confidenceRaw');
        confidenceGof.subjectId=repmat(subjects(s),height(confidenceGof),1);
        confidenceGof.optogenetics=repmat(optogenetics(opt),height(confidenceGof),1);
        allConfidenceGof=[allConfidenceGof;confidenceGof(idx,:)];
        
        %% E-F plot calibration (example subject(
        if ismember(subjects(s),exampleSubject)
            if strcmpi(button,'Load')
                load(confidencePath,'data','simDataBoot');
            end
            
            % prepare panel
            k=k+1;
            h(k)=nexttile([rowFactor colFactor]);
            
            % bin and plot
            binEdgesConfidence=prctile(data{:,'confidence'},[0:100/binNumber:100]);
            calibrationPlot(simDataBoot,'axesHandle',h(k),'binEdges',3:.1:20,'minDataPoints',0,'legend','off',...
                'fitColors',colors(opt,:));
            calibrationPlot(data,'axesHandle',h(k),'binEdges',binEdgesConfidence,'plotSize',false,'minDataPoints',minDataPoints,...
                'YPercent','on','xlabel','Invested time (s)','ylabel','Accuracy (%)','legend','off','dataMarkerSize',10,'dataLineWidth',1.2,...
                'sampleOption','up','dataColors',colors(opt,:),...
                'groupVariable','sessionId','errorMode','std','sampleOption','up','correctVarianceX',true,'correctVarianceY',false);
            
            %axes stuff
            h(k).XLim=[2 7];
            h(k).Title.String='';
            h(k).YTick=[50,75,100];
            
            % legend
            ypos=.95;
            text(h(k),.1,ypos,string,'FontSize',8,'Color',colors(opt,:),'Units','normalized');
            
            hdl{k}=addSampleSize(data,h(k),'southeast',2);
            for hd=1:length(hdl{k})
                hdl{k}(hd).Position(1)=1;
            end
        end
    end
end
%% G - fit statistical confidence and time investment under laser off and on
offIdx=allConfidenceGof.optogenetics==0;
onIdx=allConfidenceGof.optogenetics==1;
fitTable=allConfidenceGof(:,{'PearsonR','subjectId','optogenetics'});
fitTable.subject=cellfun(@(x) str2double(x(2:end)),fitTable.subjectId);
fitTable.modelFit=fitTable.PearsonR.^2;
[sumTable,~,strings]=groupMeanSem(fitTable,{'subject'},{'optogenetics'},{'modelFit'},0);
fprintf(fid,'Model fit confidence: optogenetics %s\n',strings{1});

% prepare plot
k=k+1;
h(k)=nexttile([rowFactor colFactor]);

% prepare data
ev=allConfidenceGof{offIdx,'PearsonR'}.^2;
ev2=allConfidenceGof{onIdx,'PearsonR'}.^2;

% beeswamr
y=[ev;ev2];
x=[zeros(size(ev));ones(size(ev2))];
beeswarm(x,y,'Colormap',colors,'use_current_axes',0,'dot_size',.45,'MarkerFaceAlpha',1,'Marker','o','corral_style','none',...
    'sort_style','no_sort');

%axis cosmetics
h(k).XTick=[0 1];
h(k).YLim=[0 1];
h(k).YAxis.Label.String='Variance expl. (R^2)';
h(k).XAxis.Label.String='Laser';
h(k).XTickLabel={'Off','On'};
h(k).Title.String='';
h(k).Clipping='off';

%signficance
astXCor={[0 1]};
astPval=[sumTable{sumTable.optogenetics==0,("modelFit_pairwise_pval_1.0")}];
sigHdl=sigstar(astXCor,astPval);
formatSigHdl(sigHdl,h(k));

%sample size
hdl{k}=addSampleSize(trialTab,h(k),'southeast',2);


%% H/I haloperidol rescue of optogenetics effect
%% get & prepare data
load(dataName,'trialTab','sessionTab');
[trialTab,thisSessionTab]=selectExperiment(trialTab,sessionTab,'optogeneticsHaloperidol');
trialTab.optohalo=trialTab.optogenetics;
trialTab.optohalo(trialTab.haloperidol==1)=trialTab.optohalo(trialTab.haloperidol==1)+2;

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

% group data
[sumTable,resTable,strings]=groupMeanSem(trialTab,{'subjectId'},{'optohalo'},{'falseAlarmRate','falseAlarmConfidence','highFalseAlarmRate','lowFalseAlarmRate'},0);

%% H optogenetics x haloperidol on false alarm rate
hlVars={'falseAlarmRate'};
hlYLabel={'FA rate (%)'};
colors=[colors;colors];
for hl=1
    hlVar=sprintf('%s',hlVars{hl});
    
    % prepare panel
    k=k+1;
    ph=nexttile([1 2]);
    ph.Visible='off';
    h(k)=nexttile([1 cols/2-2]);
    hold on;
    
    % plot bars
    xvals=[0,1,2.35,3.35];
    for l=1:4
        x=xvals(l);
        y=sumTable{l,['mean_',hlVar]};
        b(l)=bar(h(k),x,y,'facecolor',colors(l,:),'barwidth',1);
    end
    
    % plot single bars
    subjects=unique(resTable.subjectId);
    for s=1:length(subjects)
        idx=ismember(resTable.subjectId,subjects(s));
        y=resTable{idx,['corrected_nanmean_',hlVar]};
        y=[y(1:2);nan;y(3:4)];
        x=[0,1,1.75,2.35,3.35];
        p1=plot(h(k),x,y,'Color',getColor({'singleLine'}),'LineWidth',1,'Marker','none');
    end
    
    % axis cosmetics
    set(h(k),'xlim',[-.8 4.05],'xtick',[.5 2.85],'xticklabel',{'Vehicle','Haloperidol'})
    h(k).XLabel.String='Treatment';
    h(k).YLabel.String=hlYLabel{hl};
    h(k).YLabel.Clipping='off';
    h(k).YLim(1)=0;
    
    % sampleSize
    if hl==1
        hdl{k}=addSampleSize(trialTab,h(k),'southwest');
    end
    
    % legend (manual, if automatic does not work nicely)
    legHdl{k}(1)=text(h(k),.95,.76,'Laser off','FontSize',8,'Color',colors(1,:),'Units','normalized','HorizontalAlignment','right');
    legHdl{k}(2)=text(h(k),.95,.62,'Laser on','FontSize',8,'Color',colors(2,:),'Units','normalized','HorizontalAlignment','right');
end
%% I optogenetics x haloperidol on false alarm confidence
% prepare panel
k=k+1;
h(k)=nexttile([1 cols/2-2]);
hold on;

% plot single bars
subjects=unique(resTable.subjectId);
for s=1:length(subjects)
    idx=ismember(resTable.subjectId,subjects(s));
    y=resTable.corrected_nanmean_falseAlarmConfidence(idx);
    y=[y(1:2);nan;y(3:4)];
    x=[0,1,1.75,2.35,3.35];
    p1=plot(h(k),x,y,'Color',getColor({'singleLine'}),'LineWidth',1,'Marker','none');
end

% plot group means
xvals=[0,1.1,2.25,3.35];
for l=1:4
    x=xvals(l);
    y=sumTable{l,'mean_falseAlarmConfidence'};
    plot(h(k),x,y,'color',colors(l,:),'MarkerFaceColor',colors(l,:),'LineStyle',...
        'none','Marker','o','MarkerSize',5,'LineWidth',1.5);
end

% axis cosmetics
set(h(k),'xlim',[-.9 4.4],'xtick',[.55 2.8],'xticklabel',{'Vehicle','Haloperidol'})
h(k).XLabel.String='Treatment';
h(k).YLabel.String='FA Invested Time (s)';
h(k).Clipping='off';

% legend (manual)
legHdl{k}(1)=text(h(k),.95,.76,'Laser off','FontSize',8,'Color',colors(1,:),'Units','normalized','HorizontalAlignment','right');
legHdl{k}(2)=text(h(k),.95,.62,'Laser on','FontSize',8,'Color',colors(2,:),'Units','normalized','HorizontalAlignment','right');


%% group statistics (2x2 repeated measures ANOVA)
% print summary
fprintf(fid,'%%%%%%%%%%%%%%%%%%%%%%Haloperidol Rescue of Optogenetics Effect on HALIPs%%%%%%%%%%%%%%%%%%%%%%\n');
fprintf(fid,'%d mice, %d sessions, %d trials (%d-%d)\n',length(unique(trialTab.subjectId)),...
    height(sessionTab),...
    height(trialTab),sum(trialTab.haloperidol==0),sum(trialTab.haloperidol==1));
nsub=length(unique(trialTab.subjectId));

% run ANOVA
variables={'falseAlarmRate','falseAlarmConfidence'};
names={'false alarm rate','false alarm confidence'};
for var=1:length(variables)
    sumTable=grpstats(trialTab,{'subjectId','optogenetics','haloperidol'},'mean','DataVars',variables{var});
    repeatedTable=cell2table(num2cell(reshape(sumTable{:,{['mean_' variables{var}]}},[4,nsub])'),'VariableNames',{'o0_h0','o0_h1','o1_h0','o1_h1'});
    within = table(categorical([1 1 2 2])',categorical([1 2 1 2])','VariableNames',{'optogenetics' 'haloperidol'}); % within model
    rm = fitrm(repeatedTable,'o0_h0-o1_h1~1','WithinDesign',within,'WithinModel','optogenetics:haloperidol');
    % interaction effect
    [ranovatbl] = ranova(rm,'WithinModel','optogenetics:haloperidol');
    fprintf(fid,'ANOVA ');
    fprintf(fid,'%s\n',names{var});
    idx=find(strcmp(ranovatbl.Properties.RowNames,'(Intercept):optogenetics:haloperidol'));
    fprintf(fid,'optogenetics x haloperidol: F(%d,%d)=%2.2f, p=%2.4f\n',ranovatbl.DF(idx),...
        ranovatbl.DF(idx+1),ranovatbl.F(idx),ranovatbl.pValue(idx));
    interactionP(var)=ranovatbl.pValue(idx);% store for plot
    
    % post-hoc
    mtab=multcompare(rm,'optogenetics','By','haloperidol');
    fprintf(fid,'post-hoc comparisons optogenetics effect without haloperidol: p=%2.4f\n',mtab.pValue(1));
    fprintf(fid,'post-hoc comparisons optogenetics effect with haloperidol: p=%2.4f\n',1-mtab.pValue(3));
end

%% figure finishing
%% letters
% put
toAnnotate=[1:9];
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

% manual repositioning (no code is perfect)
x=[0.015 .1 .25 .5 .75];
y=[.97 .65 .32];
for a=1:length(an)
    k=toAnnotate(a);
    an(a).Position=[h(k).OuterPosition(1) h(k).OuterPosition(2)+h(k).OuterPosition(4) 0 0];
    [~,xIdx]=min(abs(x-an(a).Position(1)));
    [~,yIdx]=min(abs(y-an(a).Position(2)));
    an(a).Position=[x(xIdx) y(yIdx) 0 0];
end

%% limits
for k=['A','C']-64
    h(k).XLim=[-.85 1.85];
    h(k).YLim(2)=range(h(k).YLim*.9);
end

k='B'-64;
h(k).XLim=[-.45 1.45];
h(k).YLim=[ 3.9 5];
for k=['G']-64
    h(k).YLim(1)=-.08;
end

k='H'-64;
h(k).YLim=[0 25];

k='I'-64;
h(k).Clipping='off';
h(k).YLim=[3.5 7.4];
h(k).YLabel.Units='normalized';
h(k).YLabel.Position(2)=1.02;
h(k).YLabel.HorizontalAlignment='right';

% text alignment
for k=['B']-64
    for hd=1:length(hdl{k})
        hdl{k}(hd).Position(1)=.9;
        hdl{k}(hd).FontWeight='bold';
        hdl{k}(hd).Color=[.9 .9 .9];
        hdl{k}(hd).Position(1)=.84;
    end
    hdl{k}(1).Position(2)=.23;
end

for k=['E']-64
    for hd=1:length(hdl{k})
        hdl{k}(hd).Position(1)=1.1;
    end
end

for k=['F','G']-64
    for hd=1:length(hdl{k})
        hdl{k}(hd).Position(1)=1;
    end
end

k='H'-64;
for hd=1:length(hdl{k})
    hdl{k}(hd).Units='normalized';
    hdl{k}(hd).Position(1)=.2;
    hdl{k}(hd).FontWeight='bold';
    hdl{k}(hd).HorizontalAlignment='right';
    hdl{k}(hd).Color=[.9 .9 .9];
    hdl{k}(hd).Position(1)=.84;
end
hdl{k}(1).Position(2)=.23;
hdl{k}(1).Position(1)=.455;
hdl{k}(2).Position(1)=.455;

%% sigifincance interaction
n=0;
for k=['H','I']-64
    h(k).Clipping='off';
    n=n+1;
    pstring=getPstring(interactionP(n));
    h(k).Clipping='off';
    x=[0,1,2.35,3.35];
    space=.3;
    xcor=[x(1) x(2) nan mean(x(1:2)) mean(x(1:2)),...
        mean(x(2:3))-space nan nan nan mean(x(2:3))+space,...
        mean(x(3:4)) mean(x(3:4)) nan x(3) x(4)];
    y=[min(h(k).YLim)+.9*diff(h(k).YLim) min(h(k).YLim)+1.05*diff(h(k).YLim)] ;
    ycor=[repmat(y(1),1,4),...
        repmat(y(2),1,7),...
        repmat(y(1),1,4)];
    sigHdl(1)=plot(h(k),xcor,ycor,'Color','k','LineWidth',1);
    xt=mean(x(2:3));
    yt=y(2)-.02*range(h(k).YLim);
    sigHdl(2)=text(h(k),xt,yt,pstring,'HorizontalAlignment','center');
end

%% labels
% single stuff
for k=['B']-64
    h(k).YLabel.Units='normalized';
    h(k).YLabel.Position(2)=0.98;
    h(k).YLabel.HorizontalAlignment='right';
end

% split up  long label in two lines without messing with tiledlayout
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

% y labels (push y labels closer to ticks without losing out on space)
ns=['A','B','E','F']-64;
rightShift=[-.02,.02,.05,.05,.03];
for k=ns
    try delete(t(k)); end
    h(k).YLabel.Color=h(k).YAxis.Color;
    h(k).YLabel.Units='normalized';
    h(k).YLabel.Color=h(k).YAxis.Color;
    t(k)=text(h(k),h(k).YLabel.Position(1)+rightShift(ns==k),h(k).YLabel.Position(2),h(k).YLabel.String,'Rotation',90,...
        'FontSize',h(k).YLabel.FontSize,'Color',h(k).YLabel.Color,...
        'HorizontalAlignment',h(k).YLabel.HorizontalAlignment,...
        'VerticalAlignment',h(k).YLabel.VerticalAlignment,'Units','normalized');
    h(k).YLabel.Color='w';
end

% push x labels closer to ticks without losing out on space
for k=[1:9]
    h(k).XLabel.Color='w';
    h(k).XLabel.Units='normalized';
    t(k)=text(h(k),0.5,-.2,h(k).XLabel.String,'FontSize',h(k).XLabel.FontSize,'FontWeight','Normal','HorizontalAlignment','center','VerticalAlignment','top','units','normalized','Color',h(k).XAxis.Color);
end

%% save figure and caption
set(figHdl,'Visible','on');
exportgraphics(figHdl,figureName,'Resolution',900);
fprintf('Saved plots to %s.\n',figureName)
close(figHdl);

fprintf('Saved statistics to %s.\n',captionName)
fclose(fid);

%% helper function
function pstring=getPstring(p)
if p<0.001
    pstring='***';
elseif p<0.01&&p>0.001
    pstring='**';
elseif p<0.05&&p>0.01
    pstring='*';
elseif p<0.1&&p>0.05
    pstring='(*)';
elseif p>0.1
    pstring='n.s.';
end
end