function [RHO, r_sqr, h] = cv_match_analysis(fcv_data, params, TTLs)
%CV_MATCH_ANALYSIS 
%   Function to perform CV matching (correlation, pearson by default)between 
%   FCV data and a supplied set of model CVs. %Provides the correlation coefficient 
%   and r² value between templates and data CV at each time point
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Inputs - 
%           fcv_data - variable containing data CVs such that [r,c] rows = CV scan point, 
%           params - data frame containing parameters [see below]
%           TTLs - 
% Outputs - 
%           RHO - 
%           r_sqr - 
%           h - 
%
% params data frame - 
%         .cv_match_template = 'C:\Users\mpanagi\Documents\GitHub\fcv_data_processing\chemoset\cvmatrix2.txt';
%         .shiftpeak = 1;           %%if .shiftpeak = 1, allow cv match to shift the peak of the data CV to match the template cv
%         .plotfig = 1;            %%if .plotfig = 1, plot tarheel style data for example
%         .colormap_type = 'fcv';
%         .scan_number = 140;
%         .point_number = 170;
%         .bg = 95;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%check inputs
if nargin < 1; error('Need FCV data'); end;
if nargin < 2; params = []; end;
if nargin < 3; TTLs = []; end;

%check params - apply defaults for missing values
if ~isfield(params,'cv_match_template') || isempty(params.cv_match_template)
    error('Please provide CV match template filename')
end
if ~isfield(params,'point_number') || isempty(params.point_number)
     params.point_number = 150;
end
if ~isfield(params,'scan_number') || isempty(params.scan_number)
     params.scan_number = 20;
end
if ~isfield(params,'shiftpeak') || isempty(params.shiftpeak)
     params.shiftpeak = 0;
end
if ~isfield(params,'plotfig') || isempty(params.plotfig)
     params.plotfig = 0;
end
if ~isfield(params,'colormap_type') || isempty(params.colormap_type)
     params.colormap_type = 'jet';
end
if ~isfield(params,'bg') || isempty(params.bg)
     params.bg = [];
end

%% load model cvs from filepath -> cv_match
try
    cv_match = load(params.cv_match_template);
catch
    error('Failed to load cv match template, please check filename')
end

fcv_IT = fcv_data(params.point_number,:);
fcv_CV = fcv_data(:,params.scan_number);
ts = [0:0.1:length(fcv_IT)/10-0.1];

%% if .plotfig = 1, plot tarheel style data for example
if params.plotfig
    [h] = visualise_fcv_data(fcv_data, ts, params, TTLs, cv_match, []);
end

%% if .shiftpeak = 1, allow cv match to shift the peak of the data CV to match the template cv
%This is where hardcoding needs to be fixed
shifted_cv = fcv_CV;
if params.shiftpeak
    [~, index] = max(cv_match);
    avg_peak = round(mean(index(1:7)));
    %hardcoded number of templates
    %add zeros to start or end of raw data
    %HARDCODED PEAK VALS
    [value, peak] = max(shifted_cv(148:178));
    shift_val = abs(peak+147-avg_peak);
    
    if peak>avg_peak && params.shiftpeak
        if params.plotfig
            subplot(2,3,3);
            hold on
            plot(shifted_cv(1:shift_val),'color',[0.6350    0.0780    0.1840])
            subplot(2,3,4);
            hold on
            plot(cv_match(:,length(shifted_cv):length(shifted_cv)+shift_val,'color',[0.6350    0.0780    0.1840]))
        end
        shifted_cv(1:shift_val) = [];        
        %add nans to the end
        %shifted_cv(length(shifted_cv):length(shifted_cv)+shift_val)=nan;
        
        %or remove end of cv match
        cv_match(:,length(shifted_cv):length(shifted_cv)+shift_val)=[];
        
    elseif peak<avg_peak && params.shiftpeak
        
        start_index = length(shifted_cv)-shift_val+1;
        end_index = length(shifted_cv);
        if params.plotfig
            subplot(2,3,3);
            hold on
            plot([start_index:end_index],shifted_cv(start_index:end_index),'color',[0.6350    0.0780    0.1840])
            subplot(2,3,4);
            hold on
            plot(cv_match(1:shift_val,:),'color',[0.6350    0.0780    0.1840])
        end
        shifted_cv(start_index:end_index)=[];  
        %add nans to start
        %shifted_cv = [shifted_cv;nan(shift_val,1)];
        
        %or remove start of cv match
        cv_match(1:shift_val,:) = [];
    end
end



end

