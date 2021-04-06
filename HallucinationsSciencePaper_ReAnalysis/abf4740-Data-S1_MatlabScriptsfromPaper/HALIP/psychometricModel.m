function [fitresult,gof,ft] = PsychometricModel(data,varargin)
%% fits a psychometric curve
% inputs (required)
% - data: a table containing a vector evidence and a vector with choices
% inputs (optional)
% 'genModel': specifies the generative model that is used for fitting
%             'gaussian' (default) - 2-parameter Gaussian cdf (mu, sigma)
%             'gaussianZerobias' - 1-parameter Gaussian cdf (sigma)
%             'guessLapseGaussian' - 4-parameter Gaussian cdf (mu, sigma)
%             with assymmetric lapse rates (lamda, gamma)
%             'symmetricLapseGaussian' - 3-parameter Gaussian cdf (mu,
%             sigma) with symmetric lapse rates (lamda) 'guessGaussian' -
%             3-parameter Gaussian cdf (mu, sigma) with guess rate only
%             (gamma). This is commonly used for detection tasks with a
%             baseline guess rate.
% outputs
% fitresult - Matlab fit object with fitted parameters etc.
% gof       - goodness of fit
% ft        - formula used for fit

%% parse inputs
p=inputParser;
validTable = @(x) istable(x) & sum(ismember(x.Properties.VariableNames,{'evidence','choice'}))==2;
p.addRequired('data', validTable);
p.addParameter('genModel','gaussian',@(x) any(validatestring(x,{'gaussian','gaussianZerobias','guessLapseGaussian','guessGaussian','symmetricLapseGaussian'})));
p.parse(data,varargin{:})

%% prepare data
x=data.evidence;
y=data.choice;

% exclude oddtrials (this can be used to fit any model to a subset of trials)
if ismember('oddtrial', data.Properties.VariableNames)
    exclude=data.oddtrial;
else
    exclude=false(size(x));
end

% exclude nans
exclude=exclude|isnan(x)|isnan(y);

%% psychometric fit
switch p.Results.genModel
    case {'gaussian'}
        ft = fittype( 'normcdf(x,m,sigma)', 'independent', 'x', 'dependent', 'y');
        opts = fitoptions( ft );
        opts.Display = 'Off';
        opts.Lower = [1*min(x),.01];
        opts.StartPoint = [mean(x),1*range(x)./2];
        opts.Upper = [1*max(x),3*range(x)];
        opts.Exclude = exclude;
        [fitresult,gof] = fit( x(:), double(y(:)), ft, opts );
        
    case {'gaussianZerobias'}
        ft = fittype( 'normcdf(x,0,sigma)', 'independent', 'x', 'dependent', 'y');
        opts = fitoptions( ft );
        opts.Display = 'Off';
        opts.Lower = [.01];
        opts.StartPoint = [1*range(x)./2];
        opts.Upper = [3*range(x)];
        opts.Exclude = exclude;
        [fitresult,gof] = fit( x(:), double(y(:)), ft, opts );
        
    case {'guessLapseGaussian'}
        ft = fittype( 'gamma+(1-gamma-lamda).*(normcdf(x,m,sigma))','independent', 'x', 'dependent', 'y' );
        opts = fitoptions( ft );
        opts.Display = 'Off';
        opts.Lower = [0 0 1*min(x),.01];
        opts.StartPoint = [0.25,0.25,mean(x),1*range(x)./2];
        opts.Upper = [0.5,0.5,1*max(x),3*range(x)];
        opts.Exclude = exclude;
        [fitresult,gof] = fit( x(:), double(y(:)), ft, opts );
        
    case {'symmetricLapseGaussian'}
        ft = fittype( 'lamda+(1-2*lamda).*(normcdf(x,m,sigma))','independent', 'x', 'dependent', 'y' );
        opts = fitoptions( ft );
        opts.Display = 'Off';
        opts.Lower = [0 1*min(x),.01];
        opts.StartPoint = [0.25,mean(x),1*range(x)./2];
        opts.Upper = [0.5,1*max(x),3*range(x)];
        opts.Exclude = exclude;
        [fitresult,gof] = fit( x(:), double(y(:)), ft, opts );
        
    case {'guessGaussian'}
        ft = fittype( 'guess+(1-guess).*(normcdf(x,m,sigma))','independent', 'x', 'dependent', 'y');
        opts = fitoptions( ft );
        opts.Display = 'Off';
        opts.Lower = [0 1*min(x),.01];
        opts.StartPoint = [0.25,mean(x),1*range(x)./2];
        opts.Upper = [0.5,1*max(x),3*range(x)];
        opts.Exclude = exclude;
        [fitresult,gof] = fit( x(:), double(y(:)), ft, opts );
end







