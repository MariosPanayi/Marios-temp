function [pos_interp] = new_interpolation(data, interp_index, timeout, dist_interp)

% function [pos_interp] = new_interpolation(data, timeout)
%
% Interpolates rat position data. Data is interpolated if either:
%       1 - A data point's position is outside of a defined box (mazePoly)
%
%
% Inputs:
%  data - behavioural data with positions in the form of a t-by-3 vector
%         where t is the number of timestamps. The 3 columns of the data
%         are: timestamps, x coordinates of the rat at timestamp, y
%         coordinate of the rat at timestamp.
%           [timestamps, x coordinates, y coordinates]
%  interp_index - points previously selected for interpolation, outside of
%                 user defined polygon
%
% Optional Inputs:
%  timeout - minimal number of timesteps by which continual interpolation 
%            invalidD is not considered valid, data will be marked as 
%            invalid.
%            Default: 1000000 in order to disable timeout for signal loss
%            less 1000000 timesteps
% dist_interp - 

% Outputs:
%  pos_interp - data with interpolated missing positions
%
%  contents of pos_interp:
%  pos_interp (:,1) - time stamp
%  pos_interp (:,2) - x position of electrode
%  pos_interp (:,3) - y position of electrode
%  pos_interp (:,4) - v validity of datapoint
%
%  v - Vector showing if data is valid(1) or invalid(0), validiy is defined
%      by either loss of signal for greater than a given number of 
%      timesteps (timeout)

%set up variables
t = data (:,1);     %time stamp
x = data (:,2);     %position of head electrode
y = data (:,3);
N = length (t);     %number of time points
invalid = zeros(N, 1); %timeout points

pos_was_interped = zeros(N,1);
v = ones(N,1); %overall validity

if nargin < 3
    timeout = 1000000;
    dist_interp = -1;
elseif nargin < 4
    dist_interp = -1;
end

figure
hold on

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% build and run interpolation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interp_x = interpolate_vector(x, interp_index);
interp_y = interpolate_vector(y, interp_index);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Find tracking errors within maze
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


if dist_interp > 0
    
    diffxq = diff(interp_x);
    diffyq = diff(interp_y);
    
    dist_interpx = (abs(diffxq) > dist_interp);
    dist_interpy = (abs(diffyq) > dist_interp);

    interp_x = interpolate_vector(interp_x, ~dist_interpx);
    interp_y = interpolate_vector(interp_y, ~dist_interpy);

    %plot(interp_x(dist_interpy), interp_y(dist_interpy),'mo')
    %plot(interp_x(dist_interpx), interp_y(dist_interpx),'bo')

    plot(interp_x, interp_y, 'color', [.7 .7 .7])
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% work out validity
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% V = [0 0 0 0 0 0 0 0 1 1 1 1 1 1 0 1 0 0 0 0 1 1 0 1 0 0 1]
% timestamp = [find(diff([-1 V -1]) ~= 0)] % where does V change
% runlength = diff(timestamp) ;
% % to split them for 0's and 1's
% runlength0 = runlength(1+(V(1)==1):2:end)
% runlength1 = runlength(1+(V(1)==0):2:end)


%v = valid interpolation
pos_interp = [t, interp_x', interp_y'];

%show result of interpolation
plot(interp_x, interp_y, 'Color', [.7 .7 .7]);
 
leg = {'Valid Position Data'};

if ~isempty(interp_index)
    plot(interp_x(~interp_index), interp_y(~interp_index), '.c');
    leg = [leg,'Poly Interpolation'];
end

% if ~isempty(interp_index)
%     plot(interp_x(~interp_index), interp_y(~interp_index), '+r');
%     leg = [leg,'Invalid due to Signal Loss'];
% end

legend(leg);


