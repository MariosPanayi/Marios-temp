function [pos_interp] = video_interpolation(pos, ts_interval, sample_rate)

%pos = [ts, x y];
%ts interval = [start end] (in seconds)
%sample_rate = camera frame rate in Hz

%if first column isn't ts then add nans
pos = double(pos);
if size(pos,2) < 3
    pos = [nan(length(pos),1),pos];
end

% ts_interval (in seconds)

samp = ts_interval*sample_rate;
pos_interp = pos(samp(1):samp(2),:);

not_done = 0;
while not_done == 0
    mazepoly1 = create_maze_poly(pos_interp);
    interp_index1 = find_interpolation_points(pos_interp, mazepoly1);
    [pos_interp] = new_interpolation(pos_interp, interp_index1);

    %are you done?
    answer = questdlg('Finished interpolating?', ...
	'Done?', ...
	'Finished','More','More');
    % Handle response
    switch answer
        case 'Finished'
            disp('Saving your work...')
            not_done = 1;
        case 'More'
            disp([answer 'Enjoy'])
            not_done = 0;
    end
   
end

 close all