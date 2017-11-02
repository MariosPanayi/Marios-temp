
%Does this need to be in cv_params?
V_min = 0.6;
V_max = 0.8;
% Indicate whether this voltage refers to the ascending [ = 1] or descending [ = 0] 
% component of the triangular waveform
V_ascending = 1;

voltages = voltagesweep(no_of_channels);

if V_ascending
    peak_min = find(voltages > V_min, 1, 'first');
    peak_max = find(voltages > V_max, 1, 'first');
else
    peak_min = find(voltages > V_min, 1, 'last');
    peak_max = find(voltages > V_max, 1, 'last');
end