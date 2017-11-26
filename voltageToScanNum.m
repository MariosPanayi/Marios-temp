function [ output_args ] = voltToNumRange( V_min, V_max, ascending, no_of_channels, waveform_start, waveform_peak)
%voltToNumRange use voltagesweep.m to identify the min and max scan number associated with the  
%   V_min - minimum voltage e.g. 0.6
%   V_max - maximum voltage e.g. 0.8
%   ascending - [1,0] TRUE or FALSE to indicate whether voltage parameters come from ascending or descending sweep
%   num_channels -  Specify the number of channels used during recording [usually 1,2]
%   waveform_start - starting voltage of the waveform [if no input, default to -0.4]
%   waveform_peak - peak voltage of the waveform [if no input, default to +1.3]
%

end

