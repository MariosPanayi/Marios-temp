function [starttime, endtime,index_start, index_end] = get_trial_TTL(TTL,ts,threshold)
index = [];
TTL_threshold = ((max(TTL)-min(TTL))/threshold);
trial_times = [];

TTL_shift = [TTL(1) [TTL(1:(length(TTL)-1))]];
diff = TTL-TTL_shift;

starttime = ts(diff > TTL_threshold)';
endtime = ts(diff < -TTL_threshold)';
index_start = find(diff > TTL_threshold)';
index_end = find(diff < -TTL_threshold)';

