experiment = 'local_D1agonist_V2'; % change to experiment of interest
start_directory = '/Users/LauraGrima/Dropbox/pharmacology_exps_new/all_data/';

rats = {'13'};
drugs = {'sal_'};

filename = get_exp_filenames(experiment,'extract',rats,drugs);


for i = 1:length(filename)
    
    clear variable
    
    data = mpc_read_data(filename{i});
    
    fprintf('Working on %s \n',filename{i})
    
    events = data.E;
    times = data.T;
    
    %% Finding start, ends, and durations of poke times
    % find all the indices in events of when an animal is poking in (301 and 601)
    in_poke = find(events == 301|events == 601);
    
    % get associated times 
    in_poke_times = times(in_poke);
    
    % find differences between times and from these calculate durations
    in_poke_diffs = diff(in_poke_times);
    
    % find indices of when times are more than one, find corresponding actual times
    end_indices = find(in_poke_diffs>1);
    start_indices = [1, (ends_indices+1)];
    start_indices = start_indices(1:end-1);
    poke_starts = in_poke_times(start_indices);
    poke_ends = in_poke_times(end_indices);
    poke_durations = end_times - start_times;
    
    %% Finding error times 
    error_idx = find(events == 503);
    error_times = times(error_idx);
    
    %% Finding start times of each trial 
    cue_idx = find(events == 103);
    cue_times = times(cue_idx);
    
    save('/Users/LauraGrima/Dropbox/pharmacology_exps_new/video_tracking_variables.mat','error_times','cue_times');