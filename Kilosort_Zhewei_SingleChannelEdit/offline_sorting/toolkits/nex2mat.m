function nex2mat(filename)
%%%%% convert .nex file to mat file for offline sorting 

% convert nex file to .mat file for further analysiss

path = 'D:/Zhewei/ER050603-01.nex';

data = readNexFile(path);

assert(strcmp(data.markers{41, 1}.name, 'Strobed'));
strobes = str2num(cell2mat(data.markers{41}.values{1, 1}.strings));
strobes_time = data.markers{41, 1}.timestamps;
neurons = data.neurons;

save(strcat(path(1:end-4), '.mat'), "neurons", "strobes", "strobes_time")

%% for kilosort, I developed this, not correct
path = 'H:/offline/16_r17_66_67___74_75_R_G.nex';
data = readNexFile(path);


data_neuron = zeros(32, 1e7); % No. electodes by No.time point
for i =1:32
    disp(i);
    data_neuron(i, :) = int16(1000*data.contvars{i, 1}.data(1e7+1:2e7, 1));
end

fid = fopen('D:\Zhewei\pliot\data\pliotFile.bin', 'w');
fwrite(fid, data_neuron, 'int16');
fclose(fid);  

%% for kilosort. someone from internet suggested this

file = 'D:\Zhewei\offline_sorting\data\rawdata\ER050603.pl2';
data_neuron = zeros(16, 325195340); % No. electodes by No.time point

for i = 17:32
    disp(i);
    if i < 10
        name =  ['WB0', num2str(i)];
    else
        name =  ['WB', num2str(i)];
    end
    [data, n, ts, fn, ad] = plx_ad(file, name);
    data_neuron(i-16,: ) = ad(:, 1);
end


% data_neuron = data_neuron - mean(data_neuron,   2); % subtract mean of each channel
data_neuron = data_neuron - median(data_neuron, 1); % subtract median across channels
data_neuron = int16(data_neuron);

for i = 17
    channelfile = sprintf('D:\\Zhewei\\offline_sorting\\data\\ER050603_%d_oldmethod.bin', i);
    disp(i);
    fid = fopen(channelfile, 'w');
    fwrite(fid, data_neuron(i-16,:), 'int16');
    fclose(fid);
end

