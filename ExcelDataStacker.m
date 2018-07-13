%Stacks numeric data from multiple sequentially number excel files
clc
clear all
data = [];
for i = 1:44
filepath = 'H:\Experiments Being Run\MP009_LaylaMsc_GluA1\Data_KO_control\Y maze\y maze all other - Test ';
fileindex =  sprintf(' %d.csv',i);
filename = strcat(filepath, fileindex)
[num,~,~] = xlsread(filename);

filenum = ones(size(num,1),1) * i;
num = [filenum,num];
data = [data; num];

end