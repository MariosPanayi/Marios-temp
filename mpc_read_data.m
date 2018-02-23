function data = mpc_read_data(filename)
    
    % Read MED-PC data files.
    %
    % USAGE: data = mpc_read_data(filename)
    %
    % INPUTS:
    %   filename - name of MED-PC file
    %
    % OUTPUTS:
    %   data - structure containing fields corresponding to the different
    %          veriables in the MED-PC file. Numerical matrices are
    %          converted to vectors.
    %
    % Sam Gershman, June 2012
    
    f = fopen(filename);
    
    tline = fgetl(f);
    c = 0;
    x = [];
%     data = cell(4,1);
    
    while ischar(tline)
        tline = fgetl(f);
        c = c + 1;
        T{c} = tline;
        if numel(tline)>0 && strcmp(tline(1),' ');  %check if previous line indicates a matrix name
            k = find(tline==':',1);
            if numel(x)==0
                j = find(T{c-1}==':',1);
                a = T{c-1}(1:j-1);          %matrix name
            end
            x = [x str2num(tline(k+1:end))]; %concatenate line with vectorized matrix
        elseif numel(x)>0 %this means it's the end of the matrix
            temp.(a)= x;
            x = [];
        elseif c > 1 %or, it's not a matrix
            j = find(T{c-1}==':',1);
            a = T{c-1}(1:j-1);
            a(a==' ') = '_';    %you can't have spaces in field names, so convert to underscore       
            if strcmp(a,'Start_Date') &&  exist('temp','var'); 
                data{temp.Box} = temp; 
            end
            if numel(a)>0
                h = T{c-1}(j+1:end);
                if h(1)==' '; h(1) = []; end %get rid of extraneous spaces at the beginning
                if any(isletter(h)|h=='/'|h==':') %check if it's numeric
                    temp.(a)= h;
                else                   
                    temp.(a)= str2num(h);
                end
            end
        end
    end
    data = temp;
%     data{temp.Box} = temp;
    fclose(f);