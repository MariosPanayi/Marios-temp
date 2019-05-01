    if runglm == 1
        cd ../Behaviour
        if p ~= [5 12] %% exclude rats 5 and 12
            
            %% standard model
%             DAglm = Data_base(3:end,:);

%Import sample data
            temp = dlmread('C:\Users\mpanagi\Documents\GitHub\Marios-temp\TB1_SandersonData\MEW_GLM\temp_Table_for_marios.txt');
            DAglm = dlmread('C:\Users\mpanagi\Documents\GitHub\Marios-temp\TB1_SandersonData\MEW_GLM\DAglm_DAdata_for_marios.txt');

            p = 1;
            
            % Remove error trials
%             temp = tbl{p};
            temp(:,(isnan(temp(1,:)) == 1)) = [];
%             DAglm = Data_base (3:end,find(Data_base(2,:) < 10));

            reward = temp(1,:); %% pellet / sucrose
            trial = temp(2,:);  %% forced v choice
            idsup = temp(3,:);  %% ID surprise
            rewXid = temp(4,:); %% ID surprise x reward
            valsup = temp(5,:); %% value surprise
            rewXval = temp(6,:); %% 
            peleat = temp(7,:); %% numbers of pellets consumed
            suceat = temp(8,:);
            perf = temp(9,:); %% %choice in choice trials at end of block
            

        %% Model fit
                    for i = 80:160;
                        xxx = table(reward',trial',idsup',rewXid',valsup',rewXval',peleat',perf',DAglm(i,:)','VariableNames',{'reward','forcedchoice','identity','rewXidentity','value','rewXvalue','eaten','choice','DA'});
                        mdl = fitglm(xxx,'DA ~ 1 + reward + forcedchoice + identit y + rewXidentity + value + rewXvalue + eaten + choice');

                        src(p,i,:) = (mdl.Coefficients.Estimate(1:end));
                        srcab(p,i,:) = abs(mdl.Coefficients.Estimate(1:end));
                    end
                    
                     
        %% Run PseudoGLM
                if psglm ~= 0
                    for iperm = 1:1000;
                        
                        %% Standard model
                        disp(sprintf('permutation number %d',iperm))
                        preward = reward(randperm(length(reward))); %% pellet / sucrose
                        ptrial = trial(randperm(length(reward)));  %% forced v choice
                        pidsup = idsup(randperm(length(reward)));  %% ID surprise
                        prewXid = rewXid(randperm(length(reward))); %% ID surprise x reward
                        pvalsup = valsup(randperm(length(reward))); %% value surprise
                        prewXval = rewXval(randperm(length(reward))); %% 
                        ppeleat = peleat(randperm(length(reward))); %% numbers of pellets consumed
                        psuceat = suceat(randperm(length(reward)));
                        pchoice = choice(randperm(length(reward)));
                  
                     

                        %% Standard model
                        for i = 80:160;
                            xxx = table(preward',ptrial',pidsup',prewXid',pvalsup',prewXval',ppeleat',pchoice',DAglm(i,:)','VariableNames',{'reward','forcedchoice','identity','rewXidentity','value','rewXvalue','eaten','choice','DA'});
                            mdlp = fitglm(xxx,'DA ~ 1 + reward + forcedchoice + identity + rewXidentity + value + rewXvalue + eaten + choice');

                            pseudosrc(p,i,:,iperm) = (mdlp.Coefficients.Estimate(1:end));
                            pseudosrcab(p,i,:,iperm) = abs(mdlp.Coefficients.Estimate(1:end));
                        end
   
                    end
                end
                
                %% Organise data
                msrc = squeeze(nanmean(src));
                ssrc = squeeze((nanstd(src,1))./sqrt(11));   %% for 11 subjects
                
                for i = 1:8  %% for 8 regressors in cue standard
                    t{i} = (squeeze(nanmean(squeeze(pseudosrc(:,:,i,:)))))';
                end

                for i = 1:8  %% for cue standard
                    data = [t{i}];
                    tmax(i,:) = max(data);
                    tmin(i,:) = min(data);
                end  

                sig.pos1 = find(msrc(:,1) >= (tmax(1,:)'));
                sig.pos2 = find(msrc(:,2) >= (tmax(2,:)'));
                sig.pos3 = find(msrc(:,3) >= (tmax(3,:)'));
                sig.pos4 = find(msrc(:,4) >= (tmax(4,:)'));
                sig.pos5 = find(msrc(:,5) >= (tmax(5,:)'));
                sig.pos6 = find(msrc(:,6) >= (tmax(6,:)'));
                sig.pos7 = find(msrc(:,7) >= (tmax(7,:)'));
                sig.pos8 = find(msrc(:,8) >= (tmax(8,:)'));

                sig.neg1 = find(msrc(:,1) <= (tmin(1,:)'));
                sig.neg2 = find(msrc(:,2) <= (tmin(2,:)'));
                sig.neg3 = find(msrc(:,3) <= (tmin(3,:)'));
                sig.neg4 = find(msrc(:,4) <= (tmin(4,:)'));
                sig.neg5 = find(msrc(:,5) <= (tmin(5,:)'));
                sig.neg6 = find(msrc(:,6) <= (tmin(6,:)'));
                sig.neg7 = find(msrc(:,7) <= (tmin(7,:)'));
                sig.neg8 = find(msrc(:,8) <= (tmin(8,:)'));
            