function rez = memorizeW(rez, W, dWU,U,mu)

% the following 4 variables are enough to memorize the templates
rez.dWU = dWU;
rez.W = gather(W);
rez.U = gather(U);
rez.mu = gather(mu);

% this is for reconstructing the waveforms, but really it should be very similar to dWU
% we need to un-whiten this to make it into unwhitened templates
rez.Wraw = [];
for n = 1:size(U,2)
    % temporarily use U rather Urot until I have a chance to test it
    temp = rez.mu(n) * sq(rez.U(:,n,:));
    % modified by zzw, 20220616
    if ndims(temp)==2
        temp = reshape(temp, 1, []);
    end
    rez.Wraw(:,:,n) = temp * sq(rez.W(:,n,:))';
      
end
