ts = [0:0.1:60022/10-0.1];
    plot(ts,TTLs)
    title('TTLs');xlim([ts(1),max(ts)]);xlabel('Times(s)');
  
for i = 1:size(TTLs,2)
TTLs_plot(:,i) = TTLs(:,i) + i
end

plot(ts,TTLs_plot)