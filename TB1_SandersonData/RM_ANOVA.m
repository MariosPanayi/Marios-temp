t.Properties.VariableNames = {'Geno', 'Uid', 'Same', 'Diff'}
SameDiff = table([1 2]','VariableNames',{'Same_Diff'});
rm = fitrm(t, 'Same-Diff ~ Geno', 'WithinDesign',SameDiff);
Wsubj = ranova(rm)
Bsubj = anova(rm)

multcompare(rm,'Same_Diff', 'By', 'Geno')
multcompare(rm,'Geno')
multcompare(rm,'Same_Diff')




t2.Properties.VariableNames = {'Geno', 'Uid', 'Same1', 'Same2', 'Diff1','Diff2'}
WithinFactors = table({'same', 'same', 'diff', 'diff'}', {'Bin1' 'Bin2' 'Bin1' 'Bin2'}','VariableNames',{'Same_Diff', 'Bin'});
rm2 = fitrm(t2, 'Same1-Diff2 ~ Geno', 'WithinDesign',WithinFactors, 'WithinModel', 'Same_Diff+Bin+Same_Diff*Bin');

Wsubj = ranova(rm2,'WithinModel', 'Same_Diff+Bin+Same_Diff*Bin')
Bsubj = anova(rm2)

multcompare(rm2,'Same_Diff', 'By', 'Bin', 'By','Geno')
multcompare(rm2,'Geno')
multcompare(rm2,'Same_Diff')