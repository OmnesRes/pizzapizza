##this script will reproduce all analyses from the four pizza papers, p values are in parentheses
##the sample sizes at the top of the tables are taken at face value
##except paper 1, table 2, where the rows clearly had different sample sizes
##these sample sizes were inferred from paper 2, Table 2
from scipy.stats import f
from scipy.stats import ttest_ind_from_stats
from itertools import combinations_with_replacement
from itertools import permutations
def one_way(means,sds,sizes):
    x=sum([u*n for u,n in zip(means,sizes)])/sum(sizes)
    sb=sum([n*(u-x)**2 for u,n in zip(means,sizes)])/(len(means)-1)
    sw=sum([(n-1)*s**2 for s,n in zip(sds,sizes)])/(sum(sizes)-len(means))
    return sb/sw


#############Paper 1 "Lower buffet prices lead to less taste satisfaction"


print 'Paper 1: "Lower buffet prices lead to less taste satisfaction"'
##table1
print
print 'Table 1'
f1=open('paper1_table1.txt')
data=[i.strip().split() for i in f1]
for i in data:
    sizes=[62,60]
    means=[float(i[0]),float(i[2])]
    sds=[float(i[1]),float(i[3])]
    exact_test=one_way(means,sds,sizes)
    ##for min make sds larger, means closer together
    min_sds=[sd+.005 for sd in sds]
    if means[0]<means[1]:
        min_means=[means[0]+.005,means[1]-.005]
    elif means[1]<means[0]:
        min_means=[means[0]-.005,means[1]+.005]
    else:
        min_means=means
    min_test=one_way(min_means,min_sds,sizes)
    ##for max make sds smaller, means farther apart
    max_sds=[sd-.005 for sd in sds]
    if means[0]<means[1]:
        max_means=[means[0]-.005,means[1]+.005]
    elif means[1]<means[0]:
        max_means=[means[0]+.005,means[1]-.005]
    else:
        max_means=means
    max_test=one_way(max_means,max_sds,sizes)
    print 'Reported:'+'\t'+i[4],i[5]+'\t'+'Exact:'+'\t'+str(round(exact_test,2))+' ('+str(round(1-f.cdf(exact_test,1,sum(sizes)-2),2))+')'+\
          '\t'+'Possible:'+'\t'+str(round(min_test,2))+'-'+str(round(max_test,2))+\
          ' ('+str(round(1-f.cdf(max_test,1,sum(sizes)-2),2))+'-'+str(round(1-f.cdf(min_test,1,sum(sizes)-2),2))+')'


raise

##table2
print
print 'Table 2'
f1=open('paper1_table2.txt')
data=[i.strip().split() for i in f1]
for index, i in enumerate(data):
    ## the sample sizes change depending on how many slices a person ate, but this paper omits this information
    ## sample sizes can be determined using Table 2 of "Peak-end pizza"
    if index<4:
        sizes=[62,60]
    elif 4<=index<7:
        sizes=[41,26]
    else:
        sizes=[47,38]
        
    means=[float(i[0]),float(i[2])]
    sds=[float(i[1]),float(i[3])]
    exact_test=one_way(means,sds,sizes)
    ##for min make sds larger, means closer together
    min_sds=[sd+.005 for sd in sds]
    if means[0]<means[1]:
        min_means=[means[0]+.005,means[1]-.005]
    elif means[1]<means[0]:
        min_means=[means[0]-.005,means[1]+.005]
    else:
        min_means=means
    min_test=one_way(min_means,min_sds,sizes)
    ##for max make sds smaller, means farther apart
    max_sds=[sd-.005 for sd in sds]
    if means[0]<means[1]:
        max_means=[means[0]-.005,means[1]+.005]
    elif means[1]<means[0]:
        max_means=[means[0]+.005,means[1]-.005]
    else:
        max_means=means
    max_test=one_way(max_means,max_sds,sizes)
    print "Fixed sizes"+'\t'+str(sizes)+'\t'+'Reported:'+'\t'+i[4],i[5]+'\t'+'Exact:'+'\t'+str(round(exact_test,2))+' ('+str(round(1-f.cdf(exact_test,1,sum(sizes)-2),2))+')'+\
          '\t'+'Possible:'+'\t'+str(round(min_test,2))+'-'+str(round(max_test,2))+\
          ' ('+str(round(1-f.cdf(max_test,1,sum(sizes)-2),2))+'-'+str(round(1-f.cdf(min_test,1,sum(sizes)-2),2))+')'

##Table 3
#It is unclear how to reproduce this table
    

##############Paper 2 "Lower buffet prices lead to less taste satisfaction"
##Table 1 is identical to Table 1 in Paper 1
##We cannot reproduce Tables 2 and 3 without the data




##############Paper 3 "Eating Heavily: Men Eat More in the Company of Women"
print
print
print
print 'Paper 3: "Eating Heavily: Men Eat More in the Company of Women"'

##Table 1
print
print 'Table 1'
print
print "Males"
##Note, for age the authors did not provide any decimals, assumed to be 44.00 and 43.00
f1=open('paper3_table1_male.txt')
data=[i.strip().split() for i in f1]
n1=40
n2=20
for i in data:
    u1=float(i[0])
    u2=float(i[2])
    s1=float(i[1])
    s2=float(i[3])
    exact_test=ttest_ind_from_stats(u1,s1,n1,u2,s2,n2)
    ##for min make sds larger, means closer together
    min_s1=s1+.005
    min_s2=s2+.005
    if u1<u2:
        min_u1=u1+.005
        min_u2=u2-.005
    elif u2<u1:
        min_u1=u1-.005
        min_u2=u2+.005       
    else:
        min_u1=u1
        min_u2=u2
    min_test=ttest_ind_from_stats(min_u1,min_s1,n1,min_u2,min_s2,n2)
    ##for max make sds smaller, means farther apart
    max_s1=s1-.005
    max_s2=s2-.005
    if u1<u2:
        max_u1=u1-.005
        max_u2=u2+.005
    elif u2<u1:
        max_u1=u1+.005
        max_u2=u2-.005       
    else:
        max_u1=u1
        max_u2=u2
    max_test=ttest_ind_from_stats(max_u1,max_s1,n1,max_u2,max_s2,n2)
    print 'Reported:'+'\t'+i[4]+'\t'+'Exact:'+'\t'+str(round(abs(exact_test[0]),2))+' ('+str(round(exact_test[1],2))+')'+\
          '\t'+'Possible:'+'\t'+str(round(abs(min_test[0]),2))+'-'+str(round(abs(max_test[0]),2))+\
          ' ('+str(round(max_test[1],2))+'-'+str(round(min_test[1],2))+')'


print
print "Females"
f1=open('paper3_table1_female.txt')
data=[i.strip().split() for i in f1]
n1=35
n2=10
for i in data:
    u1=float(i[0])
    u2=float(i[2])
    s1=float(i[1])
    s2=float(i[3])
    exact_test=ttest_ind_from_stats(u1,s1,n1,u2,s2,n2)
    ##for min make sds larger, means closer together
    min_s1=s1+.005
    min_s2=s2+.005
    if u1<u2:
        min_u1=u1+.005
        min_u2=u2-.005
    elif u2<u1:
        min_u1=u1-.005
        min_u2=u2+.005       
    else:
        min_u1=u1
        min_u2=u2
    min_test=ttest_ind_from_stats(min_u1,min_s1,n1,min_u2,min_s2,n2)
    ##for max make sds smaller, means farther apart
    max_s1=s1-.005
    max_s2=s2-.005
    if u1<u2:
        max_u1=u1-.005
        max_u2=u2+.005
    elif u2<u1:
        max_u1=u1+.005
        max_u2=u2-.005       
    else:
        max_u1=u1
        max_u2=u2
    max_test=ttest_ind_from_stats(max_u1,max_s1,n1,max_u2,max_s2,n2)
    print 'Reported:'+'\t'+i[4]+'\t'+'Exact:'+'\t'+str(round(abs(exact_test[0]),2))+' ('+str(round(exact_test[1],2))+')'+\
          '\t'+'Possible:'+'\t'+str(round(abs(min_test[0]),2))+'-'+str(round(abs(max_test[0]),2))+\
          ' ('+str(round(max_test[1],2))+'-'+str(round(min_test[1],2))+')'


print
print 'Table 2'
##Table 2
##Note, this table is a two-way ANOVA, this code needs rpy2 and rpsychi
##Ignore this table if you cannot install these
##The methodology from this article was used to calculate a statistic from summary data for a 2X2 ANOVA:
##Barry H. Cohen (2002) Calculating a Factorial ANOVA From Means and Standard Deviations, Understanding Statistics, 1:3, 191-203, DOI: 10.1207/S15328031US0103_04
import numpy as np
from rpy2 import robjects as ro
import rpy2.robjects.numpy2ri
rpy2.robjects.numpy2ri.activate()
ro.r('library(rpsychi)')

f1=open('paper3_table2.txt')
data=[i.strip().split() for i in f1]
##all possible changes:
combos={}
for combo in combinations_with_replacement([0,.005,-.005],4):
    for permut in permutations(combo):
        combos[permut]=''
ro.globalenv['n']=ro.r.matrix(np.array([40,20,35,10]),nrow=2)
for i in data:
    u=np.array([float(i[0]),float(i[2]),float(i[4]),float(i[6])])
    s=np.array([float(i[1]),float(i[3]),float(i[5]),float(i[7])])
    ro.globalenv['u']=ro.r.matrix(u,nrow=2)
    ro.globalenv['s']=ro.r.matrix(s,nrow=2)
    exact_test=ro.r('ind.twoway.second(u,s,n,digits=10)').rx2('anova.table')[-1]
    
    ##for min test make sds larger
    ro.globalenv['s']=ro.r.matrix(s+.005,nrow=2)
    
    ##for means make all possible changes
    between_row=[]
    between_column=[]
    between_row_column=[]
    for combination in combos:
        ro.globalenv['u']=ro.r.matrix(u+combination,nrow=2)
        test=ro.r('ind.twoway.second(u,s,n,digits=10)')
        between_row.append(test.rx2('anova.table')[-1][1])
        between_column.append(test.rx2('anova.table')[-1][2])
        between_row_column.append(test.rx2('anova.table')[-1][3])
        
    min_row=sorted(between_row)[0]
    min_column=sorted(between_column)[0]
    min_row_column=sorted(between_row_column)[0]

    ##for max test make the sds smaller
    ro.globalenv['s']=ro.r.matrix(s-.005,nrow=2)
    ##for means make all possible changes
    between_row=[]
    between_column=[]
    between_row_column=[]
    for combination in combos:
        ro.globalenv['u']=ro.r.matrix(u+combination,nrow=2)
        test=ro.r('ind.twoway.second(u,s,n,digits=10)')
        between_row.append(test.rx2('anova.table')[-1][1])
        between_column.append(test.rx2('anova.table')[-1][2])
        between_row_column.append(test.rx2('anova.table')[-1][3])
    max_row=sorted(between_row)[-1]
    max_column=sorted(between_column)[-1]
    max_row_column=sorted(between_row_column)[-1]
    print 'Reported:'+'\t'+i[8]+'\t'+i[9]+'\t'+i[10]+'\t'\
          +'Exact:'+'\t'+str(round(exact_test[2],2))+'\t'+str(round(exact_test[1],2))+'\t'+str(round(exact_test[3],2))+'\t'\
          +'Possible:'+'\t'+str(round(min_column,2))+'-'+str(round(max_column,2))+'\t'+\
          str(round(min_row,2))+'-'+str(round(max_row,2))+'\t'+\
          str(round(min_row_column,2))+'-'+str(round(max_row_column,2))



print
print 'Table 3'
##Table 3
f1=open('paper3_table3.txt')
data=[i.strip().split() for i in f1]
##all possible changes:
combos={}
for combo in combinations_with_replacement([0,.005,-.005],3):
    for permut in permutations(combo):
        combos[permut]=''
for i in data:
    sizes=[20,21,19]
    means=np.array([float(i[0]),float(i[2]),float(i[4])])
    sds=np.array([float(i[1]),float(i[3]),float(i[5])])
    exact_test=one_way(means,sds,sizes)
    ##for min make sds larger
    min_sds=sds+.005
    ##for means make all possible changes
    mins=[]
    for combination in combos:
        mins.append(one_way(means+combination,min_sds,sizes))
    min_test=sorted(mins)[0]
    ##for max make sds smaller
    max_sds=sds-.005
    ##for means make all possible changes:
    maxs=[]
    for combination in combos:
        maxs.append(one_way(means+combination,max_sds,sizes))
    max_test=sorted(maxs)[-1]
    print 'Reported:'+'\t'+i[6]+'\t'+'Exact:'+'\t'+str(round(exact_test,2))+'\t'+\
    'Possible:'+'\t'+str(round(min_test,2))+'-'+str(round(max_test,2))


##############Paper 4 "Low prices and high regret: how pricing influences regret at all-you-can-eat buffets"
print
print
print
print 'Paper 4: "Low prices and high regret: how pricing influences regret at all-you-can-eat buffets"'

print
print 'Table 1'
##Table 1
f1=open('paper4_table1.txt')
data=[i.strip().split() for i in f1]
n1=43
n2=52
for i in data:
    u1=float(i[0])
    u2=float(i[2])
    s1=float(i[1])
    s2=float(i[3])
    exact_test=ttest_ind_from_stats(u1,s1,n1,u2,s2,n2)
    ##for min make sds larger, means closer together
    min_s1=s1+.005
    min_s2=s2+.005
    if u1<u2:
        min_u1=u1+.005
        min_u2=u2-.005
    elif u2<u1:
        min_u1=u1-.005
        min_u2=u2+.005       
    else:
        min_u1=u1
        min_u2=u2
    min_test=ttest_ind_from_stats(min_u1,min_s1,n1,min_u2,min_s2,n2)
    ##for max make sds smaller, means farther apart
    max_s1=s1-.005
    max_s2=s2-.005
    if u1<u2:
        max_u1=u1-.005
        max_u2=u2+.005
    elif u2<u1:
        max_u1=u1+.005
        max_u2=u2-.005       
    else:
        max_u1=u1
        max_u2=u2
    max_test=ttest_ind_from_stats(max_u1,max_s1,n1,max_u2,max_s2,n2)
    print 'Reported:'+'\t'+i[4]+'\t'+'Exact:'+'\t'+str(round(abs(exact_test[0]),2))+\
          '\t'+'Possible:'+'\t'+str(round(abs(min_test[0]),2))+'-'+str(round(abs(max_test[0]),2))


print
print 'Table 2'
##Table 2
##Note, this table is a 3X2 ANOVA, this code needs rpy2 and rpsychi
##Ignore this table if you cannot install these
##The methodology from this article was used to calculate a statistic from summary data for a 2X2 ANOVA:
##Barry H. Cohen (2002) Calculating a Factorial ANOVA From Means and Standard Deviations, Understanding Statistics, 1:3, 191-203, DOI: 10.1207/S15328031US0103_04
f1=open('paper4_table2.txt')
data=[i.strip().split() for i in f1]
##all possible changes:
combos={}
for combo in combinations_with_replacement([0,.005,-.005],6):
    for permut in permutations(combo):
        combos[permut]=''
print len(combos)
ro.globalenv['n']=ro.r.matrix(np.array([18,18,7,17,19,10]),nrow=3)
for i in data:
    u=np.array([float(i[0]),float(i[2]),float(i[4]),float(i[6]),float(i[8]),float(i[10])])
    s=np.array([float(i[1]),float(i[3]),float(i[5]),float(i[7]),float(i[9]),float(i[11])])
    ro.globalenv['u']=ro.r.matrix(u,nrow=3)
    ro.globalenv['s']=ro.r.matrix(s,nrow=3)
    exact_test=ro.r('ind.twoway.second(u,s,n,digits=10)').rx2('anova.table')[-1]
    
    ##for min test make sds larger
    ro.globalenv['s']=ro.r.matrix(s+.005,nrow=3)
    
    ##for means make all possible changes
    ##will take some time
    between_row=[]
    between_column=[]
    between_row_column=[]
    for combination in combos:
        ro.globalenv['u']=ro.r.matrix(u+combination,nrow=3)
        test=ro.r('ind.twoway.second(u,s,n,digits=10)')
        between_row.append(test.rx2('anova.table')[-1][1])
        between_column.append(test.rx2('anova.table')[-1][2])
        between_row_column.append(test.rx2('anova.table')[-1][3])
        
    min_row=sorted(between_row)[0]
    min_column=sorted(between_column)[0]
    min_row_column=sorted(between_row_column)[0]

    ##for max test make the sds smaller
    ro.globalenv['s']=ro.r.matrix(s-.005,nrow=3)
    ##for means make all possible changes
    ##will take some time
    between_row=[]
    between_column=[]
    between_row_column=[]
    for combination in combos:
        ro.globalenv['u']=ro.r.matrix(u+combination,nrow=3)
        test=ro.r('ind.twoway.second(u,s,n,digits=10)')
        between_row.append(test.rx2('anova.table')[-1][1])
        between_column.append(test.rx2('anova.table')[-1][2])
        between_row_column.append(test.rx2('anova.table')[-1][3])
    max_row=sorted(between_row)[-1]
    max_column=sorted(between_column)[-1]
    max_row_column=sorted(between_row_column)[-1]
    print 'Reported:'+'\t'+i[12]+'\t'+i[13]+'\t'+i[14]+'\t'\
          +'Exact:'+'\t'+str(round(exact_test[2],2))+'\t'+str(round(exact_test[1],2))+'\t'+str(round(exact_test[3],2))+'\t'\
          +'Possible:'+'\t'+str(round(min_column,2))+'-'+str(round(max_column,2))+'\t'+\
          str(round(min_row,2))+'-'+str(round(max_row,2))+'\t'+\
          str(round(min_row_column,2))+'-'+str(round(max_row_column,2))

print
print 'Table 3'
##Table 3
print
print '1 piece'
f1=open('paper4_table3_1piece.txt')
data=[i.strip().split() for i in f1]
for i in data:
    sizes=[18,19]
    means=[float(i[0]),float(i[2])]
    sds=[float(i[1]),float(i[3])]
    exact_test=one_way(means,sds,sizes)
    ##for min make sds larger, means closer together
    min_sds=[sd+.005 for sd in sds]
    if means[0]<means[1]:
        min_means=[means[0]+.005,means[1]-.005]
    elif means[1]<means[0]:
        min_means=[means[0]-.005,means[1]+.005]
    else:
        min_means=means
    min_test=one_way(min_means,min_sds,sizes)
    ##for max make sds smaller, means farther apart
    max_sds=[sd-.005 for sd in sds]
    if means[0]<means[1]:
        max_means=[means[0]-.005,means[1]+.005]
    elif means[1]<means[0]:
        max_means=[means[0]+.005,means[1]-.005]
    else:
        max_means=means
    max_test=one_way(max_means,max_sds,sizes)
    print 'Reported:'+'\t'+i[4]+'\t'+'Exact:'+'\t'+str(round(exact_test,2))+\
          '\t'+'Possible:'+'\t'+str(round(min_test,2))+'-'+str(round(max_test,2))

print
print '2 pieces'
f1=open('paper4_table3_2piece.txt')
data=[i.strip().split() for i in f1]
for i in data:
    sizes=[18,21]
    means=[float(i[0]),float(i[2])]
    sds=[float(i[1]),float(i[3])]
    exact_test=one_way(means,sds,sizes)
    ##for min make sds larger, means closer together
    min_sds=[sd+.005 for sd in sds]
    if means[0]<means[1]:
        min_means=[means[0]+.005,means[1]-.005]
    elif means[1]<means[0]:
        min_means=[means[0]-.005,means[1]+.005]
    else:
        min_means=means
    min_test=one_way(min_means,min_sds,sizes)
    ##for max make sds smaller, means farther apart
    max_sds=[sd-.005 for sd in sds]
    if means[0]<means[1]:
        max_means=[means[0]-.005,means[1]+.005]
    elif means[1]<means[0]:
        max_means=[means[0]+.005,means[1]-.005]
    else:
        max_means=means
    max_test=one_way(max_means,max_sds,sizes)
    print 'Reported:'+'\t'+i[4]+'\t'+'Exact:'+'\t'+str(round(exact_test,2))+\
          '\t'+'Possible:'+'\t'+str(round(min_test,2))+'-'+str(round(max_test,2))

print
print '3 pieces'
f1=open('paper4_table3_3piece.txt')
data=[i.strip().split() for i in f1]
for i in data:
    sizes=[7,12]
    means=[float(i[0]),float(i[2])]
    sds=[float(i[1]),float(i[3])]
    exact_test=one_way(means,sds,sizes)
    ##for min make sds larger, means closer together
    min_sds=[sd+.005 for sd in sds]
    if means[0]<means[1]:
        min_means=[means[0]+.005,means[1]-.005]
    elif means[1]<means[0]:
        min_means=[means[0]-.005,means[1]+.005]
    else:
        min_means=means
    min_test=one_way(min_means,min_sds,sizes)
    ##for max make sds smaller, means farther apart
    max_sds=[sd-.005 for sd in sds]
    if means[0]<means[1]:
        max_means=[means[0]-.005,means[1]+.005]
    elif means[1]<means[0]:
        max_means=[means[0]+.005,means[1]-.005]
    else:
        max_means=means
    max_test=one_way(max_means,max_sds,sizes)
    print 'Reported:'+'\t'+i[4]+'\t'+'Exact:'+'\t'+str(round(exact_test,2))+\
          '\t'+'Possible:'+'\t'+str(round(min_test,2))+'-'+str(round(max_test,2))








