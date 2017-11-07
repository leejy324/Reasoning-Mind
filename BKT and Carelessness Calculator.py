# -*- coding: utf-8 -*-
"""
Created on Sat Nov  4 13:32:21 2017

@author: leejy
"""

def BKT_Calculator(DataSet):
    #DataSet = pd.merge(DataSet, Skill_parameters, how = 'left')
    DataSet['L0'] = DataSet['L0'].fillna(DataSet['L0'].mean())
    DataSet['G'] = DataSet['G'].fillna(DataSet['G'].mean())
    DataSet['S'] = DataSet['S'].fillna(DataSet['S'].mean())
    DataSet['T'] = DataSet['T'].fillna(DataSet['T'].mean())
    DataSet.Results = DataSet.Results.astype(int)
    Ln_1 = [0]*len(DataSet)
    Ln_1_Result = [0]*len(DataSet)
    Ln = [0]*len(DataSet)
    L0 = list(DataSet.loc[:,'L0'])
    S = list(DataSet.loc[:,'S'])
    G = list(DataSet.loc[:,'G'])
    T = list(DataSet.loc[:,'T'])
    Results = list(DataSet.loc[:,'Results'])
    skill = list(DataSet.loc[:,'skill'])
    student = list(DataSet.loc[:,'student'])
    Reversed_T = [1-item for item in T]
    Reversed_S = [1-item for item in S]
    Reversed_G = [1-item for item in G]
    TRIO = ["."]*len(DataSet)
    LASTTWO = ['.']*len(DataSet)
    P_TRIO_Ln = [0]*len(DataSet)
    P_TRIO_non_Ln = [0]*len(DataSet)
    P_TRIO = [0]*len(DataSet)
    P_Ln_TRIO = [0]*len(DataSet)
    Guess = [0]*len(DataSet)
    Carelessness = [0]*len(DataSet)
        
    for i in iter(range(0,len(DataSet))):
        if (skill[i] != skill[i-1]) | (student[i] != student[i-1]):
            Ln_1[i] =  L0[i]
        else:
            Ln_1[i] = Ln[i-1]
        if Results[i] == 1:
            Ln_1_Result[i] = ((Ln_1[i]*(1-S[i]))/((Ln_1[i]*(1-S[i]))+((1-Ln_1[i])*G[i])))
        else:
            Ln_1_Result[i] = ((Ln_1[i]*(S[i]))/((Ln_1[i]*(S[i]))+((1-Ln_1[i])*(1-G[i]))))
        Ln[i] = Ln_1_Result[i] + (1-Ln_1_Result[i])*T[i]
       
    for i in iter(range(0,len(DataSet)-2)):
        if student[i] != student[i+2]:
            TRIO[i] = '.'
        else:
            if (Results[i] == 1) & (Results[i+1] == 1) & (Results[i+2] == 1):
                TRIO[i] = 'RRR'
            elif (Results[i] == 1) & (Results[i+1] == 1) & (Results[i+2] == 0):
                TRIO[i] = 'RRW'
            elif (Results[i] == 1) & (Results[i+1] == 0) & (Results[i+2] == 1):
                TRIO[i] = 'RWR'
            elif (Results[i] == 1) & (Results[i+1] == 0) & (Results[i+2] == 0):
                TRIO[i] = 'RWW'
            elif (Results[i] == 0) & (Results[i+1] == 1) & (Results[i+2] == 1):
                TRIO[i] = 'WRR'
            elif (Results[i] == 0) & (Results[i+1] == 1) & (Results[i+2] == 0):
                TRIO[i] = 'WRW'
            elif (Results[i] == 0) & (Results[i+1] == 0) & (Results[i+2] == 1):
                TRIO[i] = 'WWR'
            elif (Results[i] == 0) & (Results[i+1] == 0) & (Results[i+2] == 0):
                TRIO[i] = 'WWW'
                
    for i in iter(range(0,len(DataSet)-2)):
        if student[i] != student[i+2]:
            LASTTWO[i] = '.'
        elif student[i] == student[i+2]:
            if (Results[i+1] == 1) & (Results[i+2] == 1):
                LASTTWO[i] = 'RR'
            elif (Results[i+1] == 1) & (Results[i+2] == 0):
                LASTTWO[i] = 'RW'
            elif (Results[i+1] == 0) & (Results[i+2] == 1):
                LASTTWO[i] = 'WR'
            elif (Results[i+1] == 0) & (Results[i+2] == 0):
                LASTTWO[i] = 'WW'
         
    for i in iter(range(0,len(DataSet))):
        if LASTTWO[i] == '.':
            P_TRIO_Ln[i] = np.nan
        else:
            if LASTTWO[i] == 'RR':
                P_TRIO_Ln[i] = Reversed_S[i]*Reversed_S[i]
            elif LASTTWO[i] == 'RW':
                P_TRIO_Ln[i] = Reversed_S[i]*S[i]
            elif LASTTWO[i] == 'WR':
                P_TRIO_Ln[i] = Reversed_S[i]*S[i]
            elif LASTTWO[i] == 'WW':
                P_TRIO_Ln[i] = S[i]*S[i]
                
    for i in iter(range(0,len(DataSet))):
        if LASTTWO[i] == '.':
            P_TRIO_non_Ln[i] = np.nan
        else:
            if LASTTWO[i] == 'RR':
                P_TRIO_non_Ln[i] = ((Reversed_T[i]**2)*(G[i]**2)) + (Reversed_T[i]*T[i]*\
                G[i]*Reversed_S[i]) + (T[i]*(Reversed_S[i]**2))
            elif LASTTWO[i] == 'RW':
                P_TRIO_non_Ln[i] = ((Reversed_T[i]**2)*G[i]*Reversed_G[i]) + \
                (Reversed_T[i]*T[i]*G[i]*S[i]) + (T[i]*Reversed_S[i]*S[i])
            elif LASTTWO[i] == 'WR':
                P_TRIO_non_Ln[i] = ((Reversed_T[i]**2)*Reversed_G[i]*G[i]) + \
                (Reversed_T[i]*T[i]*Reversed_G[i]*Reversed_S[i]) + \
                (T[i]*S[i]*Reversed_S[i])
            elif LASTTWO[i] == 'WW':
                P_TRIO_non_Ln[i] = ((Reversed_T[i]**2)*(Reversed_G[i]**2)) + \
                (Reversed_T[i]*T[i]*Reversed_G[i]*S[i]) + (T[i]*(S[i]**2))
    
    for i in iter(range(0,len(DataSet))):
        if TRIO[i] == '.':
            P_TRIO[i] = np.nan
        else:
            P_TRIO[i] = (Ln[i]*P_TRIO_Ln[i]) + ((1-P_TRIO_Ln[i])*P_TRIO_non_Ln[i])
    
    for i in iter(range(0,len(DataSet))):
        if TRIO[i] == '.':
            P_Ln_TRIO[i] = '.'
        else:
            P_Ln_TRIO[i] = (P_TRIO_Ln[i]*Ln[i])/P_TRIO[i]
    
    for i in iter(range(0,len(DataSet))):
        if (TRIO[i] == '.') | (Results[i] == 0):
            Guess[i] = np.nan
        else:
            Guess[i] = 1-P_Ln_TRIO[i]
    
    for i in iter(range(0,len(DataSet))):
        if (TRIO[i] == '.') | (Results[i] == 1):
            Carelessness[i] = np.nan
        else:
            Carelessness[i] = P_Ln_TRIO[i]
    
    DataSet['Ln-1'] = Ln_1
    DataSet['Ln-1_Result'] = Ln_1_Result
    DataSet['Ln'] = Ln   
    DataSet['Guess'] = Guess
    DataSet['Carelessness'] = Carelessness
    return DataSet