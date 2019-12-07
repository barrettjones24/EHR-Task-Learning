#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Oct 29 11:48:44 2019

@author: barrett

This code is used to preprocess data for the Word2vec algorithm run by Xinmeng Zhang.
Word embeddings are sent back from Xinmeng and then clustering is done. Clusters are mapped
to event logs and formatted for visualiztion in ProM.

Data sets are not included because they contain PHI.
"""

import pandas as pd
import numpy as np

## read files
emp_dept    = pd.read_csv('data/employee_dept.csv')
logs        = pd.read_csv('data/infant-logs.csv')
health_cond = pd.read_csv('data/Inpatient_health conditions.csv')
demo        = pd.read_csv('data/patient-Demo.csv')
event       = pd.read_csv('data/unique event.csv')
ec          = pd.read_csv('event_codes.csv')

## split data by dept
ec1 = ec[ec['DEPTID'] == 209641]
ec2 = ec[ec['DEPTID'] == 104595]

## create list of lists
ec_gb1 = ec1.groupby(['PATIENTID', 'EMPLOYEE_ID', 'DEPTID'])['EVENTID'].apply(list)
ec_gb2 = ec2.groupby(['PATIENTID', 'EMPLOYEE_ID', 'DEPTID'])['EVENTID'].apply(list)

lecgb_1 = list(ec_gb1)
lecgb_2 = list(ec_gb2)


## create files
np.save('input_pedsres.npy', lecgb_1)
np.save('input_pedsgen.npy', lecgb_2)


## read in results from Xinmeng
result = np.load('Archive/res/result2_100.npy')
mdata  = pd.read_csv('Archive/res/metadata2.tsv', sep = '\t', header = None)





## K means clustering
# clustering dataset
# determine k using elbow method

from sklearn.cluster import KMeans
import numpy as np


## fit k means
km = KMeans(n_clusters = 5)
km.fit(result)
labs = km.predict(result)

## format columns
mdata['labs']   = labs
mdata['deptid'] = mdata[0].str.split('_', expand = True)[0]
mdata['evid']   = mdata[0].str.split('_', expand = True)[1].astype(int)

## join descriptions
dept   = emp_dept[['DEPTID', 'DEPT_DESCR']].drop_duplicates()
mdata2 = mdata.merge(event,
            how = 'left',
            left_on = 'evid',
            right_on = 'ID')

mdata3 = mdata2.merge(dept,
                      how = 'left',
                      left_on = 'deptid',
                      right_on = 'DEPTID')

## create datasets for Prom
prdat = ec.merge(mdata3,
                 how = 'left',
                 left_on = 'DEPTID_EVENTID',
                 right_on = 0)

## choose columns for ProM
prdat_col = prdat[['EVENTID', 'EVENT SHORT', 'labs', 'SESSION']]


## split data by cluster
gb = prdat_col.groupby('labs')    
dfs = [gb.get_group(x) for x in gb.groups]

## output clusters to csv
for num, name in enumerate(dfs):
    name.to_csv('data/prdat' + str(num) + '.csv', index = False)





