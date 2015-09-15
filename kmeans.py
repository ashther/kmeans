# -*- coding: utf-8 -*-
"""
Created on Tue Sep 15 13:56:51 2015

@author: Administrator
"""

import random
import numpy as np
import matplotlib.pyplot as plt
from dataSource import *

def centerCreate(data, k):
    n_dim = len(zip(*data))
    center = np.zeros((n_dim, k))
    for i in range(n_dim):
        for j in range(k):
            center[i, j] = random.uniform(min(zip(*data)[i]), max(zip(*data)[i]))
    return zip(*(center.tolist()))

def centerAdjust(data, center):
    temp_data = np.array(data)
    temp_center = np.array(center)
    center_adjust = []
    
    cluster = np.zeros(len(temp_data))
    for i, da in enumerate(temp_data):
        cluster[i] = np.argmin(map(sum, (da - temp_center)**2))
    
    for i in range(len(center)):
        cluster_data = temp_data[np.where(cluster == i)]
        temp = []
        for j in range(len(cluster_data[0])):
            temp.append(cluster_data[:, j].mean())
        center_adjust.append(temp)
    return center_adjust, cluster
    
def kmeans(data, k):
    center = centerCreate(data, k)
    n = 0
    while True:
        center, cluster = centerAdjust(data, center)
        if n == 0:
            pass
        elif all(cluster == cluster_adjust) | (n >= 1000):
            break
        cluster_adjust = cluster
        n += 1
    return cluster

def clusterShow(data, cluster):
    mark = ['Dr', 'Db', 'Dg', 'Dk', '^b', '+b', 'sb', 'db', '<b', 'pb']
    temp_data = np.array(data)
    for i in set(cluster):
        plt.plot(temp_data[np.where(cluster==i)][:, 0], 
                           temp_data[np.where(cluster==i)][:, 1], 
                           mark[int(i)], markersize=6)
if __name__ == '__main__':
    k = 4
    cluster = kmeans(data, k)
    clusterShow(data, cluster)
    
    
    
    
    
    