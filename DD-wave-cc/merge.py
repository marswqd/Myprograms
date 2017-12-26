#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import sys
import glob
import subprocess

os.putenv("SAC_DISPLAY_COPYRIGHT", '0')

if not os.path.exists("mergesac2"):
    os.mkdir("mergesac2")

if len(sys.argv) != 2:
    sys.exit("python %s dirname" % sys.argv[0])

dir = sys.argv[1]

os.chdir(dir)

# ����dict��key�Ĳ����ظ��Թ�������:
#     dict��key����Ϊ NET.STA.LOC.CHN
#     dict��value���ļ�����keyƥ���SAC�ļ���Ŀ
sets = {}
for fname in glob.glob("*.*.*.CFT.*.SAC"):
    #net, sta, loc, chn = fname.split('.')[6:10]
    #key = '.'.join([net, sta, loc, chn])
    year, days, hour, mint, sec, msac, net, sta, loc, chn = fname.split('.')[0:10]
    key = '.'.join([days, '*', net, sta, loc, chn])
    if key not in sets:
        sets[key] = 1
    else:
        sets[key] += 1

p = subprocess.Popen(['sac'], stdin=subprocess.PIPE)
s = "wild echo off \n"
to_del = []
for key, value in sets.items():
    # ����value����1ʱ����Ҫmerge
    if value == 1:
        continue

    print("merge %s: %d traces" % (key, value))
    # Python��glob����ֵ������ģ��������sort
    traces = sorted(glob.glob('.'.join(['*', key, '?', 'SAC'])))

    # ��SAC��ʹ��ͨ���������ʹ��@traces�Ա��������й���������
    # merge��֧��ͨ���
    s += "r *.%s.?.SAC \n" % key    # SAC v101.6 or later
    s += "merge g z o a \n"
    #s += "w %s \n" % traces[0]      # ���������ݶε��ļ�������
    s += "w 1.SAC \n"
    s += "mv 1.SAC ../mergesac2/%s \n" % traces[0]
    #s += "mv 1.SAC ../mergesac2/ \n"
    s += "message '%s done' \n" % traces[0]
    
    #to_del.extend(traces[1:])

s += "q \n"
p.communicate(s.encode())

# ɾ����������ݶ�
#for file in to_del:
#    os.unlink(file)

os.chdir("..")