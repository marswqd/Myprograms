#!/usr/bin/env perl
use strict;
use warnings;
$ENV{SAC_DISPLAY_COPYRIGHT}=0;

mkdir "mergesac",0755;

@ARGV == 1 or die "Usage: perl $0 dirname\n";
my ($dir) = @ARGV;

chdir $dir;

# ����hash��key�Ĳ����ظ��Թ�������:
#   hash��key����Ϊ NET.STA.LOC.CHN
#   hash��value���ļ�����keyƥ���SAC�ļ���Ŀ
my %sets;
foreach (glob "*.SAC") {
    # ���ļ�����'.'�ָ��ȡ���еĵ�7��10��Ԫ��
    #my ($net, $sta, $loc, $chn) = (split /\./)[6..9];
    #$sets{"$net.$sta.$loc.$chn"}++;
	#2014.365.15.13.07.6600.YN.ZAT.00.BHZ.D.SAC
    my ($year, $days, $hour, $mint, $sec, $msac, $net, $sta, $loc, $chn) = (split /\./)[0..9];
    $sets{"$days.*.$net.$sta.$loc.$chn"}++;		
}

# ��ѭ�����ⲿ����SAC����û��������Ҫmerge����˴ε����������ù�
# ����SAC���÷���ѭ�����ڣ�����ܻ��ε���SAC����Ӱ������Ч��
open(SAC, "|sac") or die "Error in opening sac\n";
print SAC "wild echo off \n";
#my @to_del;
while (my ($key, $value) = each %sets) {
    
    next if $value == 1;# ���ÿ��key����Ӧ��value��ֻ��value����1ʱ����Ҫmerge

    print STDERR "merge $key: $value traces\n";
    my @traces = sort glob "*.$key.?.SAC"; # ��glob�ķ���ֵ��sort���Ա�֤��һ��Ԫ������������ݶ�
    
    print SAC "r *.$key.?.SAC \n"; # ��SAC��ʹ��ͨ���������ʹ��@traces�Ա��������й���������
    print SAC "merge g z o a \n";  # merge��֧��ͨ���,SAC v101.6 or later only
    #print SAC "w $traces[0] \n";  # ���������ݶε��ļ�������	
	print SAC "w 1.SAC \n";
	print SAC "mv 1.SAC ../mergesac/$traces[0] \n"; # ���������ݶε��ļ�������
	#print SAC "mv *.$key.?.SAC done/ \n"; # ����������ļ��ƶ�����
    print SAC "message '$traces[0] done' \n";
    
    # ��Ҫɾ�������ݶ��ļ������浽�����У����˳�SAC����ɾ��
    #push @to_del, splice(@traces, 1);
}
print SAC "q \n";
close(SAC);
#unlink(@to_del);

chdir "..";
#chdir "..";