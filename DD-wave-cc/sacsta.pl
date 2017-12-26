#!/usr/bin/env perl
use strict;
use warnings;

@ARGV == 1 or die "Usage: perl $0 dirname\n";
my ($dir) = @ARGV;

chdir $dir;

# ����hash��key�Ĳ����ظ��Թ�������:
#   hash��key����Ϊ NET.STA.LOC.CH (������Ϣ����BHZ����BH)
#   hash��value���ļ�����keyƥ���SAC�ļ���Ŀ�������������3��������
my %sets;
foreach my $file (glob "*.SAC") {     
    my (undef, $sta, $stla, $stlo, $stel) = split " ", `saclst kstnm stla stlo stel f $file`;
    # my ($net, $sta, $loc, $chn) = split /\./;
    # my $chn2 = substr $chn, 0, 2;
    # $sets{"$net.$sta.$loc.$chn2"}++;
    $sets{"$sta+$stla+$stlo+$stel"}++;
}

open(AAA, "> ../station.txt") or die "Error in opening stafile \n";
foreach my $key (sort keys %sets) {
    my ($sta1, $stla1, $stlo1, $stel1) = split /\+/,$key;
    #print AAA "$sta1 \t$stla1\t$stlo1\t$stel1 \n";
    printf AAA "%-5s %-8s %-8s %-8s \n",$sta1,$stla1,$stlo1,$stel1;
}
close(AAA);
