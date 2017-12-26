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
foreach my $file (glob "?[!.]*.?.SAC") {     
    my @name = split /\./,$file;  #STA.Time.Z.SAC
    my $eve =$name[1];
    $sets{"$eve"}++;
    print "$eve \n";
    #my (undef, $sta, $stla, $stlo, $stel) = split " ", `saclst kstnm stla stlo stel f $file`;  
}

open(AAA, "> ../eve.txt") or die "Error in opening evefile \n";
foreach my $key (sort keys %sets) {
    my @traces = sort glob "*?[!.]*.?.SAC";
    my (undef, $evla, $evlo, $evdp, $mag) = split " ", `saclst evla evlo evdp mag f $traces[0]`;
    my $id = `saclhdr -NXSIZE $traces[0]`;
    printf AAA "%-14s %-10s %-10s %-10s %-4s %d \n",$key,$evla,$evlo,$evdp,$mag,$id;
}
close(AAA);
