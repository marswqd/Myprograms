#!/usr/bin/env perl
use strict;
use warnings;

@ARGV == 1 or die "Usage: perl $0 dirname\n";
my ($dir) = @ARGV;

chdir $dir;

# 利用hash的key的不可重复性构建集合:
#   hash的key定义为 NET.STA.LOC.CH (分量信息不是BHZ而是BH)
#   hash的value是文件名与key匹配的SAC文件数目，正常情况下是3的整数倍
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
