#!/usr/bin/env perl
use strict;
use warnings;
$ENV{SAC_DISPLAY_COPYRIGHT}=0;

mkdir "mergesac",0755;

@ARGV == 1 or die "Usage: perl $0 dirname\n";
my ($dir) = @ARGV;

chdir $dir;

# 利用hash的key的不可重复性构建集合:
#   hash的key定义为 NET.STA.LOC.CHN
#   hash的value是文件名与key匹配的SAC文件数目
my %sets;
foreach (glob "*.SAC") {
    # 将文件名用'.'分割，并取其中的第7到10个元素
    #my ($net, $sta, $loc, $chn) = (split /\./)[6..9];
    #$sets{"$net.$sta.$loc.$chn"}++;
	#2014.365.15.13.07.6600.YN.ZAT.00.BHZ.D.SAC
    my ($year, $days, $hour, $mint, $sec, $msac, $net, $sta, $loc, $chn) = (split /\./)[0..9];
    $sets{"$days.*.$net.$sta.$loc.$chn"}++;		
}

# 在循环体外部调用SAC，若没有数据需要merge，则此次调用是做无用工
# 若将SAC调用放在循环体内，则可能会多次调用SAC，而影响运行效率
open(SAC, "|sac") or die "Error in opening sac\n";
print SAC "wild echo off \n";
#my @to_del;
while (my ($key, $value) = each %sets) {
    
    next if $value == 1;# 检查每个key所对应的value，只有value大于1时才需要merge

    print STDERR "merge $key: $value traces\n";
    my @traces = sort glob "*.$key.?.SAC"; # 对glob的返回值做sort，以保证第一个元素是最早的数据段
    
    print SAC "r *.$key.?.SAC \n"; # 在SAC中使用通配符而不是使用@traces以避免命令行过长的问题
    print SAC "merge g z o a \n";  # merge不支持通配符,SAC v101.6 or later only
    #print SAC "w $traces[0] \n";  # 以最早数据段的文件名保存	
	print SAC "w 1.SAC \n";
	print SAC "mv 1.SAC ../mergesac/$traces[0] \n"; # 以最早数据段的文件名保存
	#print SAC "mv *.$key.?.SAC done/ \n"; # 将处理完的文件移动到别处
    print SAC "message '$traces[0] done' \n";
    
    # 将要删除的数据段文件名保存到数组中，待退出SAC后再删除
    #push @to_del, splice(@traces, 1);
}
print SAC "q \n";
close(SAC);
#unlink(@to_del);

chdir "..";
#chdir "..";