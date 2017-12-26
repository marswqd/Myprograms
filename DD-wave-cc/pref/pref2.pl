#sac宏perl脚本文件。功能：数据预处理-去线性趋势、去均值、带通滤波
#!/bin/perl
use strict;
use warnings;


#my @dirs=qw( 2009mdp 2010mdp 2011mdp 2012mdp 2013mdp 2014mdp );
my @dirs=qw( mdp );
foreach my $dir (@dirs){
chdir $dir;
################
mkdir 'done',0755;
$ENV{SAC_DISPLAY_COPYRIGHT}=0;
our $f1=0.02;
our $f2=0.025;
our $f3=40;
our $f4=45;
my @files=glob("*.SAC");
our $n=$#files;
open(SAC,"| sac") or die "Error opening sac\n";
#print SAC "echo on \n";
foreach (0..$n){
  print SAC "r $files[$_] \n";
  print SAC "rtr \n";
  print SAC "rmean \n";
  print SAC "taper \n";
  print SAC "transfer from none to none freqlimits $f1 $f2 $f3 $f4 \n";
  print SAC "w over \n";
  #print SAC "w prepend P. \n";
  print SAC "sc mv $files[$_] done/ \n";
  print $_+1,"\t",$n-$_;
  print "\n";
}
print SAC "quit \n";
close(SAC);
################
chdir "../";
}
#mkdir '../seedsac',0755

 
