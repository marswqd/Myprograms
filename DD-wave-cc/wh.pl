#!/bin/perl
use strict;
use warnings;

#mkdir '../seedsac',0755
$ENV{SAC_DISPLAY_COPYRIGHT}=0;
my @files=glob("*.SAC");
our $n=$#files;
open(SAC,"| sac") or die "Error opening sac\n";
#print SAC "echo on \n";
foreach (0..$n){
  print SAC "r $files[$_] \n";
  print SAC "wh \n";
  #print SAC "sc mv $files[$_] ../seedsac/ \n";
  print $_+1,"\t",$n-$_;
  print "\n";
}
print SAC "quit \n";
close(SAC);

