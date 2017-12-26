#!/bin/perl
use strict;
use warnings;

#mkdir '../seedsac',0755

my @sacN=glob("P.*.N.SAC");
my @sacE=glob("P.*.E.SAC");
our $nN=$#sacN;
our $nE=$#sacE;
if($nN ne $nE){
 print "The number of files of N and E are not equal, please check!";
 print "N files:",$nN+1;
 print "E files:",$nE+1;
 system(pause)
}
print "The number of files:",$nN+1;

$ENV{SAC_DISPLAY_COPYRIGHT}=0;
open(SAC,"| sac") or die "Error opening sac\n";
#print SAC "echo on \n";
foreach (0..$nN){
  my $N=$sacN[$_];
  my $pos=index($N,".N.SAC");
  my $name=substr($N,0,$pos) ;
  my $E=$name."E.SAC"
  my $R=$name."R.SAC"
  my $T=$name."T.SAC"
  print SAC "r $N $E  \n";
  print SAC "ch file 1 CMPAZ 0 KCMPNM N \n";
  print SAC "ch file 2 CMPAZ 90 KCMPNM E \n";
  print SAC "wh \n";
  print SAC "rot to gcp \n";
  print SAC "w $R $T \n";
  print SAC "r $R $T \n";
  print SAC "ch file 1 KCMPNM R \n";
  print SAC "ch file 2 KCMPNM T \n";
  print SAC "wh \n";
  #print SAC "sc mv $files[$_] ../seedsac/ \n";
  print $_+1,"\t",$nN-$_;
  print "\n";
}
print SAC "quit \n";
close(SAC);
