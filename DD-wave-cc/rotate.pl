#!/usr/bin/env perl
use strict;
use warnings;
use List::Util qw(min max);
use File::Copy;

@ARGV == 1 or die "Usage: perl $0 dirname\n";
my ($dir) = @ARGV;

chdir $dir;

# ����hash��key�Ĳ����ظ��Թ�������:
#   hash��key����Ϊ NET.STA.LOC.CH (������Ϣ����BHZ����BH)
#   hash��value���ļ�����keyƥ���SAC�ļ���Ŀ�������������3��������
my %sets;
foreach (glob "*.SAC") {     
    # my ($net, $sta, $loc, $chn) = split /\./;
    # my $chn2 = substr $chn, 0, 2;
    # $sets{"$net.$sta.$loc.$chn2"}++;
    my ($sta, $time) = (split /\./)[0..1];# �����ļ���������ʽΪ STA.Time.[ZNE].SAC
    $sets{"$net.$time"}++;
}

open(SAC, "| sac") or die "Error in opening sac\n";
# �����е�key��ѭ��
mkdir "del",0755;
foreach my $key (keys %sets) {
    my ($E, $N, $Z, $R, $T, $Z0);

    # ���Z�����Ƿ����
    $Z = "${key}.Z.SAC";
    if (!-e $Z) {  # �������ڣ���ɾ����̨վ����������
        warn "Vertical component missing!\n";
        # unlink glob "$key.?.SAC";
        foreach my $file (glob "$key.?.SAC") {
            move $file,"del/$file";        
        }        
        next;
    }

    # ���ˮƽ�����Ƿ����
    if (-e "${key}.E.SAC" and -e "${key}.N.SAC") {   # E��N����
        $E = "${key}.E.SAC";
        $N = "${key}.N.SAC";
    } elsif (-e "${key}.1.SAC" and -e "${key}.2.SAC") {  # 1��2����
        $E = "${key}.1.SAC";
        $N = "${key}.2.SAC";
    } else {   # ˮƽ����ȱʧ
        warn "Horizontal components missing!\n";
        # unlink glob "$key?.SAC";
        foreach my $file (glob "$key.?.SAC") {
            move $file,"del/$file";        
        } 
        next;
    }

    # �ٶ�kzdate��kztime��ͬ
    # ���B, E, DELTA
    my (undef, $Zb, $Ze, $Zdelta) = split " ", `saclst b e delta f $Z`;
    my (undef, $Eb, $Ee, $Edelta) = split " ", `saclst b e delta f $E`;
    my (undef, $Nb, $Ne, $Ndelta) = split " ", `saclst b e delta f $N`;
    die "$key: delta not equal\n" if $Zdelta != $Edelta or $Zdelta != $Ndelta;

    # ��ȡ������������B����СEֵ��Ϊ���ݴ�
    # my $begin = max($Zb, $Eb, $Nb) + $Zdelta;
    # my $end = min($Ze, $Ee, $Ne) - $Zdelta;
    my $begin = max($Zb, $Eb, $Nb);
    my $end = min($Ze, $Ee, $Ne);    

    # ����ļ���Ϊ NET.STA.LOC.[RTZ]
    # my $prefix = substr $key, 0, length($key)-2;
    # $R = $prefix."R";
    # $T = $prefix."T";
    # $Z0 = $prefix."Z";
    $R = "${key}.R.SAC";
    $T = "${key}.T.SAC";
    $Z0 = "${key}.Z.SAC";

    print SAC "cut $begin $end \n";
    print SAC "r $E $N $Z\n";
    print SAC "w $E $N $Z\n";
    print SAC "r $E $N \n";
    print SAC "rotate to gcp \n";
    print SAC "w $R $T \n";
}
print SAC "q\n";
close(SAC);
# unlink glob "*.SAC";

chdir "..";