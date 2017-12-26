#!bin/sh
#解压地震事件evt波形文件
#程序evt2sac为可自行文件,选在32位Linux下运行(Ubuntu64安装32位环境)

#EVT=(`ls *EVT`)
mkdir done
for i in *ext；do
 evt2sac ${i} && mv ${i} done/ || echo "error:" ${i}
done

# bash wh.sh
# perl wh.pl
