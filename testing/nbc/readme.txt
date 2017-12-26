本程序是由gfortran编译，生成的txt文件为linux格式，推荐用Notpad++或写字板打开

绘图功能需要库文件libcalpltf.a
其中libcalpltf-cyg.a是在CYGWIN环境下编译的；libcalpltf-ubuntu.a是在ubuntu环境下编译的
建议对特定的linux操作系统重新编译CPS3.30，使用其生成的libcalpltf.a
或者可以用matlab绘图，其信息都保存在-ST_C.txt和-T_C.txt中

需要注意的是：本程序的周期-速度(不等间隔)信息是直接由台站间距除以周期-走时(等间隔)信息得到的，
因而是不等间隔的，而绘制坐标轴时是按照等间隔绘制的，所以可能彩图(shade)和坐标轴不太对应，
不过应该影响不大，以后加入插值程序可以解决这一问题。

绘图命令
plotxvig < *.plt
转化为ps格式
plotnps -G < *.plt > *.ps
以上都为CPS3.30的内部命令
