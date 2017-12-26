%Stack several green functions
function GFstack

%All Green functions' name are saved in gfname.txt
GfFile='gfname1.txt'
seisfile = cell(1,1);
% read green file name to seisfile
fdf = fopen(GfFile,'r');
kk = 1;
while feof(fdf) ~= 1
    seisfile{kk} = fscanf(fdf, '%s', 1);
    kk = kk + 1;
end
seisfile
FileNum = kk - 1;  % the number of data files

%read the data from green function data file then stack
seisfile1=seisfile{1};
gf1=RdGreenFcn(seisfile1);
for i = 2:FileNum
    seisfile2=seisfile{i};
    gf2=RdGreenFcn(seisfile2);
    gf1=StkGreenFcn(gf1,gf2);
end
fname='1.stk'
WtGreenFcn(fname,gf1);


%Read green function
function gfcn=RdGreenFcn(greenfcnfile)
GreenFcnInfo = struct('Lat1',0,...
    'Lon1',0,...
    'Lat2',0,...
    'Lon2',0,...
    'Time',zeros(1,1),...
    'GreenFcn',zeros(2,1),...
    'PtNum',0);
gfcn = struct(GreenFcnInfo);

fgfcn = fopen(greenfcnfile, 'r');
gfcn.Lon1 = fscanf(fgfcn, '%f', 1);
gfcn.Lat1 = fscanf(fgfcn, '%f', 1);
temp = fgetl(fgfcn);
gfcn.Lon2 = fscanf(fgfcn, '%f', 1);
gfcn.Lat2 = fscanf(fgfcn, '%f', 1);
temp = fgetl(fgfcn);
GreenFcn = fscanf(fgfcn,'%f',[3,inf]);
gfcn.PtNum = size(GreenFcn, 2);
gfcn.Time = GreenFcn(1,:);
maxamp = max(max(GreenFcn(2,:)),max(GreenFcn(3,:)));

if maxamp > 0
    GreenFcn(2,:) = GreenFcn(2,:)/maxamp;
    GreenFcn(3,:) = GreenFcn(3,:)/maxamp;
end

gfcn.GreenFcn = GreenFcn(2:3,:);
fclose(fgfcn);
gfcn
%Stack green function
function gfcns=StkGreenFcn(greenfcn1,greenfcn2)
GreenFcnInfo = struct('Lat1',0,...
    'Lon1',0,...
    'Lat2',0,...
    'Lon2',0,...
    'Time',zeros(1,1),...
    'GreenFcn',zeros(2,1),...
    'PtNum',0);
gfcns = struct(GreenFcnInfo);
gfcns.Lat1=greenfcn1.Lat1;
gfcns.Lon1=greenfcn1.Lon1;
gfcns.Lat2=greenfcn1.Lat2;
gfcns.Lon2=greenfcn1.Lon2;
gfcns.Time=greenfcn1.Time;
gfcns.GreenFcn=greenfcn1.GreenFcn(1:2,:)+greenfcn2.GreenFcn(1:2,:)
gfcns.PtNum=greenfcn1.PtNum;

% Write stack file to disk file
function WtGreenFcn(filename,gf)
stalonlat = [gf.Lon1 gf.Lat1 ; gf.Lon2 gf.Lat2];
GFcn=gf.GreenFcn;
save(filename, 'stalonlat', '-ASCII');
save(filename, 'GFcn','-ASCII','-APPEND');