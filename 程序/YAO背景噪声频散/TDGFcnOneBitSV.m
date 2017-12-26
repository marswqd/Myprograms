%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Extracting Green's function for the one-bit cross correlation of the
% ambient seismic noise. Main function Program: GreenFcn_Main
% Subroutine functions: GlobalParaDefine, Rd_Sac_ASC, Rd_InstruRespFile
% Rm_Instr_Response_BandPassFiltering, CrossCorrelation.
% In this version, we can only do one-bit cross-correlation using all
% waveform data. Before using this code, please read the main function
% GreenFcn_Main very carefully. The only parameter file is DataFile which
% contains the location of all waveform data (SAC_ASC format) and response
% data. The output file names GFcnfilename & CFcnfilename MUST be modified
% according to your original input data file which has the station
% name.
% - by Huajian Yao, 2007 Jan, MIT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function GreenFcn_Main
global sta src resp filter

%---------Define the value of essential parameters in the programs---------
%__________________________________________________________________________
DataFile = '01DataFile.dat';   % file containing the name of waveform data (SAC_ASC format) and response data
StartT = 10;         % Lowest period (s) of the GFcn
EndT = 20;            % Highest period (s) of the GFcn, i.e., the period band of the GFcn is [StartT EndT]
GroupVMin = 1.5       % the minimum group velocity for the recovered Empirical Green's function
%__________________________________________________________________________

% function call: define global parameter structure
%%#[sta, src, resp, filter] = GlobalParaDefine;
[sta, src, filter] = GlobalParaDefine;
seisfile = cell(1,1);
%%# respfile = cell(1,1);
% read data and response file
fdf = fopen(DataFile,'r');
kk = 1;
while feof(fdf) ~= 1
    seisfile{kk} = fscanf(fdf, '%s', 1);
    %%# respfile{kk} = fscanf(fdf, '%s', 1);
    kk = kk + 1;
end
seisfile
FileNum = kk - 1;  % the number of data files


% cross-correlation for every combination of two stations in DataFile
for i = 1
    for j = 7
        % combine folder and data/response filename
        seisfile1 = seisfile{i}
        seisfile2 = seisfile{j}
       %%# respfile1 = respfile{i};
        %%# respfile2 = respfile{j};
        
        % function call : read two stations asc_asc file
        sta(1) = Rd_Sac_ASC(seisfile1);
        sta(2) = Rd_Sac_ASC(seisfile2);
        
        % function call : read and remove instrument response; also band-pass filtering the wave trains
%%#        Rm_Instr_Response_BandPassFiltering(respfile1, respfile2, StartT, EndT);
                
        % function call : one-bit cross-correlation 
        [GFcn, CFcn] = CrossCorrelation(StartT, EndT, GroupVMin);
        
        n1 = length(seisfile1); % number of characters of seisfile1
        n2 = length(seisfile2); % number of characters of seisfile2
        
        %---- PLEASE MODIFY THIS PART FOR OUTPUT THE GFcn AND CFcn ----%
        %%% YOU HAVE TO FIRST CREATE TWO FOLDERS (e.g., 04AugGFs and 04AugCFs)
        %%% WHICH CONTAIN GFcn AND CFcn BEFORE YOU RUN THIS CODE
        % save GFcn to data file GFcnfilename,  GFcnfilename should be modified according to the input seisfile1 and seisfile2 name        
        GFcnfilename = strcat('./GF/GFcn.', seisfile1(10:12),  '-', seisfile2(10:12), '_10-20s.dat')
        % save GFcn to data file GFcnfilename,  GFcnfilename should be modified according to the input seisfile1 and seisfile2 name   
        CFcnfilename = strcat('./CF/CFcn.', seisfile1(10:12),  '-', seisfile2(10:12), '_10-20s.dat');
        %-----------------------END OF MODIFICATION--------------------%
        
        stalonlat = [sta(1).Lon sta(1).Lat ; sta(2).Lon sta(2).Lat]; % stations longitude and latitude
        save(GFcnfilename, 'stalonlat', '-ASCII');    % save Stations Lat and Lon to GFcn data file
        save(GFcnfilename, 'GFcn', '-ASCII', '-APPEND');    % save calculated GFcns (two sides) to CFcn data file
        save(CFcnfilename, 'stalonlat', '-ASCII');    % save Stations Lat and Lon to CFcn data file
        save(CFcnfilename, 'CFcn', '-ASCII', '-APPEND');    % save calculated CFcns (two sides) to CFcn data file
        
    end
end
clear all


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% define global parameter structure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%# function [sta, src, resp, filter] = GlobalParaDefine
function [sta, src, filter] = GlobalParaDefine
%%# global sta src resp filter
global sta src  filter
StationInfo = struct('Lon',0,...
	'Lat',0,...
	'GCDkm',0,...
	'GCDdeg',0,...
	'Azim',0,...
	'YY',0,...
	'DD',0,...
	'HH',0,...
	'MM',0,...
	'SS',0,...
	'MS',0,...
	'DiffT',0,...
    'NumPt',0,...
	'SampleT',0,...
	'SampleF',0,...
    'Amp',0,...
    'wave',zeros(1,100));

% define source information
SourceInfo = struct('YY',0,...
    'DD',0,...
    'HH',0,...
	'MM',0,...
	'SS',0,...
	'MS',0,...
    'Lon',0,...
    'Lat',0);

% define instrument response information
RespInfo = struct('Amp',0,...
    'NumZeros',0,...
    'NumPoles',0,...
    'Zeros',0,...
    'Poles',0,...
    'poly_num',0,...
    'poly_den',0);

% define filter parameter
FilterInfo = struct('CtrT',0,...   
    'LowT',0,...
    'HighT',0,...
    'CtrF',0,...
    'LowF',0,...
    'HighF',0,...
    'Length',0,...
    'KaiserPara',0,...
    'data',zeros(1,100));

sta = struct(StationInfo);
src = struct(SourceInfo);
%%# resp = struct(RespInfo);
filter = struct(FilterInfo);



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% read sac ascii format seismic data and remove the zero offset
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function station = Rd_Sac_ASC(seisfile)

StationInfo = struct('Lon',0,...
	'Lat',0,...
	'GCDkm',0,...
	'GCDdeg',0,...
	'Azim',0,...
	'YY',0,...
	'DD',0,...
	'HH',0,...
	'MM',0,...
	'SS',0,...
	'MS',0,...
	'DiffT',0,...
    'NumPt',0,...
	'SampleT',0,...
	'SampleF',0,...
    'Amp',0,...
    'wave',zeros(1,100));

station = struct(StationInfo);

fname = fopen(seisfile,'r');

% read line 1
station.SampleT = fscanf(fname,'%f',1);
station.SampleF = 1/station.SampleT;
temp = fscanf(fname, '%f', 2);
station.Amp = fscanf(fname, '%e', 1);
temp = fgetl(fname);

% read line 2
temp = fscanf(fname, '%d', 2);
station.DiffT = fscanf(fname, '%f', 1);
temp1 = fgetl(fname);

% skip 3 - 6 line
for i = 1:4
	temp1 = fgetl(fname);
end

% read 7 line
temp2 = fscanf(fname,'%f',1);
station.Lat = fscanf(fname,'%f',1);
station.Lon = fscanf(fname,'%f',1);
temp1 = fgetl(fname);

% read 8 line
sourceLat = fscanf(fname,'%f',1);
sourceLon = fscanf(fname,'%f',1);
temp1 = fgetl(fname);

% skip 9 - 10 line
for i = 1:2
	temp1 = fgetl(fname);
end

% read 11 line
station.GCDkm = fscanf(fname,'%f',1);
station.Azim = fscanf(fname,'%f',1);
temp2 = fscanf(fname,'%f',1);
station.GCDdeg = fscanf(fname,'%f',1);	
temp1 = fgetl(fname);

% skip 12 - 14 line
for i = 1:3
	temp1 = fgetl(fname);
end

% read the time of recording the seismic data ( 15 16 line )
station.YY = fscanf(fname,'%d',1);
station.DD = fscanf(fname,'%d',1);
station.HH = fscanf(fname,'%d',1);
station.MM = fscanf(fname,'%d',1);
station.SS = fscanf(fname,'%d',1);
station.MS = fscanf(fname,'%d',1);
temp = fscanf(fname, '%d', 3);
station.NumPt = fscanf(fname,'%d',1);	
temp1 = fgetl(fname);

% skip 17 - 23
for i =1:14
   temp1 = fgetl(fname);
end

% read seismic data file, NumSeisPoint is the total point of seismic data
station.wave = fscanf(fname,'%f',inf);  
station.NumPt = length(station.wave);
fclose(fname);

% remove the zero offset of the seimic wave records
SegNum = 0;
SegLength = round(2000/station.SampleT);
if mod(SegLength,2)~=0
    SegLength = SegLength + 1;
end
HalfSegLen = round(SegLength/2);
SegNum = floor(station.NumPt/SegLength);
if SegNum > 0
    for k = 0:(SegNum-1);
        SegPt1 = 1 + k*SegLength;
        SegPt2 = (k + 1)*SegLength; 
        offset = mean(station.wave(SegPt1:SegPt2));
        station.wave(SegPt1:SegPt2) = station.wave(SegPt1:SegPt2) - offset;  % remove mean
        offset1 = mean(station.wave(SegPt1:(SegPt1+HalfSegLen-1)));
        offset2 = mean(station.wave((SegPt1+HalfSegLen):SegPt2));
        Slope = (offset2 - offset1)/HalfSegLen;
        station.wave(SegPt1:SegPt2) = station.wave(SegPt1:SegPt2) - ((-HalfSegLen):(HalfSegLen-1))'*Slope; % remove trend
    end
    if station.NumPt > SegNum*SegLength
        offset = mean(station.wave((1 + SegNum*SegLength):station.NumPt));
        station.wave((1 + SegNum*SegLength):station.NumPt) = station.wave((1 + SegNum*SegLength):station.NumPt) - offset;
    end
else
    station.wave(1:station.NumPt) = station.wave(1:station.NumPt) - mean(station.wave(1:station.NumPt));
end                                                     %???????????????

station

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% --- Read Instrument Response File and Bass Pass Filtering the seismogram ---%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%#function [Normfactor, Numzeroes, Numpoles, Respzero, Resppole] = Rd_InstruRespFile(RespFile)

%%#fname = fopen(RespFile,'r');
% skip 1 - 18 line
%%#for i = 1:18
%%#temp1 = fgetl(fname);
%%#end

%read line 19
%%#temp1 = fscanf(fname,'%s',4);
%%#Normfactor = fscanf(fname,'%f',1);
%%#temp1 = fgetl(fname);

%read line 20
%%#temp1 = fgetl(fname);
%read line 21
%%#temp1 = fscanf(fname,'%s',4);
%%#Numzeroes = fscanf(fname,'%f',1);
%%#temp1 = fgetl(fname);
%read line 22
%%#temp1 = fscanf(fname,'%s',4);
%%#Numpoles = fscanf(fname,'%f',1);
%%#temp1 = fgetl(fname);
%read line 23, 24: zeroes header
%%#temp1 = fgetl(fname);
%%#temp1 = fgetl(fname);
%read zeros
%%#for i = 1:Numzeroes
   %%#temp1 = fscanf(fname,'%s',1);
   %%#temp = fscanf(fname,'%d',1);
   %%#realpart = fscanf(fname,'%e',1);
   %%#imagpart = fscanf(fname,'%e',1);
   %%#Respzero(i) = complex(realpart, imagpart);
   %%#temp1 = fgetl(fname);
%%#end
%read 2 lines: poles header
%%#temp1 = fgetl(fname);
%%#temp1 = fgetl(fname);
%read poles
%%#for i = 1:Numpoles
   %%#temp1 = fscanf(fname,'%s',1);
   %%#temp = fscanf(fname,'%d',1);
   %%#realpart = fscanf(fname,'%e',1);
   %%#imagpart = fscanf(fname,'%e',1);
   %%#Resppole(i) = complex(realpart, imagpart);
   %%#temp1 = fgetl(fname);
%%#end
%%#fclose(fname);



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% remove instrument responses for two waves and band-pass filter the wave trains
% in the period band [StartT EndT]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%#function Rm_Instr_Response_BandPassFiltering(respfile1, respfile2, StartT, EndT)
function Rm_Instr_Response_BandPassFiltering(StartT, EndT)
%%# global sta resp
global sta 
%read instrument response files
%%#[resp(1).Amp, resp(1).Numzeroes, resp(1).Numpoles, resp(1).Zeros, resp(1).Poles] = Rd_InstruRespFile(respfile1);
%%#[resp(1).poly_num, resp(1).poly_den] = zp2tf(resp(1).Zeros', resp(1).Poles',resp(1).Amp);  
%%% help zp2tf

%%#[resp(2).Amp, resp(2).Numzeroes, resp(2).Numpoles, resp(2).Zeros, resp(2).Poles] = Rd_InstruRespFile(respfile2);
%%#[resp(2).poly_num, resp(2).poly_den] = zp2tf(resp(2).Zeros', resp(2).Poles', resp(2).Amp);

%remove instrument response for the waves
%highest period kept in the wave after instrument response removal
TMax = 100;
LowFMin = 1/TMax;

% band pass filter [LowF HighF]
LowF = 1/EndT;
HighF = 1/StartT;

SegLength = 2^16;
fftnum = zeros(1,2);
for i = 1:2
    fftnum(i) = floor(sta(i).NumPt/SegLength);
end

while min(fftnum) == 0
    SegLength = SegLength/2;
    for i = 1:2
        fftnum(i) = floor(sta(i).NumPt/SegLength);
    end
end    

for i = 1:2    
    if mod(sta(i).NumPt,2) == 1
        sta(i).NumPt = sta(i).NumPt - 1;
    end
    
    for k = 0:(fftnum(i)-1)
        if k ~= (fftnum(i)-1)
            fftdata = fft(sta(i).wave((1 + k*SegLength):(k + 1)*SegLength), SegLength);
            fftlength = SegLength;
        else
            fftdata = fft(sta(i).wave((1 + k*SegLength):sta(i).NumPt), sta(i).NumPt - k*SegLength);
            fftlength = sta(i).NumPt - k*SegLength;
        end
        
        f(1:(fftlength/2+1)) = sta(i).SampleF*(0:(fftlength/2))/fftlength;%?????????????????
        delta_f = sta(i).SampleF/fftlength;
        w(1:(fftlength/2+1)) = 2*pi*f(1:(fftlength/2+1));
        %%# h = freqs(resp(i).poly_num, resp(i).poly_den, w); 
        %%#MinFPoint = floor(LowFMin/delta_f);
        %%#if  MinFPoint <= 2
           %%# MinFPoint = 2;
        %%#end    
        
        % remove instrument response
       %%# for n = MinFPoint:(fftlength/2+1)
          %%#  fftdata(n) = fftdata(n)*conj(h(n)/abs(h(n)));  % only correct phase shift in this case
            %%#fftdata(fftlength+2-n) = conj(fftdata(n));
        %%#end        
        %%#clear n
        %%#for n = (MinFPoint-25):(MinFPoint-1)
           %%# fftdata(n) = exp(-(MinFPoint-n)^2/25)*fftdata(n)*conj(h(n)/abs(h(n)));
        %%#end
        %%#clear n
        %%#fftdata(1:(MinFPoint-26))=0;
 
        % band pass filtering in frequency band [LowF HighF]
        LowPtN = round(LowF/delta_f);
        HighPtN = round(HighF/delta_f); 
        fftdata(1:(LowPtN - 26))=0;
        for n = (LowPtN - 25):(LowPtN-1)
            fftdata(n)=exp(-(LowPtN-n)^2/25)*fftdata(n);
        end
        for n = (HighPtN + 1):(HighPtN + 25)
            fftdata(n) = exp(-(HighPtN-n)^2/25)*fftdata(n);
        end
        fftdata((HighPtN + 26):(round(fftlength/2)+1)) = 0;
        
        n = 2:(round(fftlength/2)+1);
        fftdata(fftlength+2-n) = conj(fftdata(n));
        clear n
        
        if  k ~= (fftnum(i)-1)
            sta(i).wave((1 + k*SegLength):(k + 1)*SegLength) = real(ifft(fftdata,fftlength));
        else
            sta(i).wave((1 + k*SegLength):sta(i).NumPt) = real(ifft(fftdata,fftlength));
        end
        
    end
    
end

'Instrument reponse removal and bass-pass filtering finished !'

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Onebit cross-correlation 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [GFcn, CFcn] = CrossCorrelation(StartT, EndT, GroupVMin)

global sta filter
MonthDays = [31 29 31 30 31 30 31 31 30 31 30 31; 31 28 31 30 31 30 31 31 30 31 30 31];

% one bit cross correlation
SampleF = round(sta(1).SampleF);
if SampleF == 0
    error('Error! Sampling Frequency of data should not be zero!');
end

% if the first point of the two wain trains start at differnet year (e.g., 2003, 2004)
% Here we only deal with the case of one year difference
if (sta(2).YY - sta(1).YY) == 1
    if mod(sta(1).YY, 4) == 0  
        YearDays = 366;
    else
        YearDays = 365;  
    end            
    sta(2).DD = sta(2).DD + YearDays;
elseif (sta(1).YY - sta(2).YY) == 1  
    if mod(sta(2).YY, 4) == 0  
        YearDays = 366;
    else
        YearDays = 365;  
    end            
    sta(1).DD = sta(1).DD + YearDays;
elseif abs(sta(2).YY - sta(1).YY) > 1
    'Year Error! '
    exit
end

DeltaTInitial = (sta(2).DD - sta(1).DD)*24*3600 + (sta(2).HH - sta(1).HH)*3600 + (sta(2).MM - sta(1).MM)*60; 
DeltaTInitial = DeltaTInitial + (sta(2).SS - sta(1).SS) + (sta(2).MS - sta(1).MS)/1000; 
DeltaTInitial = round(DeltaTInitial/sta(1).SampleT);

% let the first point of the two wave trains start at same time (the later one)
if DeltaTInitial > 0
    if (sta(1).NumPt-DeltaTInitial) > 0
        sta(1).wave(1:(sta(1).NumPt-DeltaTInitial)) = sta(1).wave((DeltaTInitial+1):sta(1).NumPt);
        sta(1).wave((sta(1).NumPt-DeltaTInitial+1):sta(1).NumPt) = 0;    
    else
        sta(1).wave(1:sta(1).NumPt) = 0;
    end
    DeltaTInitial = 0;
elseif DeltaTInitial < 0
    DeltaTInitial = abs(DeltaTInitial);
    if (sta(2).NumPt-DeltaTInitial) > 0
        sta(2).wave(1:(sta(2).NumPt-DeltaTInitial)) = sta(2).wave((DeltaTInitial+1):sta(2).NumPt);
        sta(2).wave((sta(2).NumPt-DeltaTInitial+1):sta(2).NumPt) = 0;
    else
        sta(2).wave(1:sta(2).NumPt) = 0;
    end
    DeltaTInitial = 0;
end

cross.StartNum = 1;
cross.EndNum = min(sta(1).NumPt, sta(2).NumPt);
cross.PointNum = cross.EndNum - cross.StartNum + 1;
cross.VMin = GroupVMin;

StaDistance = deg2km(distance(sta(1).Lat,sta(1).Lon,sta(2).Lat,sta(2).Lon))

MaxTravT = round(StaDistance/cross.VMin)
MinTravT = - MaxTravT;
MaxShiftNum = round(MaxTravT/sta(1).SampleT);
MinShiftNum = round(MinTravT/sta(1).SampleT);
ShiftNum = MaxShiftNum - MinShiftNum + 1    % also ShiftNum = 2*MaxTravT*SampleF + 1
ShiftPtT = zeros(1,ShiftNum);
for i = 1:ShiftNum
    ShiftPtT(i) = round((MinShiftNum + i - 1)*sta(1).SampleT);
end


% one-bit of the waveform
for k = 1:2
    seglength = 10^5;
    segnum = floor(cross.EndNum/seglength);
    if segnum == 0
        sta(k).wave(1:cross.EndNum) = sign(sta(k).wave(1:cross.EndNum));
    else
        for i = 1:(segnum-1)
            sta(k).wave((1+(i-1)*seglength):(i*seglength)) = sign(sta(k).wave((1+(i-1)*seglength):(i*seglength)));
        end
        sta(k).wave(((segnum-1)*seglength+1):cross.EndNum) = sign(sta(k).wave(((segnum-1)*seglength+1):cross.EndNum));
    end
end


% one-bit cross-correlation
OneBitCross = zeros(1, ShiftNum);
for k = 1:ShiftNum
    shift = MinShiftNum + k - 1;
    OneBitCross(k) = 0;
    if shift >= 0
        OneBitCross(k) = dot(sta(2).wave((1 + shift):cross.PointNum), sta(1).wave(1:(cross.PointNum - shift)));        
    else
        OneBitCross(k) = dot(sta(2).wave(1:(cross.PointNum + shift)), sta(1).wave((1 - shift):cross.PointNum));
    end   
end

 % band-pass filter the correlation function in [filter.LowF, filter.HighF]
filter.LowF = (2/SampleF)/EndT;
filter.HighF = (2/SampleF)/StartT;
[B, A] = butter(4,[filter.LowF, filter.HighF]);
wave = OneBitCross;
OneBitCross = filtfilt(B, A, wave);

% Differentiate the correlation function to get the Green's fcn
GreenFcn = zeros(ShiftNum,1);

for i = 2:(ShiftNum-1)
    GreenFcn(i) = (OneBitCross(i+1) - OneBitCross(i-1))/2;
end
GreenFcn(1) = OneBitCross(2) - OneBitCross(1);
GreenFcn(ShiftNum) = OneBitCross(ShiftNum) - OneBitCross(ShiftNum - 1);
if max(GreenFcn>0)
    GreenFcn = GreenFcn/max(GreenFcn);
end
GFcn = zeros(MaxTravT*SampleF + 1, 3);
GFcn(:, 1) = (0:1:MaxTravT)'/SampleF;
GFcn(:, 2) = - GreenFcn((MaxTravT*SampleF+1):ShiftNum);
GFcn(:, 3) = GreenFcn((MaxTravT*SampleF+1):-1:1);

CFcn = zeros(MaxTravT*SampleF + 1, 3);
CFcn(:, 1) = (0:1:MaxTravT)'/SampleF;
CFcn(:, 2) = OneBitCross((MaxTravT*SampleF+1):ShiftNum);
CFcn(:, 3) = OneBitCross((MaxTravT*SampleF+1):-1:1);
