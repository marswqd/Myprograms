%�Ƚ�̨վ��γ�ȣ���̨վ������

%% ��̨վ�����У��������ļ�
A=importdata('1.txt'); a=A.textdata(:,1);b=A.data(:,1);c=A.data(:,2);
[a_s,index]=sort(a); a_s=char(a_s);b_s=b(index);c_s=c(index);  %����̨վ��
id=fopen('2.txt','w');n= size(a,1);id2=fopen('3.txt','w');
for i=1:n
    flag=0;
    for j=i+1:n
        db=abs(b_s(i,1)-b_s(j,1));dc=abs(c_s(i,1)-c_s(j,1));
        if db<0.01 && dc<0.01  && ~strcmp(a_s(i,1:end),a_s(j,1:end)) %��γ����ͬ��̨վ����ͬ
            fprintf(id2,'%s   %f   %f\n',a_s(i,1:end),b_s(i,1),c_s(i,1));
            fprintf(id2,'%s   %f   %f\n\n',a_s(j,1:end),b_s(j,1),c_s(j,1));
        end
        if strcmp(a_s(i,1:end),a_s(j,1:end)) && (db>0.01 || dc>0.01)  %̨վ����ͬ����γ�Ȳ�ͬ
            fprintf(id2,'%s   %f   %f\n',a_s(i,1:end),b_s(i,1),c_s(i,1));
            fprintf(id2,'%s   %f   %f\n\n',a_s(j,1:end),b_s(j,1),c_s(j,1));
        end
        if strcmp(a_s(i,1:end),a_s(j,1:end)) && db<0.01 && dc<0.01 %̨վ����ͬ����γ����ͬ      
            flag=1;
        end
    end
    if flag==0
        fprintf(id,'%s   %f   %f\n',a_s(i,1:end),b_s(i,1),c_s(i,1));
    end  
end
fclose(id);fclose(id2);


