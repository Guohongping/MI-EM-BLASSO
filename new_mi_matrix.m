function  mi1=new_mi(a,b)
mi1=ones(length(a),1);
for i=1:1:length(a)
    mi1(i) = mutualinfo(b,a(:,i));        
end 
mi1;
