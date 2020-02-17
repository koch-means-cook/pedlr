function[out]  = pedlr_model_nochoice(p, out)
P = [];
al = [];
for i = 1:size(out.R,2)

    al(i,1) = p.al0 + (1-p.al0)*(p.al1)* abs(out.R(i) - out.Q(i))/100;
    out.Q(i+1) = out.Q(i) + al(i,1)*(out.R(i) - out.Q(i)); 
  
end
out.al = al;


