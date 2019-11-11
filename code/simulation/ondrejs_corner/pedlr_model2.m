function[out]  = pedlr_model2(p, out)
P = [];
ch  =[]; %(choice)
al = [];
for i = 1:size(out.sch,1)
    
    if out.sch(i,4) == 1 %forced choice
        ch(i,1) = 1;
    else % Choice based on values
        P(i,1) = exp(p.beta*out.Q(i,out.sch(i,1))) / (exp(p.beta*out.Q(i,out.sch(i,2))) + exp(p.beta*out.Q(i,out.sch(i,1))));
        P(i,2) = 1 - P(i,1);
        ch(i,1) = (-1)*binornd(1,P(i,1)) +2; % choice 1 or 2
    end
    
    % Only update chosen bandit
    [chosen_b, out.chb(i)] = deal(out.sch(i,ch(i,1)));
    al(i,1) = p.al0 + (1-p.al0)*(p.al1)* abs(out.R(i,chosen_b) - out.Q(i, chosen_b))/100;
    out.Q(i+1, chosen_b) = out.Q(i, chosen_b) + al(i,1)*(out.R(i,chosen_b) - out.Q(i, chosen_b)); 
    
    % For unchosen bandits pass forward previous value
    unchosen_bs = setdiff(1:3, chosen_b);
    out.Q(i+1,unchosen_bs) = out.Q(i,unchosen_bs);
    
end
out.P  = P;
out.ch = ch;
out.al = al;


