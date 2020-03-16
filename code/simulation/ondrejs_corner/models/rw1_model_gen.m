function[out]  = rw1_model_gen(p, out)
P = [];
ch  =[]; %(choice)
al = [];
if strcmp(class(p), 'double')
    a =p;
    p=[];
    p.al = a(1);
    p.beta = a(2);
end
for i = 1:size(out.sch,1)
    
    if out.sch(i,4) == 1 %forced choice
        ch(i,1) = 1;
        P(i,1) = exp(p.beta*out.Q(i,out.sch(i,1))/100) / (exp(p.beta*out.Q(i,out.sch(i,2))/100) + exp(p.beta*out.Q(i,out.sch(i,1))/100));
        P(i,2) = 1 - P(i,1);
    else % Choice based on values
        try 
        P(i,1) = exp(p.beta*out.Q(i,out.sch(i,1))/100) / (exp(p.beta*out.Q(i,out.sch(i,2))/100) + exp(p.beta*out.Q(i,out.sch(i,1))/100));
        catch 
            l = [];
        end
        P(i,2) = 1 - P(i,1);
        ch(i,1) = (-1)*binornd(1,P(i,1)) +2; % choice 1 or 2
    end
    
    % Only update chosen bandit
    try
        [chosen_b, out.chb(i)] = deal(out.sch(i,ch(i,1)));
    catch
        i =1;
    end
    
    %al(i,1) = p.pi*(p.al1) + (1-p.pi)*(p.al2)*(abs(out.R(i,chosen_b) - out.Q(i, chosen_b))/100);
    out.Q(i+1, chosen_b) = out.Q(i, chosen_b) +p.al*(out.R(i,chosen_b) - out.Q(i, chosen_b)); 
    out.r(i,1) = out.R(i,chosen_b);
    
    % For unchosen bandits pass forward previous value
    unchosen_bs = setdiff(1:out.ncues, chosen_b);
    out.Q(i+1,unchosen_bs) = out.Q(i,unchosen_bs);
    
    
end
out.P  = P;
out.ch = ch;
out.al = al;


