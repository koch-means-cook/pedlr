f=figure;
c=1;
j= 1;
for val = [20,80]
    R =[];
    R = (100 - val);
    R(2) = val;
    R(3) = val;
    for i = 4:10
        R(i) = (100-val);
    end
    out=[];
    out.R =R;
    out.Q = 80;
    a0v = [0.2:0.2:0.8];
    a1v = [0:0.1:1];
    labels ={};
    for i = 1:numel(a1v)
        labels{i} = [num2str(a1v(i))];
    end

    for a0 = a0v
        subplot(2,numel(a0v), j);
        d =[];
        for a1 = a1v
            id= find(a1v==a1);
            p.al0 = a0;
            p.al1 = a1;
            out.Q = 100-val;
            [out]  = pedlr_model_nochoice(p, out);
            plot(1:length(out.R), out.Q(1:length(out.R)), 'Color', [0.3 0.3 0.05+id*0.08], 'LineWidth', 2);
            hold on 
        end
        title(['$\alpha_0=' num2str(a0) '$'], 'Interpreter', 'Latex');
        ylim([0 100]);
        if find(a0v==a0) == length(a0v)
            legend(labels, 'location', 'southeast')
        end
        j = j+1;
    end
    f.Position(3) = 1000;
    f.Position(4) = 600;
    c=c+1;
end