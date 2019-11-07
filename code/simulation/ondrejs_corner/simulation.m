clear all
close all
iter = 40;


for t = 1:iter
    clear('out', 'C')
    % Generate distributions of rewards
    n = 10000;
    for i = 1:n
        C(1, i) = betarnd(1.66, 3.33)*100; 
        C(3, i) = betarnd(3.33, 1.66 )*100;
        C(2, i) = normrnd(50, 15);
    end

    % visually check distributions



    %%
    out = [];
    ntrls = 600; %should be 100 of each choice
    tr_types = nchoosek(1:3,2);
    tr_types = [tr_types; [tr_types(:,2) tr_types(:,1)]];
    out.sch = repmat(tr_types,100,1);

    %add force choice
    out.sch(1:120,4) = 1;
    out.sch(121:end,4) = 0;
    out.sch = out.sch(randperm(ntrls),:);

    %select 600 rewards from the 10k generated above
    C= C(:,randperm(n))';
    out.R = C(1:600,:);
    out.Q = [50 50 50];


    p.al0 = 0.1;
    p.al1 = 0.9;
    p.beta   = 5;
    out = pedlr_model(p, out);

    fi = 0;
    if fi 
        f=figure;
        subplot(1,4,[1 2])
        title('Reward Distributions');
        hold on 
        hist(out.R, 50)
        xlim([0 100]);

        subplot(1,4,[3 4])
        title('Model-estimated Distributions');
        hold on 
        hist(out.Q, 50)
        xlim([0 100]);
    end

    meanR(t,:)=mean(out.R);
    meanQ(t,:)=mean(out.Q); 
end
mR = mean(meanR);
mQ = mean(meanQ);
y_coord = [0 100];

f = figure;
subplot(3,2,1)
histogram(out.R(:,1), 'BinWidth', 2, 'Handlevisibility', 'Off')
hold on 
plot([mQ(1) mQ(1)], y_coord, 'r')
plot([mR(1) mR(1)], y_coord, 'b')
legend({'Mean value estimate', 'True mean'})
subplot(3,2,3)
histogram(meanQ(:,1), 'BinWidth', 0.5)
hold on 
histogram(meanR(:,1), 'BinWidth', 0.5)
legend({'Mean value estimate', 'True mean'})

subplot(3,2,2)
histogram(out.R(:,3), 'BinWidth', 2, 'Handlevisibility', 'Off')
hold on 
plot([mQ(3) mQ(3)], y_coord, 'r')
plot([mR(3) mR(3)], y_coord, 'b')
legend({'Mean value estimate', 'True mean'})
subplot(3,2,4)
histogram(meanQ(:,3), 'BinWidth', 0.5)
hold on 
histogram(meanR(:,3), 'BinWidth', 0.5)
legend({'Mean value estimate', 'True mean'})

subplot(3,2,[5 6]);
plot(out.Q);

f.Position(3) = 800;
f.Position(4) = 800;
    

