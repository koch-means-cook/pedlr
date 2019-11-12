clear all
close all
iter = 50;
domain = 0; % 0 = positive domain; -1 = negative domain   
ncues = 3;
for t = 1:iter
    clear('out', 'C')
    % Generate distributions of rewards
    n = 10000;
    for i = 1:n
        %C(1, i) = betarnd(1.66, 3.33)*100 + domain*100; 
        %C(3, i) = betarnd(3.33, 1.66 )*100 + domain*100;
        %C(2, i) = normrnd(50, 15) + domain*100;
        C(1, i) = betarnd(1.66, 3.33)*100 + domain*100; 
        C(3, i) = betarnd(3.33, 1.66 )*100 + domain*100;
        C(2, i) = betarnd(2.5, 2.5)*100 + domain*100;
        
        %C(1, i) = normrnd(33, 20) + domain*100;
        %C(3, i) = normrnd(66, 20) + domain*100;
        %C(2, i) = normrnd(50, 20) + domain*100;
    end

    % visually check distributions



    %%
    out = [];
    out.ncues =ncues;
    ntrls_each = 200; %should be 100 of each choice
    tr_types = nchoosek(1:3,2);
    tr_types = [tr_types; [tr_types(:,2) tr_types(:,1)]];
    out.sch = (repmat(tr_types,ntrls_each,1));
    ntrls = size(out.sch,1);
    %add force choice
    out.sch(1:0.2*ntrls,4) = 1;
    out.sch(0.2*ntrls+1:end,4) = 0;
    out.sch = out.sch(randperm(ntrls),:);

    %select 600 rewards from the 10k generated above
    C= C(:,randperm(n))';
    out.R = C(1:ntrls,:);
    out.Q = [50 50 50];
    %out.Q = [-50 -50 -50];

    p.al0 = 0.1;
    p.al1 = 0.9;
    p.beta   = 1;
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
    
    deliveredR = out.R(:, out.chb');
    estimatedQ = out.Q(:, out.chb');
    %for k = 1:3
    %    meanR(t,k) = mean(deliveredR(out.chb==k,k));
    %    meanQ(t,k) = mean(estimatedQ(out.chb==k,k));
    %end
    for k = 1:ncues
        cho = find(out.chb==k);
        chosen(t,k)=length(cho);
        meanR(t,k) = mean(out.R(cho, k));
        meanQ(t,k) = mean(out.Q(cho, k));
    end
end
mean_diff = meanQ - meanR;
mR = mean(meanR);
mQ = mean(meanQ);
y_coord = [0 100];

f = figure;
subplot(3,3,1)
histogram(out.R(:,1), 'BinWidth', 2, 'Handlevisibility', 'Off')
hold on 
plot([mQ(1) mQ(1)], y_coord, 'r')
plot([mR(1) mR(1)], y_coord, 'b')
title(['Chosen ' num2str(mean(chosen(:,1))) '/' num2str(ntrls*2/3) ]);
text(17, 120, {['true: ' num2str(round(mR,1))], ['estim: ' num2str(round(mQ,1))]})
legend({'Mean value estimate', 'True mean'})
subplot(3,3,4)
histogram(meanQ(:,1), 'BinWidth', 0.5)
hold on 
histogram(meanR(:,1), 'BinWidth', 0.5)
legend({'Mean value estimate', 'True mean'})

subplot(3,3,2)
histogram(out.R(:,2), 'BinWidth', 2, 'Handlevisibility', 'Off')
hold on 
plot([mQ(2) mQ(2)], y_coord, 'r')
plot([mR(2) mR(2)], y_coord, 'b')
title(['Chosen ' num2str(mean(chosen(:,2))) '/' num2str(ntrls*2/3) ]);
legend({'Mean value estimate', 'True mean'})
subplot(3,3,5)
histogram(meanQ(:,2), 'BinWidth', 0.5)
hold on 
histogram(meanR(:,2), 'BinWidth', 0.5)
legend({'Mean value estimate', 'True mean'})

subplot(3,3,3)
histogram(out.R(:,3), 'BinWidth', 2, 'Handlevisibility', 'Off')
hold on 
plot([mQ(3) mQ(3)], y_coord, 'r')
plot([mR(3) mR(3)], y_coord, 'b')
title(['Chosen ' num2str(mean(chosen(:,3))) '/' num2str(ntrls*2/3) ]);
legend({'Mean value estimate', 'True mean'})
subplot(3,3,6)
histogram(meanQ(:,3), 'BinWidth', 0.5)
hold on 
histogram(meanR(:,3), 'BinWidth', 0.5)
legend({'Mean value estimate', 'True mean'})

subplot(3,3,[7:9]);
plot(out.Q);

f.Position(3) = 800;
f.Position(4) = 800;
    
for i = 1:ncues
    [r,p]=corr(mean_diff(:,i), chosen(:,i));
    disp(['Cue ' num2str(i) ' r=' num2str(r) ' p=' num2str(p)]);
end
