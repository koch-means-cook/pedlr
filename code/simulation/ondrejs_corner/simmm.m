clear all
close all
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
out.sch(121:end,4) = 1;
out.sch = out.sch(randperm(ntrls),:);

%select 600 rewards from the 10k generated above
C= C(:,randperm(n))';
out.R = C(1:600,:);
out.Q = [50 50 50];


p.al0 = 0.2;
p.al1 = 0.3;
p.beta   = 5;
out = pedlr_model(p, out);

f=figure;
subplot(1,2,1)
title('Reward Distributions');
hold on 
hist(out.R)
xlim([0 100]);
subplot(1,2,2)
title('Model-estimated Distributions');
hold on 
hist(out.Q)
xlim([0 100]);

