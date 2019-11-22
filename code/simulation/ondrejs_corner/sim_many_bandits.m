clear all
close all
iter = 20;
shift = 0.1; % 0 = positive domain; -1 = negative domain   
ncues = 10;
f=figure;
[al1vals,al0vals] = deal([0.1:0.2:0.9]);
c=1;
I ={};
calc = 0;
if calc == 0
load('bias_over_parameters_forced_choice_last10perc.mat', 'I');
end
for al1 = 1:numel(al1vals)
    for al0 = 1:numel(al0vals)
        p.al0 = al0vals(al0);
        p.al1 = al1vals(al1);
        p.beta   = 1;
        
        if calc == 1
            for t = 1:iter
                clear('out', 'C')
                % Generate distributions of rewards
                n = 10000;
                for i = 1:n
                    for j = 1:ncues
                        C(j, i) = (betarnd(2.5, 2.5)-0.5+(shift*(j)))*100; 
                    end

                end

                % visually check distributions



                %%
                out = [];
                out.ncues = ncues;
                ntrls_each = 100;  %should be 100 of each choice
                tr_types = nchoosek(1:ncues,2);
                tr_types = [tr_types; [tr_types(:,2) tr_types(:,1)]];
                out.sch = (repmat(tr_types,ntrls_each,1));
                ntrls = size(out.sch,1);

                 %add force choice
                out.sch(1:0.2*ntrls,4) = 1;
                out.sch(0.2*ntrls+1:end,4) = 1;
                out.sch = out.sch(randperm(ntrls),:);

                %select 600 rewards from the 10k generated above
                C= C(:,randperm(n))';
                out.R = C(1:ntrls,:);
                out.Q = repmat([50], 1, ncues);
                %out.Q = [-50 -50 -50];

                
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
                for k = 1:ncues
                    chosen=find(out.chb==k);
                    meanR(t,k) = mean(out.R(intersect(8100:9000, chosen), k));
                    meanQ(t,k) = mean(out.Q(intersect(8100:9000, chosen), k));;
                end
                %meanR(t,:) = mean(out.R);
                %meanQ(t,:) = mean(out.Q);
            end

            mean_diff = meanQ - meanR;
            mR = mean(meanR);
            mQ = mean(meanQ);
            y_coord = [0 100];
            I{al0, al1}.meanQ       = meanQ;
            I{al0, al1}.meanQ       = meanR;
            I{al0, al1}.mean_diff   = mean_diff;
        else
            meanQ = I{al0, al1}.meanQ ;
            meanR = I{al0, al1}.meanQ ;
            mean_diff = I{al0, al1}.mean_diff;
        end
        
        
        subplot(numel(al1vals), numel(al0vals), c);
        for i = 1:ncues
            scaled_i = i*shift*100;
            scatter(repmat(scaled_i,iter,1), mean_diff(:,i), 'MarkerFaceColor', 'k', 'MarkerEdgeColor', 'k')
            hold on

            plot([scaled_i-0.2*shift*100 scaled_i+0.2*shift*100], repmat(mean(mean_diff(:,i)),2,1), 'Color', [0.8 0.1 0.1], 'LineWidth', 5);
            ylim([-1 1]);
        end
        title(['$\alpha_0=' num2str(round(p.al0,2)) '\hspace*{1cm} \alpha_1=' num2str(round(p.al1,2)) '$'], 'Interpreter', 'Latex') 
        ylabel('Est-Mean')
        xlabel('Cue mean');
        c=c+1;
    end 
end
save('bias_over_parameters_forced_choice_last10perc.mat', 'I');
f.Position(3) = 1500;
f.Position(4) = 1500;
%{
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
    
for i = 1:3
    [r,p]=corr(mean_diff(:,i), chosen(:,i));
    disp(['Cue ' num2str(i) ' r=' num2str(r) ' p=' num2str(p)]);
end
    
%}
