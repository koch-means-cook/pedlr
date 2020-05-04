function[err] = pedlr_calculate_err(pred, prob, type, spec)
if strcmp(type, 'least_squares')
    
    err = sum((pred(:) - prob(:)).^2);
   
elseif strcmp(type, 'likelihood')
    L =[];
    for i = 1:length(pred)
        L(i) = -log(normpdf(prob(i), pred(i), 0.2));
    end
    err = sum(L);
elseif strcmp(type, 'pri_and_liklhd')
        logp =[];
        pd =[];%probability density
        for i = 1:numel(spec.pr.type) %loop over all parameters
            if strcmp(spec.pr.type{i}, 'beta')
                n = betacdf([spec.limits(i,1) spec.limits(i,2)], spec.pr.alpha(i), spec.pr.beta(i));
                norm = n(2) - n(1);
                mu = spec.pr.alpha(i) / (spec.pr.alpha(i) + spec.pr.beta(i));
                pd(i) = betapdf(mu, spec.pr.alpha(i), spec.pr.beta(i));
                logp(i) = log(pd(i) / norm);
            elseif strcmp(spec.pr.type{i}, 'norm')
                n = normcdf([spec.limits(i,1) spec.limits(i,2)], spec.pr.mu(i), spec.pr.sigm(i));
                norm = n(2) - n(1);
                pd(i) = normpdf(spec.pr.mu(i), spec.pr.mu(i), spec.pr.sigm(i));
                logp(i) = log(pd(i) / norm);
            end
        end

        %calculate log likelihood
       loglik =[];
        for i = 1:length(pred)
            loglik(i) = log(normpdf(prob(i), pred(i), 0.2)); %0.2 is an arbitrary value
        end

        %combine prior and likelihood
        err = sum(logp) + sum(loglik);
    
end