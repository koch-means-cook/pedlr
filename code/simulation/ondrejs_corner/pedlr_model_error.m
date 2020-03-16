function[err] = pedlr_model_error(p, out)

[out] = feval(out.model_name, p, out);

%log likelihood
err = -(sum(log(out.P(out.data.ch==1,1)))+sum(log(out.P(out.data.ch==2,2))));
if err == Inf 
    err= 9999;
end




