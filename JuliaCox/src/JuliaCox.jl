module JuliaCox

using Survival
using CSV
using DataFrames
using DataFramesMeta
using StatsBase

function genecox(mydf, genename, factorcol, positivevaule, negativevalue; quant_range = 0.2:0.02:0.81)
    d = DataFrame()
    pos_df = @subset(mydf, $factorcol .== positivevaule)
    neg_df = @subset(mydf, $factorcol .== negativevalue)
    for n in quant_range
        qcut = quantile(mydf[:, genename], n)
        ###all#######
        try
            all = DataFrame(coeftable(coxph(hcat(ifelse.(mydf[:, genename] .> qcut, 1, 0)), mydf.event)))
            replace!(all[!, :Name], "x1" => join([genename, "all", string(n), string(qcut)], "_"))
            append!(d, all)
        catch
            push!(d,[join([genename, "all", string(n),string(qcut)], "_") 0 0 0 0])
        end
        ###positive#######
        try
            pos = DataFrame(coeftable(coxph(hcat(ifelse.(pos_df[:, genename] .> qcut, 1, 0)), pos_df.event)))
            replace!(pos[!, :Name], "x1" => join([genename, "pos", string(n),string(qcut)], "_"))
            append!(d, pos)
        catch
            push!(d,[join([genename, "pos", string(n),string(qcut)], "_")  0 0 0 0])
        end
        ###negative######
        try
            neg = DataFrame(coeftable(coxph(hcat(ifelse.(neg_df[:, genename] .> qcut, 1, 0)), neg_df.event)))
            replace!(neg[!, :Name], "x1" => join([genename, "neg", string(n),string(qcut)], "_"))
            append!(d, neg)
        catch
            push!(d,[join([genename, "neg", string(n),string(qcut)], "_")  0 0 0 0])
        end

    end
    return d
end

function dopipeline(mydf, factorcol, positivevaule, negativevalue,inexclude,genelist)
    myresult = DataFrame()
    ####group data ######    
    mydf.event = EventTime.(mydf.survivalMonth, mydf.survivalEvent .== 1.0)
    ####run ######
    if inexclude =="y"
        for genename in genelist
            append!(myresult, genecox(mydf, genename, factorcol, positivevaule, negativevalue))
        end
    elseif inexclude =="n"
        for genename in names(mydf)
            if !(genename in genelist) && (genename !="Column1") && (genename !="event") && (genename !=factorcol)
                #println(genename)  
                append!(myresult, genecox(mydf, genename, factorcol, positivevaule, negativevalue))
            end
        end
    else
        println("inexclude value must be y or n!")
    end
    return myresult
end


function dopipeline_file(myfile, factorcol, positivevaule, negativevalue,inexclude, genelist)
    mydf = CSV.read(myfile, DataFrame,header=1,delim=",",)
    df = dopipeline(mydf, factorcol, positivevaule, negativevalue,inexclude, genelist)
    CSV.write(join([factorcol, "coxph.csv"], "_"), df, delim = '\t')
    return join([factorcol, "done"], " ")
end


function julia_main()::Cint
    # do something based on ARGS?
    #@show ARGS
    myfile = ARGS[1]
    factorcol = ARGS[2]
    positivevaule = ARGS[3]
    negativevalue = ARGS[4]
    inexclude = ARGS[5]
    genelist = ARGS[6:end]
    re = (dopipeline_file(myfile, factorcol, positivevaule, negativevalue,inexclude,genelist))
    println(re)
    return 1  # if things finished successfully
  end

end



