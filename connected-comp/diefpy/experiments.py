import diefpy.dief as diefpy

traces_email_enron = diefpy.load_trace("/Users/juan/Projects/upc/upc-miri-tfm/connected-comp/results/diefpy/email-Enron.csv")
diefpy.plot_answer_trace(traces_email_enron, "E1",["#ECC30B","#D56062"]).show()
metrics_enron = diefpy.load_metrics("/Users/juan/Projects/upc/upc-miri-tfm/connected-comp/results/diefpy/email-Enron-metrics.csv")
exp1 = diefpy.experiment1(traces_email_enron, metrics_enron)
diefpy.plotExperiment1Test(exp1, 'E1', ["#ECC30B","#D56062"]).show()


traces_ca_astroph = diefpy.load_trace("/Users/juan/Projects/upc/upc-miri-tfm/connected-comp/results/diefpy/ca-AstroPh.csv")
diefpy.plot_answer_trace(traces_ca_astroph, "E2",["#ECC30B","#D56062"]).show()
metrics_ca_astroph = diefpy.load_metrics("/Users/juan/Projects/upc/upc-miri-tfm/connected-comp/results/diefpy/ca-AstroPh-metrics.csv")
exp2 = diefpy.experiment1(traces_ca_astroph, metrics_ca_astroph)
diefpy.plotExperiment1Test(exp2, 'E2', ["#ECC30B","#D56062"]).show()

traces_web_google = diefpy.load_trace("/Users/juan/Projects/upc/upc-miri-tfm/connected-comp/results/diefpy/web-Google.csv")
diefpy.plot_answer_trace(traces_web_google, "E3",["#ECC30B","#D56062"]).show()
metrics_web_google = diefpy.load_metrics("/Users/juan/Projects/upc/upc-miri-tfm/connected-comp/results/diefpy/web-Google-metrics.csv")
exp2 = diefpy.experiment1(traces_web_google, metrics_web_google)
diefpy.plotExperiment1Test(exp2, 'E3', ["#ECC30B","#D56062"]).show()
