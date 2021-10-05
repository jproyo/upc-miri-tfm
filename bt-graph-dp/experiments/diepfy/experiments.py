import diefpy.dief as diefpy

def plot_experiments():
  root_path = '/Users/juan/Projects/upc/upc-miri-tfm/data/experiments'
  # folders = ["moreno_crime","dbpedia","opsahl-ucforum","wang-amazon"]
  folders = ["wang-amazon"]
  colors = ["#4287f5","#19E67C","#1A211E","#244A4F","#0fC4DB","#8F2E73","#B36224","#ECC30B","#D56062"]
  for f in folders:
    traces = diefpy.load_trace(f'{root_path}/{f}/results/results.csv')
    # diefpy.plot_answer_trace(traces, f, colors).show()
    # metrics = diefpy.load_metrics(f"{root_path}/{f}/results/metrics.csv")
    # exp1 = diefpy.experiment1(traces, metrics)
    print(diefpy.dieft(traces, "wang-amazon",4144))
    # diefpy.plotExperiment1Test(exp1, f, colors).show()

plot_experiments()


