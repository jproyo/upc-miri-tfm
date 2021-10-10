import diefpy.dief as diefpy

def plot_experiments():
  root_path = '/Users/juan/Projects/upc/upc-miri-tfm/data/experiments'
  # folders = ["moreno_crime","dbpedia","opsahl-ucforum","wang-amazon"]
  folders = ["wang-amazon"]
  colors = ["#4287f5","#19E67C","#1A211E","#244A4F","#0fC4DB","#8F2E73","#B36224","#ECC30B","#D56062"]
  for f in folders:
    traces = diefpy.load_trace(f'{root_path}/{f}/results/results.csv')
    diefpy.plot_answer_trace(traces, f, colors).show()
    metrics = diefpy.load_metrics(f"{root_path}/{f}/results/metrics.csv")
    exp1 = diefpy.experiment1(traces, metrics)
    diefpy.plotExperiment1Test(exp1, f, colors).show()

def to_scenario_id(name):
  scenarios = {
    'vertex-lower-low': 'VL-L',
    'vertex-lower-medium': 'VL-M',
    'vertex-lower-high': 'VL-H',
    'vertex-upper-low': 'VU-L',
    'vertex-upper-medium': 'VU-M',
    'vertex-upper-high': 'VU-H',
    'edge-low': 'E-L',
    'edge-medium': 'E-M',
    'edge-high': 'E-H',
  }
  return scenarios[name]


def dief_t_k():
  root_path = '/Users/juan/Projects/upc/upc-miri-tfm/data/experiments'
  folders = ["moreno_crime","dbpedia","opsahl-ucforum","wang-amazon"]
  #folders = ["wang-amazon"]
  colors = ["#4287f5","#19E67C","#1A211E","#244A4F","#0fC4DB","#8F2E73","#B36224","#ECC30B","#D56062"]
  for f in folders:
    traces = diefpy.load_trace(f'{root_path}/{f}/results/results.csv')
    print("dief@t", f)
    for (_, s, n) in diefpy.dieft(traces, f):
      print('&', to_scenario_id(s), '&', '$'+"{:.2E}".format(round(n,2))+'$', '\\\\')
  print('')
  for f in folders:
    traces = diefpy.load_trace(f'{root_path}/{f}/results/results.csv')
    print("dief@k", f)
    for (_, s, n) in diefpy.diefk(traces, f):
      print('&', to_scenario_id(s), '&', '$'+"{:.2E}".format(round(n,2))+'$', '\\\\')

dief_t_k()
# plot_experiments()

