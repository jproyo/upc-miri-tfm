import diefpy.dief as diefpy

def plot_experiments():
  root_path = '/Users/juan/Projects/mios/upc-miri-tfm/connected-comp/results/diefpy/dp-blh'
  folders = ["ca-AstroPh", "email-Enron", "web-Google"]
  colors = ["#ECC30B","#00CC00"]
  for f in folders:
    traces = diefpy.load_trace(f'{root_path}/{f}/results.csv')
    diefpy.plot_answer_trace(traces, f, colors).show()
    metrics = diefpy.load_metrics(f"{root_path}/{f}/metrics.csv")
    exp1 = diefpy.performance_of_approaches_with_dieft(traces, metrics)
    diefpy.plot_performance_of_approaches_with_dieft(exp1, f, colors).show()


def dief_t_k():
  root_path = '/Users/juan/Projects/mios/upc-miri-tfm/connected-comp/results/diefpy/dp-blh'
  folders = ["ca-AstroPh", "email-Enron", "web-Google"]
  for f in folders:
    traces = diefpy.load_trace(f'{root_path}/{f}/results.csv')
    print("dief@t", f)
    for (_, s, n) in diefpy.dieft(traces, f):
      print('&', s, '&', '$'+"{:.2E}".format(round(n,2))+'$', '\\\\')
  print('')
  for f in folders:
    traces = diefpy.load_trace(f'{root_path}/{f}/results.csv')
    print("dief@k", f)
    for (_, s, n) in diefpy.diefk(traces, f):
      print('&', s, '&', '$'+"{:.2E}".format(round(n,2))+'$', '\\\\')

#dief_t_k()
plot_experiments()

