import igraph
from math import comb
import os

def btcounting():
  lambda_big = 0
  lambda_big_hat = 0
  g = igraph.Graph.Read_Edgelist(f = os.path.realpath("input/threebts.txt"))
  upper = g.get_edge_dataframe()["source"]
  lower = g.get_edge_dataframe()["target"]
  for l in lower:
    s,h = {}, {}
    for u in g.neighbors(l):
      for x in g.neighbors(u):
        if x != l:
          if x not in s:
            s[x] = 0
          s[x] = s[x] + 1
    for x in s:
      lambda_big_hat = lambda_big_hat + (comb(s[x],2) * (g.degree(x) - 2))
      for y in g.neighbors(x):
        if y not in h:
          h[y] = 0
        h[y] = h[y] + s[x] 
    for y in h:
      if y in g.neighbors(l):
        h[y] = h[y] - g.degree(y) + 1
      lambda_big = lambda_big + comb(h[y],2)
  for u in upper:
    ts = {}
    for l in g.neighbors(u):
      for t in g.neighbors(l):
        if t != u:
          if t not in ts:
            ts[t] = 0
          ts[t] = ts[t] + 1
    for t in ts:
      lambda_big_hat = lambda_big_hat + (comb(ts[t],2) * (g.degree(t) - 2))

  return (lambda_big - lambda_big_hat)/3

print(btcounting())

