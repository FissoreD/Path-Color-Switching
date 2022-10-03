---
title: "Find path of length K with minimum number of color switch"
author: "Fissore Davide"
date: "2022-10-01"
caption-justification: centering
...

# Introduction

## Definitions

- $G = (X, E)$ is a graph
- $c(e)$ is a function returning the set of colors associated to the edge $e$
- $P_x = [x_1, ..., x_{n+1}]$ be a path or sequence of nodes of $X$
- $P_e = [e_1, ..., e_n]$ be the sequence of edges of a path $P_x$, the extrema of each $e_i = (x_i, x_{i+1})$ 
- $C = [c_1, ..., c_{n}]$ is the list of selected colors: $c_i$ is the color of $e_i \in P_e$ among $c(e_i)$
- a *color switch* is given if $c_i \neq c_{i+1}$ for all $i \in [1, ..., n]$

## Objective

For a given graph, find a sequence of edges $P_e$ of length $K$ with the minimum number of switches.

## Existing algorithms

From an adjacency matrix $M$ of a weighted graph $G$ we are able to find the length and the path(s) of minimum length made of exactly $K$ edges in polynomial time.

If we want to find the shortest path of length $1$ between two nodes $a$ and $b$, we only have to take $M[a][b]$. 

> Note that this length is $\infty$ if there does not exist an edge between $a$ and $b$

Let $\bigodot$ be a binary matrix operation between $A$ and $B$ then $A \bigodot B = C$ where

$$C_{ij} = \min_{p = 1 \ldots n}\left(A_{i p} + B_{p j}\right)$$

For a graph $G$ a path $p$ of minimum length from $a$ to $b$ with $K$ edges can be computed by applying $K$ times $\bigodot$ to the adjacency matrix $M$ of G.

$$p = {G^{\bigodot K}}_{ab}$$

## Objective

Apply the previous algorithm on a colored graphs in order the find a path of minimal length (minimal number of switches) with $K$ edges.

# Solution

## Idea 

From now on a shortest path is a path with the minimum number of color switches.

The main idea is to build a modified adjency matrix $M$ where 

- $c_{ij} = (\varnothing, \infty)$ if there is no edge going from $i$ to $j$;
- $c_{ij} = (c(i,j), 0)$ otherwise. 

To compute the length of the shortest path of length $K$ we should introduce the $\bigotimes$ operator. 

Given two matrices $A$ and $B$, $A \bigotimes B = C$ where $c_{ij}$ is returned by the following algorithm : 

```ocaml
let l = [] in 

for i = 0 to |V| - 1 do
  let old_c, old_w = A[i][p] in (* The edge ip *)
  let col, _ = B[p][j] in       (* The edge pj *)
  
  match Set.inter old_c col with 
  | EMPTY_SET -> l.append(col, old_w + 1)
  | inter -> l.append(inter, old_w)
done;

if l.costgth = 0 then return (EMPTY_SET, MAX_INT)
else
  let min = List.fold_left min MAX_INT l in 
  let edge_colors = List.filter (fun _ w -> w = min).fold_left(color_union) in 
  return (edge_colors, min)
```

Essentially, $c_{ij}$ represents an edge and it is made of a couple $(s, i)$ where $s$ is a set of colors and $i$ the number of colors switches.

When we compute the resulting matrix $C$, for every cell $c_{ij}$ we want to know the minimum length of a path going from $i$ to $j$ through a node $p$. We know the colors on the edge $ip = fst(A_{ip})$ and its weight $= snd(A_{ip})$. Then the colors on the edge $pj = fst(B_{pj})$. 

If $inter = (A_{ip}).color \cap  (B_{pj}).color$ is empty then we have a color switch and $c_{ij}^p$ is $((B_{pj}).color, (A_{ip}).cost+1)$ else if is $(inter, (A_{ip}).cost)$. Note that in this way we are computing all the color-length couple for every path from $i$ to $j$ throught $p$. The goal is to keep all the $p$s with the minimum cost and make the union of every $p.color$. The cell $c_{ij}$ will the be the couple made of that cost and that union.

$$c_{ij} = (\{\bigcup_{p = 0}^{|V|-1} (c_{ij}^p.color \text{ if } c^p_{ij} \text{ has minimum cost})\},\min_{p\in [1..|V|]}(c_{ij}^p.cost))$$

