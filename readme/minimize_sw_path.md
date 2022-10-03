---
title: "Minimize number of colors in Path "
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

For a sequence of nodes $P_x$ compute $C$ that minimizes the number of color switches.

## Examples

1. let $e_1 = \{r,g,b\}, e_2 = \{r,g\}, e_3 = \{r\}$ then the selection of color minimizing the number of switches is $c_1 = c_2 = c_3 = ``r"$ and the number of switches is 0

2. let $e_1 = \{r,g\}, e_2 = \{g\}, e_3 = \{r\}$ then the be selection of color that minimize the number of switches is $c_1 = c_2 = ``g"$, $c_3 = ``r"$ and $h(P_e) = 1$ (there is 1 switch between $c_2$ and $c_3$).

# Solutions

## Find minimum number of color switches

### Algorithm

input : **e** = a list of set of colors   
output : **counter** = the minimum number of switches  

```ocaml
(** The current set of colors *)
let cs = e[0]
let counter = 0

for i = 1 to e.length - 1 do
  let inter = cs.inter(e[i])
  if inter.is_empty then 
    counter++
    cs = e[i]
  else 
    cs = inter
done;

counter
```

### Idea 

We try to delay as much as possible a color switch by selecting the longest sub-sequence of edges $e_1, \dots, e_i$ having at least one color in common. If i = $n$ then we have zero color switch. Else we have a switch between $e_i$ and $e_{i+1}$. We repeat the same procedure from $e_{i+1}$ until we reach $e_n$

### Formally  

Let $x_1, x_2, ..., x_n$ a path where $x_i$ are its vertices.

1. If the intersection $i_1$ of $c(x_1, x_2), c(x_2, x_3)$ is **non-empty**, it means that we can choose one color of $i_1$ for $e_1 = (x_1, x_2)$ and $e_2 = (x_2, x_3)$ without impacting the number of switches (these two edges will have same color).

2. Otherwise, we have two disjoint sets and we are forced to make a color switch.

At this moment it is just like repeating the same algo for the subpath $x_2, ..., x_n$ where $c(x_2, x_3) = i_1$ if $i_1 \neq \varnothing$ and $c(e_{2})$ otherwise. We can also claim that in this way we are minimizing the number of switches, because we try each time to infer to the next edge a set of colors that do not raise a color switch, which is $i_1$ in our case. On the other hand if $i_1$ is empty, a switch is forced and we give to the next edge $e_{next}$ the largest set of colors possible which is the set returned by $c(e_{next})$. The fact of giving to $e_{next}$ the maximum number of colors allow us to reduce the probability a colors switch later.

### Time Complexity

We have one loop of size $n$ (the length of the path) and inside it we make intersection between sets. Let $m$ be the largest set of color returned by the function $c$, then the intersection between two sets of that size will take $O(m)$. The global time complexity is therefor $O(n * m)$.

## Compute the sequence C of a sequence $P_e$ of edges

### Algorithm

input : **e** = a list of set of colors   
output : **res** = a sequence C with the minimum number of color switches

```ocaml
let n = e.length - 1

for i = 1 to n do
  let inter = e[i-1].inter(e[i])
  if not inter.is_empty 
    e[i] = inter
done;

let res = [e[n].choose()]
for i = n - 1 downto 0 do 
  if e[i].includes(res[0]) then 
    res.prepend(res[0]) 
  else 
    res.prepend(e[i].choose())
done;

res
```

### Idea

Let $p = x_1, x_2, ..., x_n$ be a path, $c(i, j)$ the color function given in input that return the set of colors of the edge $ij$ and $f(i, j)$ the function that return the chosen colors of the edge $(i, j)$.

The chosen colors of the edge $x_1x_2 = c(x_1,x_2)$.

Each edge $e$ made of a couple of adjacent vertices $(x_i, x_{i+1}) \in p$ where $x_i \neq x_1$, is colored with

-  $inter = f(x_{i-1}, x_i) \cap c(x_i, x_{i+1})$ if $inter \neq \varnothing$.
-  $c(x_i, x_{i+1})$ otherwise

To compute a valid sequence of colors, we start from the last edge $e = (x_{n-1}, x_n)$ of $p$ and we choose a color $c_n$ from $f(e)$. We go to the previous edge $e_{n-1} = (x_{n-2}, x_{n-1})$ and its colors will be either $c_n$ if $c_n \in c(e_{n-1})$ or a color chosen in $c(e_{n-1})$. 

A more detailed proof is not provided since it will be similar to the proof of the previous algorithm.

### Time Complexity

We have two loops of size $n$ (the length of the path) and inside them we make intersection between sets. Let $m$ be the largest set of color returned by the function $c$, then the intersection between two sets of that size will take $O(m)$. The global time complexity is therefor $O(2 * n * m) = O(n*m)$.