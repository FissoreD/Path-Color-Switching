---
title: "Minimize number of colors switches in Path "
author: "Fissore Davide"
date: "2022-10-01"
caption-justification: centering
...

# Introduction

Given a path $P$ composed by a sequence of $k$ adjacent edges $(e_1, ..., e_k)$ and a function $F$ returning a set of colors for every edge, we say that a valid affectation $H = (h_1, ..., h_k)$ is a sequence of colors such that each $h_i$ is a color belonging to $C(e_i)$.

The weight $w$ of a sequence $H$ is given by the number of color switch inside the sequence.

The goal of this section is to provide a greedy algorithm able to compute a sequence $H$ of colors that minimize the number of color switches.

# Definitions

- $P = (e_1, ..., e_k)$ is a path made of $k$ edges 
- $C(e_i)$ is a function returning the set of colors associated to the edge $e_i \in P$
- $H = (c_1, ..., c_k)$ is a list of colors where $c_i$ is a color selected from $C(e_i)$ for each $e_i \in P$
- the weight $w$ of a path $p$ is defined as :
  $$
    w(H) = \sum_{i = 1}^{|H| - 1} (h_i \neq h_{i+1})
  $$


# Find minimum number of color switches

In this section we propose a greedy strategy to solve the previous problem.

## Idea 

We want to show that the good strategy to solve this problem is a greedy way is to delay as much as possible a color switch by selecting the longest sub-sequence of consecutive edges $(e_1, \dots, e_i)$ having at least one color in common. When we are no more able to extend our set of common colors, we repeat the same algo from the position causing the color switch. 

## Formally  

Let $p$ be a path and $(e_1, ..., e_k)$ its edges. We start to build the first sub-seauence of edges sharing the same set colors. Let $c_1 = C(e_1)$. We go to $e_2$ and :

1. If $c_1 \cap C(e_2)$ is **non-empty**, we repeat the algo from $e_2$ where $c_2 = c_1 \cap C(e_2)$

2. Otherwise, we have two disjoint sets forcing a color switch. We repeat the algorithm from $e_2$ with $c_2 = C(e_2)$.

We can note that this algo returns a set of possible colors for each edge. Since we want each edge to be affected to a single color, we start from $c_k$ and select one random color from it. Then $\forall i \in (1, .., k - 1), \text{ if } c_k \in c_{k-i} \text{ then } c_{k-i} = c_k \text{ else } peek(c_{k-i})$

## Proof

We proove the previous algorithm by means of contradiction.

Let $H = (c_1, ..., c_k)$ be the solution returned by our algorithm and let $H^* = (c^*_1, ..., c^*_k)$ the optimal solution such that $H > H^*$. 

Since $H$ is not optimal, at least one extra color switch has been done at a certain moment $j$. Let's suppose that for all $i < j, c_i = c^*_i$ then:
$$
c_{j-1} = c^*_{j-1} \land c_{j-1} \neq c_j \land c^*_{j-1} = c^*_j \rightarrow c_j \neq c_j^*
$$

Then it means that $C(e_j)$ should not contain $c_{j-1}$ otherwise the algorithm, by its construction, would have affected $c_j$ to $c_{j-1}$. However, since $c^*_j = c^*_{j-1}$ and since $H^*$ is the optimal solution then $C(e_j)$ must contain $c^*_{j-1}$ forcing $c_j$ to be equal to $c^*_j$, contradictiong our previous hypothesis.

We can conclude that $H = H^*$.



## Implementation

In this section we provide an implementation of the previous algorithm that really follow the specification provided just before.

input : **e** = a list of set of colors   
output : **h** = a sequence of color H with the minimum number of color switches

```ocaml
let k = p.length
let prov = [C(p[0])]

for i = 1 to k - 1 do
  let inter = prov[i - 1].inter(C(p[i]))
  if not inter.is_empty 
    prov.append(inter)
  else
    prov.append(C(p[i]))
done;

let h = [prov[k - 1].choose()]
for i = k - 2 downto 0 do 
  if prov[i].includes(h[0]) then 
    h.prepend(h[0]) 
  else 
    h.prepend(prov[i].choose())
done;

h
```

## Time Complexity

We have two loops of size $k$ (the length of the path). Inside them we make intersection between sets of at most $s$ colors, then the intersection between two sets of that size will take $O(s)$. Finally, the global time complexity will be $O(2 * k * s) = O(k*s)$.

# Examples

1. let $e_1 = \{r,g,b\}, e_2 = \{r,g\}, e_3 = \{r\}$ then the selection of color minimizing the number of switches is $c_1 = c_2 = c_3 = ``r"$ and the number of switches is 0. The algorithm exectution will be the folloing:
   1. First loop :
      1. Iteration 1 : $prov = [\{r,g,b\}]$
      2. Iteration 2 : $prov = [\{r,g,b\}, \{r,g\}]$
      3. Iteration 3 : $prov = [\{r,g,b\}, \{r,g\},\{r\}]$
   2. Second loop :
      1. Iteration 1 : $h = [r]$
      2. Iteration 2 : $h = [r, r]$
      3. Iteration 3 : $h = [r, r, r]$
2. let $e_1 = \{r, g\}, e_2 = \{r, b\}, e_3 = \{b, g\}$ then the be selection of color that minimize the number of switches is $c_1 = c_2 = ``r"$, $c_3 = ``b"$ and $h(P_e) = 1$ (there is 1 switch between $c_2$ and $c_3$). The algorithm exectution will be the folloing:
   1. First loop :
      1. Iteration 1 : $prov = [\{r,g\}]$
      2. Iteration 2 : $prov = [\{r,g\}, \{r\}]$
      3. Iteration 3 : $prov = [\{r,g\}, \{r\},\{b, g\}]$
   2. Second loop :
      1. Iteration 1 : $h = [b]$
      2. Iteration 2 : $h = [r, b]$
      3. Iteration 3 : $h = [r, r, b]$