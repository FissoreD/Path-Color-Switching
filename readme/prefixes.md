# The prefixes of a colored shortest path are also minimal ???

Definition :   
A **prefix** of a path $P = x_1, x_2, ..., x_k$ is every subpath of P going from $x_1$ to any $x_i \in P$.

Let $P = x_1, x_2, ..., x_k$ a path of minimal length with k edges.

**Statement** : The length of every prefix of $P$ is always minimal.

**Proof** :   
let's suppose that there exists a path $P'$ from $x_1$ to $x_i$ different from the subpath $P_s = x_1, ..., x_i$ such that the number of edges in $P'$ equals the number of edges of $P_s$.

Let $l_1 = len(P')$ and $l_s = len(P_s)$:
- if $l_1 = l_s$ then