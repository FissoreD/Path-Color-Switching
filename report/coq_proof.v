From mathcomp Require Import all_ssreflect.

Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Section abstract_assign.

Variable color : finType.

Variable def_col : color.

Variable exchoose : {set color} -> color.

Hypothesis exchooseP : forall s,  s != set0 -> exchoose s \in s.

Fixpoint make_assignment_set (l : seq {set color}) (run : nat)
   (current_set : {set color}) : seq {set color} :=
  match l with
  | nil => nseq run current_set 
  (* nseq := return une liste [elt] * n *)
  | s1 :: l' =>
  let new_set := current_set :&: s1 in
  if new_set == set0 then
    nseq run current_set ++ make_assignment_set l' 1 s1
  else
    make_assignment_set l' (run + 1) new_set
  end.

Definition make_assignment (l : seq {set color}) : seq color :=
  match l with
    nil => nil
  | s0 :: l' => 
    [seq exchoose s | s <- make_assignment_set l' 1 s0]
  end. 
  (* exchoose return tjrs la m valeur pour un set donnée *)

Lemma size_make_assignment_set (l : seq {set color}) (run : nat)
      (current_set : {set color}) :
      size (make_assignment_set l run current_set) = run + size l.
Proof.
  (* move : l run current_set.
  elim ; last first. *)
elim : l run current_set => [ | s1 l' Ih] run current_set.
(* rewrite /=. (*simpl*) *)
  by rewrite /= addn0 size_nseq.
rewrite /=. case: ifP => [inter0 | inter_n0].
rewrite size_cat.
rewrite size_nseq.
rewrite Ih.
rewrite add1n.
by [].
  (* by rewrite size_cat size_nseq Ih add1n. *)
by rewrite Ih addn1 addSn addnS.
Qed.

Fixpoint count_breaks (a : color) (l : seq color) : nat :=
  match l with
  | nil => 0
  | b :: l' => if b == a then count_breaks a l' else S (count_breaks b l')
  end.

Lemma count_breaks_nseq a n : count_breaks a (nseq n a) = 0.
Proof. by elim: n => [ | p Ih] //=; rewrite eqxx. Qed.

Lemma count_breaks_cat a b l1 l2 :
  count_breaks a (l1 ++ b :: l2) = count_breaks a (l1 ++ b :: nil) +
  count_breaks b l2.
Proof.
elim: l1 a => [ | c l1' Ih] a /=.
  by have [/eqP ->| bna] := boolP(b == a).
case: (c == a).
  by apply: Ih.
by rewrite addSn; congr (_.+1); apply: Ih.
Qed.

Definition compatible (spec : seq {set color}) (l : seq color) :=
  all2 (fun (c : color) (s : {set color}) => c \in s) l spec.

Lemma compatible_cat spec1 spec2 l1 l2 :
  size spec1 = size l1 ->
  compatible (spec1 ++ spec2) (l1 ++ l2) =
  compatible spec1 l1 && compatible spec2 l2.
Proof.
rewrite /compatible.
elim: spec1 l1 => [ | s1 spec1' Ih] [ | c1 l1'] //= [] sizes.
by rewrite (Ih _ sizes) andbA.
Qed.

(* prouver que (zip (map f l1) (map g l2)) == (map (fun p -> (f (fst p), g (snd p))) (zip l1 l2)) *)
Lemma zip_map2 [T1 T2 T3 T4: Type](l1 : seq T1) (l2 : seq T2) 
  (f : T1 -> T3) (g : T2 -> T4) :
   (* comme zip en python *)
  zip [seq f x | x <- l1] [seq g x | x <- l2] =
  (* [seq f x | x <- l1] := map f l1 *)
  [seq (f p.1, g p.2) | p <- zip l1 l2].
Proof.
by elim: l1 l2 => [ | a l1 Ih] [ | b l2] //=; congr (_ :: _).
Qed.

Let fstzip' : forall (s1 s2 : seq {set color}) p, p \in zip s1 s2 -> p.1 \in s1.
Proof.
elim=> [ | a s Ih] [ | b s2] p //=.
  rewrite inE=> /orP[/eqP -> //=| pin].
  by rewrite inE eqxx.
by rewrite inE (Ih _ _ pin) orbT.
Qed.

Let sndzip : forall (s1 s2 : seq {set color}) p, p \in zip s1 s2 -> p.2 \in s2.
Proof.
elim=> [ | a s Ih] [ | b s2] p //=.
  rewrite inE=> /orP[/eqP -> //=| pin].
  by rewrite inE eqxx.
by rewrite inE (Ih _ _ pin) orbT.
Qed.

Let fstzip : forall (s : seq {set color}) p (s0 : {set color}) r,
           p \in zip (nseq r s0) s -> p.1 = s0.
Proof. by move=> s p s0 r /fstzip'; rewrite mem_nseq => /andP[] _ /eqP. Qed.

Lemma make_assignment_set_compatible :
(* 
h : head de la liste (partie de la liste déjà traitée) + diff de []
run : size h 
current_set : bigcap de tout les elts h + diff de empty set
*)
  forall l (current_set : {set color}) (h : seq {set color}) (r : nat),
  current_set != set0 ->
  all (fun (s : {set color}) => s != set0) l ->
  all (fun (s : {set color}) => current_set \subset s) h ->
  size h = r ->
  1 <= r ->
  (* sr : set resultat , sp : specification *)
  all2 (fun (sr sp : {set color}) => sr \subset sp)
    (make_assignment_set l r current_set) (h ++ l) /\
  all (fun s : {set color} => s != set0) (make_assignment_set l r current_set).
Proof.
elim => [ | a l Ih] current_set h r stn0 ln0 lh sh rgt0.
  rewrite /= all2E cats0 sh size_nseq eqxx /=; split; last first.
    by apply/allP=> x; rewrite mem_nseq=> /andP[] _ /eqP ->.
  apply/allP=> /= p /[dup] pin /fstzip ->.
  apply: (allP lh).
  by apply: (sndzip pin).
rewrite all2E.
have ln0' : all (fun s : {set color} => s != set0) l.
  by move: ln0; rewrite /= => /andP[].
rewrite /=; case: ifP => [emptycase | nonemptycase].
  have an0 : a != set0 by apply: (allP ln0); rewrite inE eqxx.
  have := Ih a [:: a] 1 an0 ln0'; rewrite /= subxx => /(_ isT erefl isT).
  move=>[] + m_a_s_n0.
  rewrite all2E=> /andP[] size_result subset_result.
  rewrite !size_cat size_nseq /= sh (eqP size_result) eqxx /=.
  rewrite zip_cat; last by rewrite sh size_nseq.
  split; last by rewrite all_cat all_nseq stn0 orbT andTb.
  rewrite all_cat; apply/andP; split => //.
  apply/allP=> p pin /=; rewrite (fstzip pin).
  by apply: (allP lh); apply: (sndzip pin).
have lh' : all (fun s : {set color} => current_set :&: a \subset s) (rcons h a).
  rewrite all_rcons subsetIr /=.
  apply/allP=> x => /(allP lh).
  by apply: subset_trans; apply: subsetIl.
have := Ih (current_set :&: a) (rcons h a) (r + 1) (negbT nonemptycase) ln0' lh'.
rewrite !addn1 size_rcons sh => /(_ erefl isT) [] + m_a_s_n0.
rewrite all2E size_cat size_rcons addSn=>/andP[] Ihsize Ihsub.
rewrite size_cat /= addnS Ihsize /=.
split; by rewrite // -cat_rcons.
Qed.

Lemma make_assignment_compatible :
  forall l, 
   (forall s, s \in l -> s != set0) -> compatible l (make_assignment l).
Proof.
case=> [// | a l] ln0 /=.
have an0 : a != set0 by apply: ln0; rewrite inE eqxx.
have ln0' : all (fun s : {set color} => s != set0) l.
  by apply/allP=> x xin; apply: ln0; rewrite inE xin orbT.
have trivin :  all (fun (s : {set color}) => a \subset s) [:: a].
  by apply/allP=> x; rewrite inE => /eqP ->; rewrite subxx.
have := make_assignment_set_compatible an0 ln0' trivin erefl isT.
rewrite /compatible !all2E /= => - [] + newn0. 
case m_a_s_eq : (make_assignment_set _ _ _) =>
   [// | b l'] /= /andP[] sl' /andP[]ba allsub.
rewrite size_map sl' (subsetP ba) /=; last first.
  by apply: exchooseP; apply: (allP newn0); rewrite m_a_s_eq inE eqxx.
apply/allP=> p.
rewrite -(map_id l) zip_map2 /= => /mapP[p' p'in ] -> /=.
apply: (subsetP (allP allsub _ p'in)).
apply: exchooseP.
apply: (allP newn0); rewrite m_a_s_eq inE.
by rewrite (fstzip' p'in) orbT.
Qed.

Lemma nseq_rcons [T : Type](v : T) n : nseq n.+1 v = rcons (nseq n v) v.
Proof.
by elim: n => [ | p Ih] //=; rewrite -Ih /=.
Qed.

Lemma count_breaks_diff : forall c0 hs c1,
 c0 != c1 -> 0 < count_breaks c0 (hs ++ [:: c1]).
Proof.
move=> + hs.
elim: hs => [ | a hs Ih].
  by move=> c0 c1; rewrite /= eq_sym => /negbTE ->.
move=> c0 c1 c0nc1; rewrite /=.
have [/eqP ac0 /= | anc0 //] := boolP(a == c0).
by apply: Ih.  
Qed.

Lemma count_breaks_gt0 (s0 : {set color}) (hs : seq {set color}) 
  (hc : seq color) c0 c (a : {set color}) :
  c0 \in s0 -> compatible hs hc ->
  c \in a -> \bigcap_(s <- s0 :: hs) s :&: a == set0 ->
  0 < count_breaks c0 (hc ++ [:: c]).
Proof.
move=> c0in + cina inter0.
have : c \notin \bigcap_(s <- s0 :: hs) s.
  apply/negP=> abs.
  suff : c \in set0 by rewrite in_set0. 
  by rewrite -(eqP inter0) in_setI abs cina.
elim: hs hc {inter0} s0 c0 c0in => [ | s1 hs Ih] [ | c1 hc] // s0 c0 c0in.
  by rewrite big_seq1 /=; case: ifP=> [/eqP -> | ]; first rewrite c0in.
rewrite /= !big_cons in_setI negb_and.
have [/eqP c1c0 | //]:= boolP (c1 == c0).
move=> /orP[] A /andP[] c1s1 hshc; last first.
  rewrite -c1c0; apply: (Ih _ _ _ c1s1 _ hshc).
  by rewrite big_cons.
apply: count_breaks_diff; apply/negP=> /eqP c0c.
by case/negP: A; rewrite -c0c.
Qed.

Lemma compatible_not_empty ls lc :
  compatible ls lc -> all (fun x => x != set0) ls.
Proof.
elim: ls lc => [ | s0 ls Ih] [ | c lc] //= /andP[cin cmpt].
apply/andP; split; first by apply/set0Pn; exists c.
apply: (Ih _ cmpt).
Qed.

Lemma make_assigmnent_optimal (l : seq {set color}) (lc : seq color) :
  compatible l lc ->
  count_breaks (head def_col (make_assignment l)) (make_assignment l) <=
  count_breaks (head def_col lc) lc.
Proof.
(* In the next command, we explain the property that will be proved
  by induction. *)
suff main:
  forall l lc hs hc (s0 : {set color})
  c0 r, (size hs).+1 = r -> (size hc).+1 = r -> c0 \in s0 ->
  compatible (hs ++ l) (hc ++ lc) ->
  let st := \bigcap_(s <- s0 :: hs) s in
  let res := [seq exchoose s | s <- make_assignment_set l r st] in
  st != set0 ->
  count_breaks (head def_col res) res <= count_breaks c0 (hc ++ lc).
  case: l lc => [ | a l] [ | c lc] //.
  rewrite /make_assignment /compatible /= => /andP[] cina lcinl.
  have := size_make_assignment_set l 1 a.
  case m_a_s_eq : (make_assignment_set l 1 a) => [ | rs rl] //= [] srl.
  rewrite !eqxx.
  have := main l lc nil nil a c 1 erefl erefl cina lcinl => /=.
  have an0 : a != set0 by apply/set0Pn; exists c.
  rewrite big_seq1 => /(_ an0).
  by rewrite m_a_s_eq /= eqxx.
move=> {l lc}.
(* Now the proof by induction really starts. *)
elim=> [ | a l Ih] /= lc hs hc s0 c0 r shs shc c0in cmpt int_n0.
  by rewrite map_nseq -[in head _ _]shs /= count_breaks_nseq.
have shshc : size hs = size hc by move: shc; rewrite -shs; case.
case lceq: lc => [ | c' lc'].
  move: cmpt; rewrite /compatible all2E !size_cat ?eqn_add2l shshc eqn_add2l.
  by rewrite lceq.
have [c'ina cmptl] : c' \in a /\ compatible l lc'.
  by move: cmpt; rewrite compatible_cat // lceq /= => /and3P[].
have ln0 : all (fun s => s != set0) l.
  by apply: (compatible_not_empty cmptl).
have trivin : all (fun (s : {set color}) => a \subset s) [:: a].
  by rewrite /= andbT subxx.
have an0 : a != set0 by apply/set0Pn; exists c'.
have [subs /= alln0]:= make_assignment_set_compatible an0 ln0 trivin erefl isT.
have [inter0 | inter_n0] := boolP (\bigcap_(s <- (s0 :: hs)) s:&: a == set0).
  rewrite map_cat map_nseq -[in head _ _]shs /= -shs nseq_rcons -cats1 -catA.
  rewrite /= count_breaks_cat cats1 -nseq_rcons count_breaks_nseq add0n.
  have := size_make_assignment_set l 1 a.
  case m_a_s_eq : (make_assignment_set _ _ _) => [ | rc rl] //= [] srl.
  have -> : exchoose rc == exchoose (\bigcap_(s <- (s0 :: hs)) s) = false.
    have rcsuba : rc \subset a.
      have := make_assignment_set_compatible an0 ln0 trivin erefl isT.
      rewrite /= => - [] + _.
      by rewrite m_a_s_eq /= => /andP[].
    apply/negbTE/negP=> /eqP abs.
    move: inter0; apply/negP/set0Pn; exists (exchoose rc).
    rewrite in_setI (subsetP rcsuba); last first.
      rewrite exchooseP //; apply: (allP alln0).
      by rewrite m_a_s_eq inE eqxx.
    by rewrite abs exchooseP.
  move: (cmpt); rewrite compatible_cat ?shshc // lceq /= => /and3P[] _ _. 
  move=> lc'inl.
  have := Ih lc' nil nil a c' 1 erefl erefl c'ina lc'inl.
  rewrite big_seq1 /=.
  rewrite m_a_s_eq /= eqxx count_breaks_cat -add1n => /(_ an0) {}Ih.
  apply leq_add=> //.
  (* where the big computation happens. *)
  have hshc : compatible hs hc.
    by move: cmpt; rewrite compatible_cat // => /andP[].
  by apply: (count_breaks_gt0 c0in hshc c'ina).
have := size_make_assignment_set l (r + 1) (\bigcap_(s <- (s0 :: hs)) s :&: a).
(* rcons : l elt => l ++ [elt] *)
have := Ih lc' (rcons hs a) (rcons hc c') s0 c0 (r + 1).
rewrite 2!size_rcons shs shc 2!cat_rcons -lceq addn1.
move=> /(_ erefl erefl c0in cmpt).
have -> /= : \bigcap_(s <- s0 :: rcons hs a) s =
   \bigcap_(s <- s0 :: hs) s :&: a.
  by rewrite !big_cons -cats1 big_cat /= big_seq1 setIA.
move=> /(_ inter_n0).
by case m_a_s_eq : (make_assignment_set l r.+1 _).
Qed.

End abstract_assign.

(* The following is just an example: set intersection
  does not compute in this library (through simpl or compute), so
  the only way to make tests is to ruly on a more symbolic form of
  execution based or rewriting. *)

(* We define a type of colors. *)
Inductive color := Red | Green | Blue | Yellow | Black | White.

(* To show this type to be finite, we develop an injection into a type
  that is known to be finite. *)
Definition color_n (c : color) :=
  match c with
  Red => 0 | Green => 1 | Blue => 2 | Yellow => 3 | Black => 4 | White => 5
  end.

Definition nat_color (n : nat) :=
  match n with
  0 => Red | 1 => Green | 2 => Blue | 3 => Yellow | 4 => Black | _ => White
  end.

Definition color_n_bound (c : color) : color_n c < 6.
Proof. by case: c. Qed.

Definition color_idx (c : color) := Ordinal (color_n_bound c).

Definition idx_color (idx : 'I_6) := nat_color (val idx).

Lemma color_idxK : cancel color_idx idx_color.
Proof. by case. Qed.

Canonical color_eqType := EqType color (CanEqMixin color_idxK).
Canonical color_choiceType := ChoiceType color (CanChoiceMixin color_idxK).
Canonical color_countType := CountType color (CanCountMixin color_idxK).
Canonical color_finType := FinType color (CanFinMixin color_idxK).

(* At this point, the Coq+math components system knows the type color
  to be finite and this type inherits a large set of properties, in
  particular the capacity to reason about its subdsets. *)

(* This function plays the same role as the predefined function
   "choose" automatically provided for types, but by re-define it,
   we get the possibility to execute all the way to a color that
   is easier to define. *)
Definition min_color (s : {set color}) : color :=
  nat_color (find (fun n => nat_color n \in s) (iota 0 6)).

Lemma min_colorP (s : {set color}) :
  s != set0 -> min_color s \in s.
Proof.
move=> /set0Pn [e eP].
rewrite /min_color.
have in_iota : has (fun n => nat_color n \in s) (iota 0 6).
  apply/hasP; exists (color_n e).
    by rewrite mem_iota leq0n add0n color_n_bound.
  by rewrite -[nat_color _]/(idx_color (color_idx e)) color_idxK.
have := nth_find 6 in_iota.
rewrite nth_iota ?add0n //.
by move: (in_iota); rewrite has_find size_iota.
Qed.

(* This lemma is not required by the abstract section but will be useful
  for the coming tests. *)
Lemma mincolor_set1 (c : color) : min_color [set c] = c.
Proof.  by case: c; rewrite /min_color /= !in_set1 ?eqxx. Qed.

(* We now have all the needed elements (described as variables or
  hypotheses in the abstract section). We can make a test. *)
Definition test_path :=
  [:: [set Red; Green]; [set Green; Blue]; [set Red; Yellow]; [set Red]].

(* To perform the test, we need to do it in a proof, as we will use
  rewriting to compute set intersections and elements of sets. *)
Lemma test_path_result :
  make_assignment min_color test_path = [:: Green; Green; Red; Red].
Proof.
rewrite /make_assignment /=.
have r_b : [set Red] :&: [set Blue] = set0.
  by apply/eqP; rewrite setI_eq0 disjoints1 in_set1.
have rg_gb : [set Red; Green] :&: [set Green; Blue] = [set Green].
  by rewrite setUC -setUIr r_b setU0.
have g_ry : [set Green] :&: [set Red; Yellow] = set0.
  by apply/eqP; rewrite setI_eq0 disjoints1 in_set2.
have s1n0 (x : color) : [set x] != set0.
  by apply/negP=> abs; move: (set11 x); rewrite (eqP abs) in_set0.
have ry_r : [set Red; Yellow] :&: [set Red] = [set Red].
  by rewrite -setP=> x; rewrite in_setI in_set1 in_set2 orbK.
rewrite rg_gb (negbTE (s1n0 Green)) g_ry eqxx ry_r (negbTE (s1n0 _)).
rewrite /= !mincolor_set1.
by [].
Qed.
