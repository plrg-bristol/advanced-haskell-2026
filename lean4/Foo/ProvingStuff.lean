theorem modus_ponens {p q : Prop}
  : (p -> q) -> p -> q := by
  intro pImpQ
  intro hp
  apply pImpQ
  exact hp

#print modus_ponens

theorem modus_ponens' {p q : Prop}
  : (p -> q) -> p -> q := λpImpQ hp => pImpQ hp

theorem de_morgan {p q : Prop}
  : ¬(p ∨ q) -> ¬p ∧ ¬q := sorry

theorem de_morgan_op {p q : Prop}
  : ¬(p ∧ q) -> ¬p ∨ ¬q := sorry

theorem de_morgan_spec {p q : Prop}
  : ¬(p ∧ ¬q) -> ¬p ∨ q := by
  intro notPandNotQ
  sorry -- needs law of excluded middle?

theorem imp_to_or {p q : Prop}
  : (p -> q) -> ¬p ∨ q := by
  intro pImpQ
  apply de_morgan_spec
  intro ⟨hp, hNotQ⟩
  have hq := pImpQ hp
  contradiction
