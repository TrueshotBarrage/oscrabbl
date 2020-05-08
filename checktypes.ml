module type StateSig = sig
  type state
  val init_state : unit -> state
  val fill_hand : state -> state
end

module StateCheck : StateSig = State

module type AuthorSig = sig
  val hours_worked : int list
end

module AuthorCheck : AuthorSig = Authors
