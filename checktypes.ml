module type AuthorSig = sig
  val hours_worked : int list
end

module AuthorCheck : AuthorSig = Authors
