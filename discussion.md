## Todos

- is_row (done, more testing needed)
- confirm_player_turn (needs testing)
- handling exception:

  - st on exception |> reset_board |> update_player/bot_hand (give person his hand back) |> reset_coords
  - give person his hand back: basically put all the letters back in the player/bot's hand that
    was "filled"

- Successful placement looks something like this:
  - st' = st |> put_on_board (as many as desired) |> confirm_player_turn
  - confirm calls => score_of_words (assign st.score) -> set_board -> reset_coords -> end_turn (maybe in main?)

- Need to integrate board & hand & bag all together for the final bonanza
- cache (needs testing, uses `(string, unit) Hashtbl`)
