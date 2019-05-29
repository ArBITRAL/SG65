-module(user_code).
-compile(export_all).


%% helper functions
min_colour(L) ->
    min_colour(1,lists:usort(sets:to_list(L))).

min_colour(V, []) -> V;
min_colour(V, [H | _]) when V < H ->
    V;
min_colour(V, [H | T]) when V == H ->
    min_colour(V + 1, T).
