-module (calc_props).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


proper_test_() ->
    {timeout, 30, ?_assertEqual([],
        proper:module(?MODULE, [{to_file, user}, {numtests, 1000}]))}.

prop_simple() ->
    ?FORALL(Node, expr_node(),
            begin
            Str = expr_node_to_str(Node), 
            erl_calc(Str) == element(1, calculator:solve(Str)) 
            end).

operator() ->
    oneof(['+', '-', '*']).

expr_node_str() ->
    ?LET(X, expr_node(),
         expr_node_to_str(X)).

expr_node() ->
    ?LAZY(?LET([X, Op, Y], 
               [frequency([{5, pos_integer()}, {2, expr_node()}]),
                operator(),
                frequency([{5, pos_integer()}, {2, expr_node()}])],
               {X, Op, Y})).

expr_node_to_str({X, Op, Y}) ->
    lists:flatten(io_lib:format("(~s ~s ~s)", [transform(X), Op, transform(Y)]));
expr_node_to_str(Other) ->
    io:format(user, "~p~n", [Other]),
    "(1 + 1)".

transform(X) when is_integer(X) -> integer_to_list(X);
transform(X) -> expr_node_to_str(X).

erl_calc(Str) ->
    {ok, Tokens, _} = erl_scan:string(Str++"."),
    {ok, [Form]} = erl_parse:parse_exprs(Tokens),
    {value, Result, _} = erl_eval:expr(Form, []),
    Result.