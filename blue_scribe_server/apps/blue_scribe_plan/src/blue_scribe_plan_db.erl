-module(blue_scribe_plan_db).

-include_lib("blue_scribe_burner/include/blue_scribe_burner.hrl").

-export([install/0, get_all_plan_ids/0, get_plan_name/1, get_plan_notes/1,
         get_plan_op_list/1, get_plan_preview_png/1,
         create_plan/3, update_plan_name/2,
         update_plan_notes/2, update_plan_op_list/2,
         increment_plan_start_counter/1,
         increment_plan_finish_counter/1,
         delete_plan/1, get_png_filename/1]).

-record(blue_scribe_plan, {id :: non_neg_integer(),
                           name :: string(),
                           notes :: string(),
                           started_count = 0 :: non_neg_integer(),
                           completed_count = 0 :: non_neg_integer(),
                           op_list :: laser_plan() | undefined}).

-spec get_all_plan_ids() -> [non_neg_integer()].
get_all_plan_ids() ->
    F = fun() ->
                mnesia:all_keys(blue_scribe_plan)
        end,
    mnesia:activity(transaction, F).

-spec get_plan_name(Id :: non_neg_integer()) ->
    {ok, string()} | {error, _}.
get_plan_name(Id) ->
    F = fun() ->
                case mnesia:read({blue_scribe_plan, Id}) of
                    [] -> {error, not_found};
                    [#blue_scribe_plan{name=Name}] ->
                        {ok, Name}
                end
        end,
    mnesia:activity(transaction, F).

-spec get_plan_notes(Id :: non_neg_integer()) ->
    {ok, string()} | {error, _}.
get_plan_notes(Id) ->
    F = fun() ->
                case mnesia:read({blue_scribe_plan, Id}) of
                    [] -> {error, not_found};
                    [#blue_scribe_plan{notes=Notes}] ->
                        {ok, Notes}
                end
        end,
    mnesia:activity(transaction, F).

-spec get_plan_op_list(Id :: non_neg_integer()) ->
    {ok, laser_plan()} | {error, _}.
get_plan_op_list(Id) ->
    F = fun() ->
                case mnesia:read({blue_scribe_plan, Id}) of
                    [] -> {error, not_found};
                    [#blue_scribe_plan{op_list=undefined}] ->
                        {error, no_plan};
                    [#blue_scribe_plan{op_list=Ops}] ->
                        {ok, Ops}
                end
        end,
    mnesia:activity(transaction, F).

-spec get_plan_preview_png(Id :: non_neg_integer()) ->
    {ok, binary()} | {error, _}.
get_plan_preview_png(Id) ->
    Filename = do_get_png_filename(Id),
    file:read_file(Filename).

-spec create_plan(File :: binary(),
                  Name :: string(),
                  Notes :: string()) ->
    {ok, Id :: non_neg_integer()} | {error, _}.
create_plan(File, Name, Notes) ->
    F = fun() ->
                Id = do_make_id(),
                Filename = do_get_png_filename(Id),
                %TODO make sure these go somewhere appropriate (priv, etc)
                ok = file:write_file(Filename, File),
                ok = mnesia:write(#blue_scribe_plan{
                                     id=Id,
                                     name=Name,
                                     notes=Notes}),
                {ok, Id}
        end,
    mnesia:activity(transaction, F).

-spec update_plan_name(Id :: non_neg_integer(), Name :: string()) ->
    ok | {error, _}.
update_plan_name(Id, Name) ->
    F = fun() ->
                case mnesia:read({blue_scribe_plan, Id}) of
                    [] -> {error, not_found};
                    [#blue_scribe_plan{}=Rec] ->
                        mnesia:write(Rec#blue_scribe_plan{name=Name})
                end
        end,
    mnesia:activity(transaction, F).

-spec update_plan_notes(Id :: non_neg_integer(), Notes :: string()) ->
    ok | {error, _}.
update_plan_notes(Id, Notes) ->
    F = fun() ->
                case mnesia:read({blue_scribe_plan, Id}) of
                    [] -> {error, not_found};
                    [#blue_scribe_plan{}=Rec] ->
                        mnesia:write(Rec#blue_scribe_plan{notes=Notes})
                end
        end,
    mnesia:activity(transaction, F).

-spec update_plan_op_list(Id :: non_neg_integer(), OpList :: laser_plan()) ->
    ok | {error, _}.
update_plan_op_list(Id, OpList) ->
    F = fun() ->
                case mnesia:read({blue_scribe_plan, Id}) of
                    [] -> {error, not_found};
                    [#blue_scribe_plan{}=Rec] ->
                        mnesia:write(Rec#blue_scribe_plan{op_list=OpList})
                end
        end,
    mnesia:activity(transaction, F).

-spec increment_plan_start_counter(Id :: non_neg_integer()) -> ok | {error, _}.
increment_plan_start_counter(Id) ->
    F = fun() ->
                case mnesia:read({blue_scribe_plan, Id}) of
                    [] -> {error, not_found};
                    [#blue_scribe_plan{started_count=Sc}=Rec] ->
                        mnesia:write(Rec#blue_scribe_plan{started_count=Sc+1})
                end
        end,
    mnesia:activity(transaction, F).

-spec increment_plan_finish_counter(Id :: non_neg_integer()) -> ok | {error, _}.
increment_plan_finish_counter(Id) ->
    F = fun() ->
                case mnesia:read({blue_scribe_plan, Id}) of
                    [] -> {error, not_found};
                    [#blue_scribe_plan{completed_count=Cc}=Rec] ->
                        mnesia:write(Rec#blue_scribe_plan{completed_count=Cc+1})
                end
        end,
    mnesia:activity(transaction, F).

-spec delete_plan(Id :: non_neg_integer()) ->
    ok | {error, _}.
delete_plan(Id) ->
    F = fun() ->
                case mnesia:read({blue_scribe_plan, Id}) of
                    [] -> {error, not_found};
                    _ ->
                        Filename = do_get_png_filename(Id),
                        file:delete(Filename),
                        mnesia:delete({blue_scribe_plan, Id})
                end
        end,
    mnesia:activity(transaction, F).

-spec get_png_filename(Id :: non_neg_integer()) -> string().
get_png_filename(Id) ->
    do_get_png_filename(Id).

install() ->
    mnesia:create_schema([node()]),
    application:start(mnesia),
    mnesia:create_table(blue_scribe_plan,
                        [{attributes, record_info(fields, blue_scribe_plan)},
                         %{index, [#blue_scribe_plan.id]},
                         {disc_copies, [node()]}]).


do_make_id() ->
    Size = mnesia:table_info(blue_scribe_plan, size),
    do_make_id_(Size).

do_make_id_(N) ->
    case mnesia:read({blue_scribe_plan, N}) of
        [] ->
            N;
        _ ->
            do_make_id_(N * 2)
    end.

-spec do_get_png_filename(Id :: non_neg_integer()) -> string().
do_get_png_filename(Id) ->
    lists:flatten(io_lib:format("plan_~8..0w.png", [Id])).

