-module(blue_scribe_plan_image).

-on_load(init/0).

-export([do_load_png_file/1, do_count_mats/0, do_crop_image/3, do_png_encode/1, do_image_to_plan/2]).

-include_lib("blue_scribe_burner/include/blue_scribe_burner.hrl").

-type image_mat() :: any().

-define(APP, blue_scribe_plan).
-define(NIF, "blue_scribe_plan_image").


%% -----------------------------------------------------------------------------
%% @doc Loads the NIF so the image processing functions will work.
%% -----------------------------------------------------------------------------
init() ->
    erlang:load_nif(code:lib_dir(?APP) ++ "/priv/" ++ ?NIF, 0).

%% -----------------------------------------------------------------------------
%% @doc Loads a PNG file from disk as an OpenCV Mat
%% -----------------------------------------------------------------------------
-spec do_load_png_file(Path :: string()) -> image_mat().
do_load_png_file(_Path) ->
    erlang:nif_error(nif_library_not_loaded).

%% -----------------------------------------------------------------------------
%% @doc Counts how many OpenCV Mats are tracked in this module's NIF
%% -----------------------------------------------------------------------------
-spec do_count_mats() -> non_neg_integer().
do_count_mats() ->
    erlang:nif_error(nif_library_not_loaded).

%% -----------------------------------------------------------------------------
%% @doc Crops the specified image top-left if the specified bounds are smaller
%% than the original image; otherwise the original is returned.
%% -----------------------------------------------------------------------------
-spec do_crop_image(Mat :: image_mat(),
                    Height :: non_neg_integer(),
                    Width :: non_neg_integer()) -> image_mat().
do_crop_image(_,_,_) ->
    erlang:nif_error(nif_library_not_loaded).

%% -----------------------------------------------------------------------------
%% @doc Encode an OpenCV Mat as PNG format.
%% -----------------------------------------------------------------------------
-spec do_png_encode(Mat :: image_mat()) -> binary().
do_png_encode(_) ->
    erlang:nif_error(nif_library_not_loaded).

%% -----------------------------------------------------------------------------
%% @doc Does a scan of the OpenCV Mat and turs it into laser operations.
%% No optimization is done at this stage.
%% -----------------------------------------------------------------------------
-spec do_image_to_plan(Mat :: image_mat(), PowerScale :: float()) -> laser_plan().
do_image_to_plan(_,_) ->
    erlang:nif_error(nif_library_not_loaded).
