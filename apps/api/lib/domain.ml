(** Domain Models — facade that re-exports all domain sub-modules.
    Existing code continues to use [Domain.type_name] unchanged. *)

include Domain_core
include Domain_game_flow
include Domain_splits
include Domain_prediction
include Domain_streaks
include Domain_mvp_fantasy
include Domain_play_analysis
