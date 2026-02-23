(** Player identity helpers.

    Some players appear under multiple WKBL player IDs. We already canonicalize
    stats via `player_identities`, but we also want stable URLs so the same
    person does not look like two different players in the UI. *)

let redirect_location ~(requested_id : string) ~(canonical_id : string) ?suffix (uri : Uri.t) :
    string option =
  if String.trim canonical_id = "" then None
  else if canonical_id = requested_id then None
  else
    let qs = Uri.encoded_of_query (Uri.query uri) in
    let base =
      match suffix with
      | None -> Views_common.player_href canonical_id
      | Some s -> Views_common.player_href canonical_id ^ "/" ^ s
    in
    Some (if qs = "" then base else base ^ "?" ^ qs)

