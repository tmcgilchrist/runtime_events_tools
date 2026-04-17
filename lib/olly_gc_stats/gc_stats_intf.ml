(* Per-OCaml-version interface for the GC allocation-stats portion of olly.
   Each implementation owns its per-domain allocation state and knows how
   to translate that state into human-readable text and a JSON fragment.
   Everything else in the gc-stats pipeline is version-independent and
   lives in [Gc_stats_shared.Make]. *)
module type S = sig
  val runtime_counter :
    ring_id:int ->
    Runtime_events.Timestamp.t ->
    Runtime_events.runtime_counter ->
    int ->
    unit

  val print_text : out_channel -> unit

  (* Comma-separated "key": value entries with no surrounding braces, ready
     to be interpolated into the larger gc-stats JSON object between
     "distr_latency" and "collections". *)
  val to_json_fragment : unit -> string
end
