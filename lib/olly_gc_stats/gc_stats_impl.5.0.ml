module M : Gc_stats_intf.S = struct
  let domain_minor_words = Array.make Gc_stats_shared.number_domains 0
  let domain_promoted_words = Array.make Gc_stats_shared.number_domains 0
  let bytes_per_word = Sys.word_size / 8

  let runtime_counter ~ring_id _ts counter_type value =
    match counter_type with
    | Runtime_events.EV_C_MINOR_PROMOTED ->
        domain_promoted_words.(ring_id) <-
          domain_promoted_words.(ring_id) + (value / bytes_per_word)
    | Runtime_events.EV_C_MINOR_ALLOCATED ->
        domain_minor_words.(ring_id) <-
          domain_minor_words.(ring_id) + (value / bytes_per_word)
    | _ -> ()

  let totals () =
    let minor_words = ref 0.0 in
    let promoted_words = ref 0.0 in
    Array.iteri
      (fun i v ->
        minor_words := !minor_words +. float_of_int v;
        promoted_words :=
          !promoted_words +. float_of_int domain_promoted_words.(i))
      domain_minor_words;
    let total_heap = !minor_words -. !promoted_words in
    let promoted_pct = !promoted_words /. !minor_words *. 100.0 in
    (!minor_words, !promoted_words, total_heap, promoted_pct)

  let print_text oc =
    let minor_words, promoted_words, total_heap, promoted_pct = totals () in
    Printf.fprintf oc "GC allocations (in words): \n";
    Printf.fprintf oc "Total heap:\t %.0f\n" total_heap;
    Printf.fprintf oc "Minor heap:\t %.0f\n" minor_words;
    Printf.fprintf oc "Promoted words:\t %.0f (%.2f%%)\n" promoted_words
      promoted_pct;
    Printf.fprintf oc "\n"

  let to_json_fragment () =
    let minor_words, promoted_words, total_heap, promoted_pct = totals () in
    Printf.sprintf
      {|"allocations": {"total_heap": %.0f, "minor_heap": %.0f, "promoted_words": %.0f, "promoted_pct": %.2f}|}
      total_heap minor_words promoted_words promoted_pct
end

include Gc_stats_shared.Make (M)
