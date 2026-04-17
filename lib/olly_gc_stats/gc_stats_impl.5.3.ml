module M : Gc_stats_intf.S = struct
  let domain_minor_words = Array.make Gc_stats_shared.number_domains 0
  let domain_promoted_words = Array.make Gc_stats_shared.number_domains 0
  let domain_major_words = Array.make Gc_stats_shared.number_domains 0
  let bytes_per_word = Sys.word_size / 8

  let runtime_counter ~ring_id _ts counter_type value =
    match counter_type with
    | Runtime_events.EV_C_MINOR_PROMOTED ->
        domain_promoted_words.(ring_id) <-
          domain_promoted_words.(ring_id) + (value / bytes_per_word)
    | Runtime_events.EV_C_MINOR_ALLOCATED ->
        domain_minor_words.(ring_id) <-
          domain_minor_words.(ring_id) + (value / bytes_per_word)
    | Runtime_events.EV_C_MAJOR_ALLOCATED_WORDS ->
        domain_major_words.(ring_id) <- domain_major_words.(ring_id) + value
    | _ -> ()

  let totals () =
    let minor_words = ref 0.0 in
    let major_words = ref 0.0 in
    let promoted_words = ref 0.0 in
    Array.iteri
      (fun i v ->
        minor_words := !minor_words +. float_of_int v;
        major_words := !major_words +. float_of_int domain_major_words.(i);
        promoted_words :=
          !promoted_words +. float_of_int domain_promoted_words.(i))
      domain_minor_words;
    let total_heap = !minor_words -. !promoted_words +. !major_words in
    let promoted_pct = !promoted_words /. !minor_words *. 100.0 in
    (!minor_words, !major_words, !promoted_words, total_heap, promoted_pct)

  let print_global_allocation_stats oc =
    let minor_words, major_words, promoted_words, total_heap, promoted_pct =
      totals ()
    in
    Printf.fprintf oc "GC allocations (in words): \n";
    Printf.fprintf oc "Total heap:\t %.0f\n" total_heap;
    Printf.fprintf oc "Minor heap:\t %.0f\n" minor_words;
    Printf.fprintf oc "Major heap:\t %.0f\n" major_words;
    Printf.fprintf oc "Promoted words:\t %.0f (%.2f%%)\n" promoted_words
      promoted_pct;
    Printf.fprintf oc "\n"

  let print_per_domain_stats oc =
    Printf.fprintf oc "Per domain stats: \n";
    let data =
      ref [ [ "Domain"; "Total"; "Minor"; "Promoted"; "Major"; "Promoted(%)" ] ]
    in
    Array.iteri
      (fun i (domain_major_word, (domain_minor_word, domain_promoted_word)) ->
        if domain_major_word > 0 then
          data :=
            List.append !data
              [
                [
                  string_of_int i;
                  string_of_int
                    (domain_minor_word - domain_promoted_word
                   + domain_major_word);
                  string_of_int domain_minor_word;
                  string_of_int domain_promoted_word;
                  string_of_int domain_major_word;
                  Printf.sprintf "%.2f"
                    (float_of_int domain_promoted_word
                    /. float_of_int domain_minor_word
                    *. 100.0);
                ];
              ])
      (Array.combine domain_minor_words domain_promoted_words
      |> Array.combine domain_major_words);
    Gc_stats_shared.print_table oc !data

  let print_text oc =
    print_global_allocation_stats oc;
    print_per_domain_stats oc

  let domain_alloc_stats_json () =
    let buf = Buffer.create 256 in
    Array.iteri
      (fun i (domain_major_word, (domain_minor_word, domain_promoted_word)) ->
        if domain_major_word > 0 then (
          if Buffer.length buf > 0 then Buffer.add_char buf ',';
          Buffer.add_string buf
            (Printf.sprintf
               {|"%d": {"total": %d, "minor": %d, "promoted": %d, "major": %d, "promoted_pct": %.2f}|}
               i
               (domain_minor_word - domain_promoted_word + domain_major_word)
               domain_minor_word domain_promoted_word domain_major_word
               (float_of_int domain_promoted_word
               /. float_of_int domain_minor_word
               *. 100.0))))
      (Array.combine domain_minor_words domain_promoted_words
      |> Array.combine domain_major_words);
    Buffer.contents buf

  let to_json_fragment () =
    let minor_words, major_words, promoted_words, total_heap, promoted_pct =
      totals ()
    in
    Printf.sprintf
      {|"allocations": {"total_heap": %.0f, "minor_heap": %.0f, "major_heap": %.0f, "promoted_words": %.0f, "promoted_pct": %.2f}, "domain_alloc_stats": {%s}|}
      total_heap minor_words major_words promoted_words promoted_pct
      (domain_alloc_stats_json ())
end

include Gc_stats_shared.Make (M)
