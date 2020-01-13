open Core

let words text =
    let remove_non_lyric = function
        | c when Char.is_alphanum c -> c
        | '\'' -> '\''
        | _ -> ' '
    in
    let only_lyric = String.map ~f:remove_non_lyric text in
        List.filter
        ~f:(fun str -> not (String.is_empty str))
        (String.split ~on:' ' only_lyric)

let word_count text =
    words text
    |> List.fold ~init:[] ~f:(fun counts word ->
        let count =
            match List.Assoc.find ~equal:String.equal counts word with
            | None -> 0
            | Some x -> x
        in
        List.Assoc.add ~equal:String.equal counts word (count + 1)
    )
  (* |> String.Map.of_alist_fold ~init:0 ~f:(+) *)

let build_counts () =
    let input_lines = In_channel.input_lines In_channel.stdin in 
    (* need regex here to strip out [...] lines *)
    let combined = String.concat ~sep:" " input_lines in
    word_count combined

let () =
  build_counts ()
  |> List.sort ~compare:(fun (_,x) (_,y) -> Int.descending x y)
  |> (fun l -> List.take l 20)
  |> List.iter ~f:(fun (line,count) -> printf "%3d: %s\n" count line)