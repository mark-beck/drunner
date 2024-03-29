module S = Sys
open Core
module Unix = Core_unix

(**replaces ~ with the correct path*)
let realize_path path =
  if String.is_prefix path ~prefix:"~"
  then (
    let path = String.chop_prefix_exn path ~prefix:"~" in
    String.concat [ Sys.getenv_exn "HOME"; path ])
  else path
;;

(**returns a list of all desktop paths*)
let desktop_paths =
  [ "/usr/share/applications/"
  ; "/usr/local/share/applications/"
  ; "~/.local/share/applications/"
  ]
  |> List.map ~f:realize_path
;;

let prevs_path = realize_path "~/.cache/drunner_prevs"

type bin =
  { displayname : string
  ; exec : string list
  ; exectimes : int
  } [@@deriving show]

type exec =
  | Bin of bin
  | Command of string [@@deriving show]

type prev =
  { name : string
  ; times : int
  } [@@deriving show]

exception ParseError

let split_colon line =
  match String.split line ~on:':' with
  | [ name; times ] -> Some (name, times)
  | _ -> None
;;

(**read prev file*)
let read_prevs path =
  let open Option.Let_syntax in
  try
    path
    |> In_channel.read_lines
    |> List.filter_map ~f:(fun line ->
      let%bind name, times = split_colon line in
      let%bind times = Int.of_string_opt times in
      Some { name; times })
  with
  | _ -> []
;;

(**apply prevs to bins*)
let apply_prevs bins ~prevs =
  List.map bins ~f:(fun bin ->
    let exectimes =
      prevs
      |> List.find ~f:(fun prev -> String.( = ) prev.name bin.displayname)
      |> Option.value_map ~default:0 ~f:(fun prev -> prev.times)
    in
    { bin with exectimes })
  |> List.sort ~compare:(fun a b -> Int.compare b.exectimes a.exectimes)
;;

(**increase prev*)
let increase_prev prevs ~name =
  if List.exists prevs ~f:(fun prev -> String.( = ) prev.name name)
  then
    List.map prevs ~f:(fun prev ->
      if String.( = ) prev.name name then { prev with times = prev.times + 1 } else prev)
  else { name; times = 1 } :: prevs
;;

(**write prevs to file*)
let write_prevs ~path prevs =
  let content =
    List.map prevs ~f:(fun prev ->
      String.concat [ prev.name; ":"; Int.to_string prev.times ])
    |> String.concat ~sep:"\n"
  in
  Out_channel.write_all path ~data:content
;;

(**parse a desktop entry into a bin*)
let parse_entry content =
  let map =
    content
    |> String.split_lines
    |> List.filter ~f:(fun line -> String.contains line '=')
    |> List.map ~f:(fun line ->
      match String.split line ~on:'=' with
      | k :: xs -> k, List.hd_exn xs
      | _ ->
        Printf.printf "Error on line: %s" line;
        raise ParseError)
  in
  let displayname = List.Assoc.find_exn map ~equal:String.equal "Name" in
  let exec =
    List.Assoc.find_exn map ~equal:String.equal "Exec"
    |> String.split ~on:' '
    |> List.filter ~f:(fun e -> not (String.contains e '%'))
  in
  { displayname; exec; exectimes = 0 }
;;

(**reads all desktop entries in folder and converts them to (list bin)*)
let get_desktop_entries dir =
  try
    S.readdir dir
    |> Array.to_list
    |> List.filter_map ~f:(fun filename ->
      let filepath = String.concat [ dir; filename ] in
      try In_channel.read_all filepath |> parse_entry |> Option.some with
      | _ -> None)
  with
  | _ -> []
;;

(**convert all filenames in dir into bins*)
let get_bins (dir : string) =
  try
    dir
    |> S.readdir
    |> Array.to_list
    |> List.map ~f:(fun displayname ->
      { displayname; exec = [ dir ^ "/" ^ displayname ]; exectimes = 0 })
  with
  | Sys_error _ -> []
;;

(**opens dmenu with a list of bins and returns an optional exec*)
let open_dmenu (bins : bin list) =
  let inch, outch = Core_unix.open_process "dmenu -i" in
  List.iter bins ~f:(fun bin -> Printf.fprintf outch "%s\n" bin.displayname);
  Out_channel.close outch;
  In_channel.input_line inch
  |> Option.map ~f:(fun ret ->
    bins
    |> List.find ~f:(fun bin -> String.( = ) bin.displayname ret)
    |> Option.value_map ~default:(Command ret) ~f:(fun bin -> Bin bin))
;;

let dedup =
  List.stable_dedup_staged ~compare:(fun bin1 bin2 ->
    String.compare bin1.displayname bin2.displayname)
  |> Staged.unstage
;;

(**executes a binary or command*)
let execute exec =
  let command =
    match exec with
    | Bin bin -> bin.exec
    | Command c -> c |> String.split ~on:' '
  in
  Printf.printf "Executing: %s\n" (String.concat command ~sep:" ");
  S.command (String.concat command ~sep:" ") |> ignore
;;

let () =
  let bins =
    Sys.getenv_exn "PATH"
    |> String.split ~on:':'
    |> List.map ~f:realize_path
    |> List.concat_map ~f:get_bins
  in
  let desktop_entries = desktop_paths |> List.concat_map ~f:get_desktop_entries in
  let prevs = read_prevs prevs_path in
  [ desktop_entries; bins ]
  |> List.concat
  |> dedup
  |> apply_prevs ~prevs
  |> open_dmenu
  |> Option.iter ~f:(fun exec ->
    execute exec;
    match exec with
    | Bin bin -> increase_prev prevs ~name:bin.displayname |> write_prevs ~path:prevs_path
    | _ -> ())
;;
