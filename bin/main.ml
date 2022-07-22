module S = Sys
open Core
module Unix = Core_unix

let realize_path path =
  if String.is_prefix path ~prefix:"~" then
    let path = String.chop_prefix_exn path ~prefix:"~" in
    String.concat [Sys.getenv_exn "HOME"; path]
  else
    path

let desktop_paths =
  ["/usr/share/applications/"; "/usr/local/share/applications/"; "~/.local/share/applications/"]
  |> List.map ~f:realize_path


type bin = {
  displayname : string;
  exec : string list;
}

type exec = Bin of bin | Command of string

exception ParseError

let parse_entry content =
  let lines = String.split_lines content |> List.filter ~f:(fun line -> String.contains line '=') in
  let map = lines |> List.map ~f:(fun line ->
    match String.split line ~on:'=' with
    | k :: xs -> (k, List.hd_exn xs)
    | _ -> 
      Printf.printf "Error on line: %s" line;
      raise ParseError) in
  
  let displayname = List.Assoc.find_exn map ~equal:String.equal "Name" in
  let exec = List.Assoc.find_exn map ~equal:String.equal "Exec" |> String.split ~on:' ' |> List.filter ~f:(fun e -> not (String.contains e '%')) in
  {displayname; exec}

let get_desktop_entries dir =
  try S.readdir dir |> Array.to_list |> List.filter_map ~f:(fun filename ->
    let filepath = String.concat [dir; filename] in
    try Some (In_channel.read_all filepath |> parse_entry)
    with
    | _ -> None)
  with
  | _ -> []


let get_bins (dir : string) =
  S.readdir dir |> Array.to_list |> List.map ~f:(fun name -> {displayname = name; exec = [name]})

let open_dmenu bins =
  let (_inch, outch) = Core_unix.open_process "dmenu -i" in
  List.iter bins ~f:(fun bin ->
    Printf.fprintf outch "%s\n" bin.displayname);
  Out_channel.close outch;
  let ret = In_channel.input_line _inch in
  ret |> Option.map ~f:(fun ret ->
    bins 
    |> List.find ~f:(fun bin ->
      String.(=) bin.displayname ret) 
      |> Option.value_map ~default:(Command ret) ~f:(fun bin -> Bin bin))
  


let execute exec =
  let command = match exec with
  | Bin bin   -> bin.exec
  | Command c -> c |> String.split ~on:' ' in
  Unix.create_process ~prog:(List.hd_exn command) ~args:(List.tl_exn command) |> ignore

let () = 
  let path = Sys.getenv_exn "PATH" |> String.split ~on:':' |> List.map ~f:realize_path in
  let bins = path 
  |> List.map ~f:get_bins 
  |> List.concat in
  let desktop_entries = desktop_paths 
  |> List.map ~f:get_desktop_entries 
  |> List.concat in
  let dedup = List.stable_dedup_staged ~compare:(fun bin1 bin2 ->  String.compare (String.concat bin1.exec) (String.concat bin2.exec)) |> Staged.unstage in
  List.concat [desktop_entries; bins] 
  |> dedup
  |> open_dmenu
  |> Option.iter ~f:execute