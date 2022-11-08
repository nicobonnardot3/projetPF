type t =
| Text of string
| Directory of (string * bool * Digest.t * t) list

let rec hash _obj = 
  match _obj with
  | Text text -> Digest.string text
  | Directory ([]) -> ""
  | Directory ((name, isDir, _, t)::tl) -> 
    if isDir then Digest.string (name ^ ";d;" ^ (hash t) ^ (hash (Directory(tl))))
    else Digest.string (name ^ ";t;" ^ (hash t) ^ (hash (Directory(tl))))

let is_known _h = Sys.file_exists (".ogit/objects/" ^ _h)

let store_object _obj = failwith "TODO ( store_object )"

let read_text_object _h: Digest.t = Core.In_channel.read_all (".ogit/objects/" ^ _h)

let store_work_directory () = failwith "TODO ( store_work_directory )"

let rec read_directory_object _h =
  let dataString = read_text_object _h in
  let splitData = String.split_on_char '\n' dataString in
  let rec createDirObj data = 
    match data with
    | [] -> Text ""
    | [txt] ->
        let txtData = String.split_on_char ';' txt in
        if (List.nth txtData 1) = "t" then Text (List.nth txtData 2)
        else Directory ([List.hd txtData, true, (List.nth txtData 2), read_directory_object ( List.nth txtData 2 )] )
    | hd::tl ->
        let txtData = String.split_on_char ';' hd in
        if (List.nth txtData 1) = "t" then (Text (List.nth txtData 2))::(createDirObj(tl))
        else (Directory ([List.hd txtData, true, (List.nth txtData 2), read_directory_object ( List.nth txtData 2 )]))::(createDirObj(tl))
  in
  Text ""
  
let clean_work_directory () = failwith "TODO ( clean_work_directory )"

let restore_work_directory _obj = failwith "TODO ( restore_work_directory )"

let merge_work_directory_I _obj = failwith "TODO ( merge_work_directory_I )"