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

let store_object _obj = failwith "TODO ( store_object )" nique ta mere nico

let read_text_object _h: Digest.t = Core.In_channel.read_all (".ogit/objects/" ^ _h)

let store_work_directory () = failwith "TODO ( store_work_directory )"

let read_directory_object _h = failwith "TODO ( read_directory_object )" 
  
let clean_work_directory () = failwith "TODO ( clean_work_directory )"

let restore_work_directory _obj = failwith "TODO ( restore_work_directory )" 

let merge_work_directory_I _obj = failwith "TODO ( merge_work_directory_I )"