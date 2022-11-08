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
  let rec createDirObj data = match data with
    | [] -> [("", false, Digest.string "", Text "")]
    | [txt] ->
        let txtData = String.split_on_char ';' txt in
        let fileName = List.hd (String.split_on_char '\r' (List.nth txtData 2)) in
        let fileContent = read_text_object fileName in
        if (List.nth txtData 1) = "t" then [(List.hd txtData, false, hash (Text (fileContent)), Text (fileContent))]
        else [(List.hd txtData, true, (List.nth txtData 2), read_directory_object ( List.nth txtData 2 ))]
    | hd::tl ->
        let txtData = String.split_on_char ';' hd in
        let fileName = List.hd (String.split_on_char '\r' (List.nth txtData 2)) in
        let fileContent = read_text_object fileName in
        if (List.nth txtData 1) = "t" then (List.hd txtData, false, hash (Text (fileContent)), Text (fileContent))::(createDirObj(tl))
        else (List.hd txtData, true, (List.nth txtData 2), read_directory_object ( List.nth txtData 2 ))::(createDirObj(tl))
  in
  Directory (createDirObj splitData)

let clean_work_directory () = failwith "TODO ( clean_work_directory )"

let read_directory_object _h = failwith "TODO ( read_directory_object )" 
  
let clean_work_directory () = 
  
  let rec clearDir dir =
    let currentDirArray = Sys.readdir dir in 

    for i = 0 to ((Array.length currentDirArray)-1) do 
      let currentFile = currentDirArray.(i) in 
      let firstChar = String.get currentFile 0 in

      if(firstChar <> '.') then 

        if !(Sys.is_directory currentFile) then Sys.remove currentFile
        else try Sys.rmdir currentFile with | _ -> clearDir dir ^ "/" ^ currentFile
        
  clearDir "./"

let restore_work_directory _obj = failwith "TODO ( restore_work_directory )"

let merge_work_directory_I _obj = failwith "TODO ( merge_work_directory_I )"
