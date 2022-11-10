type t =
| Text of string
| Directory of (string * bool * Digest.t * t) list

let removeTrailingEndOfLine str =
    let strLen = String.length str in
    if strLen < 2 then str
    else begin
      let substring1 = String.sub str (strLen-1) 1 in
      if substring1 = "\n" then String.sub str 0 (strLen-1)
      else str
    end

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

let store_work_directory () : Digest.t =
  let rec treatCurrentDir dir : Digest.t =
    let currentDirArray = Sys.readdir dir in
    let directories = ref [] in
    let files = ref [] in
    for i=0 to (Array.length currentDirArray) - 1 do
      let currentFile = currentDirArray.(i) in
      let firstChar = String.get currentFile 0 in
        if(firstChar <> '.') then begin
          if (Sys.is_directory (dir ^ "/" ^ currentFile)) then begin
            let hashedDir = treatCurrentDir (dir ^ "/" ^ currentFile) in
            directories := (currentFile ^ ";d;" ^ (Digest.to_hex hashedDir) ^ "\n")::!directories;
          end
          else begin
            let fileContent = Core.In_channel.read_all (dir ^ "/" ^ currentFile) in
            let hashedFile = Digest.string fileContent in
            files := (currentFile ^ ";t;" ^ (Digest.to_hex hashedFile)  ^ "\n")::!files;
            Core.Out_channel.write_all (".ogit/objects/" ^ (Digest.to_hex hashedFile)) ~data:fileContent
          end
        end
    done;
    let fileContent = String.concat "" (List.sort compare !files) in
    let nFileContent = fileContent ^ (String.concat "" (List.sort compare !directories)) in
    let nhashedDir = Digest.string (removeTrailingEndOfLine nFileContent) in
    Core.Out_channel.write_all (".ogit/objects/" ^ (Digest.to_hex nhashedDir)) ~data:nFileContent;
    nhashedDir
  in
  treatCurrentDir "../repo"

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
  Directory (List.rev (createDirObj splitData))

let clean_work_directory () = 
  
  let rec clearDir dir =
    let currentDirArray = Sys.readdir dir in 
    for i = 0 to (Array.length currentDirArray) - 1 do
      let currentFile = currentDirArray.(i) in 
      let firstChar = String.get currentFile 0 in
      if firstChar <> '.' then begin
        if Sys.is_directory currentFile = false then
          Sys.remove currentFile
        else
          try Sys.command ("rm -r " ^currentFile) |> ignore with | _ -> clearDir (dir ^ "/" ^ currentFile)
      end
    done
  in
  clearDir "./"

let restore_work_directory _obj =
  let rec treatCurrentobj dir _nobj currentFileName =
    match _nobj with
    | Text text -> Core.Out_channel.write_all (dir ^ "/" ^ currentFileName) ~data:text
    | Directory ([]) -> if Sys.file_exists (dir ^ "/" ^currentFileName) = false then Sys.command ("mkdir " ^ (dir ^ "/" ^currentFileName))  |> ignore;
    | Directory ([(name, isDir, _, content)]) ->
      if isDir then begin
        if Sys.file_exists dir = false then Sys.command ("mkdir " ^ dir)  |> ignore;
        treatCurrentobj dir content name
      end
      else treatCurrentobj dir content name
    | Directory ((name, isDir, md5, content)::tl) -> 
      if isDir then treatCurrentobj (dir ^ "/" ^ name) (Directory([(name, isDir, md5, content)])) name
      else Core.Out_channel.write_all (dir ^ "/" ^ name) ~data:(match content with | Text text -> text | _ -> invalid_arg "Invalid content");
      treatCurrentobj dir (Directory(tl)) currentFileName
  in
  treatCurrentobj "." (Directory [("repo", true,  hash _obj, _obj)]) ""

let merge_work_directory_I _obj = failwith "TODO ( merge_work_directory_I )"
