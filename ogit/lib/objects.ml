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

(** read all text from file *)
let read_all filename =
  let treatIc ic = In_channel.input_all ic in
  In_channel.with_open_text filename treatIc

(** write all input text to output file *)
let write_all filename ~data =
  let treatOc ic = Out_channel.output_string ic data in
  Out_channel.with_open_text filename treatOc


let printList (a: string list) = print_string ("[\"" ^ (String.concat "\"; \"" a) ^ "\"]")

let rec stringOfObject obj = match obj with
| Text s -> Format.sprintf "Text('%s')" s
| Directory(l) -> 
  l |> List.sort compare |> List.map (fun (s,d,h,obj) -> Format.sprintf "(%s,%b,%s,%s)" s d (Digest.to_hex h) (stringOfObject obj))
  |> String.concat ";" 
  |> Format.sprintf "Directory[%s]"

let rec hash _obj =
  match _obj with
  | Text text -> Digest.string text
  | Directory ([]) -> Digest.string ""
  | Directory ((name, isDir, _, t)::tl) ->
    if isDir then Digest.string (name ^ ";d;" ^ (hash t) ^ (hash (Directory(tl))))
    else Digest.string (name ^ ";t;" ^ (hash t) ^ (hash (Directory(tl))))

let is_known _h = Sys.file_exists (".ogit/objects/" ^ _h)

let store_object _obj = 

  let rec writeFolder list fileToWriteIn = match list with
    | ((name, isDir, h, _)::tl) -> if(isDir) 
      then 
      (ignore (Sys.command ("echo '" ^ name ^ ";d;" ^ h ^ "' >> " ^ fileToWriteIn)); writeFolder tl fileToWriteIn) 
      else 
      (ignore (Sys.command ("echo '" ^ name ^ ";t;" ^ h ^ "' >> " ^ fileToWriteIn)); writeFolder tl fileToWriteIn)
    | [] -> ()
  in

  let name = hash _obj in
  match _obj with
    (* 
       Si c'est un fichier, on crée un fichier texte dans objects et on y écrit son contenu 
       Si c'est un dossier, on crée un fichier texte dans objects et on y écrit ligne par ligne le nom;[d ou t selon type du fichier];hash de chaque fichier du dossier
       Et la fonction renvoie le Digest.t de l'objet   
    *)
    | Text(contenu) -> (write_all ("./.ogit/objects/" ^ name) ~data:contenu; name)
    | Directory ([]) -> (ignore (Sys.command ("touch ./.ogit/objects/" ^ name)); name)
    | Directory([(nameF, isDir, h, _)]) -> if(isDir) then (write_all ("./.ogit/objects/" ^ name) ~data:(nameF ^ ";d;" ^ h); h) else (write_all ("./.ogit/objects/" ^ name) ~data:(nameF ^ "t" ^ h); h)
    | Directory(list) -> 
      let fileToWriteIn =  "./.ogit/objects/" ^ name in
      ignore (Sys.command ("touch ./.ogit/objects/" ^ name));
      writeFolder list fileToWriteIn;
      name
    

let read_text_object _h = 
  read_all (".ogit/objects/" ^ (Digest.to_hex _h))

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
            let fileContent = read_all (dir ^ "/" ^ currentFile) in
            let hashedFile = Digest.string fileContent in
            files := (currentFile ^ ";t;" ^ (Digest.to_hex hashedFile)  ^ "\n")::!files;
            write_all (".ogit/objects/" ^ (Digest.to_hex hashedFile)) ~data:fileContent
          end
        end
    done;
    let fileContent = String.concat "" (List.sort compare !files) in
    let nFileContent = removeTrailingEndOfLine(fileContent ^ (String.concat "" (List.sort compare !directories))) in
    let nhashedDir = Digest.string nFileContent in
    write_all (".ogit/objects/" ^ (Digest.to_hex nhashedDir)) ~data:nFileContent;
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
        let fileContent = read_text_object (Digest.from_hex fileName) in
        if (List.nth txtData 1) = "t" then [(List.hd txtData, false, hash (Text (fileContent)), Text (fileContent))]
        else [(List.hd txtData, true, Digest.from_hex (List.nth txtData 2), read_directory_object (Digest.from_hex (List.nth txtData 2 )))]
    | hd::tl ->
        let txtData = String.split_on_char ';' hd in
        let fileName = List.hd (String.split_on_char '\r' (List.nth txtData 2)) in
        let fileContent = read_text_object (Digest.from_hex fileName) in
        if (List.nth txtData 1) = "t" then (List.hd txtData, false, hash (Text (fileContent)), Text (fileContent))::(createDirObj(tl))
        else (List.hd txtData, true, Digest.from_hex (List.nth txtData 2), read_directory_object (Digest.from_hex (List.nth txtData 2)))::(createDirObj(tl))
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
  let rec treatCurrentobj dir _nobj currentFileName = match _nobj with
    | Text text -> write_all (dir ^ "/" ^ currentFileName) ~data:text
    | Directory ([]) -> if Sys.file_exists (dir ^ "/" ^ currentFileName) = false then Sys.command ("mkdir " ^ (dir ^ "/" ^ currentFileName))  |> ignore;
    | Directory ([(name, isDir, _, content)]) ->
      if isDir then begin
        if Sys.file_exists dir = false then Sys.command ("mkdir " ^ dir)  |> ignore;
        treatCurrentobj dir content name
      end
      else treatCurrentobj dir content name
    | Directory ((name, isDir, md5, content)::tl) -> 
      if isDir then treatCurrentobj (dir ^ "/" ^ name) (Directory([(name, isDir, md5, content)])) name
      else write_all (dir ^ "/" ^ name) ~data:(match content with | Text text -> text | _ -> invalid_arg "Invalid content");
      treatCurrentobj dir (Directory(tl)) currentFileName
  in
  treatCurrentobj "." (Directory [("repo", true,  hash _obj, _obj)]) ""

let merge_work_directory_I _obj =
  let res = ref true in
  let treatFile ~remoteFileContent ~localFileName ~currentDir =
    if (Sys.file_exists (currentDir ^ "/" ^ localFileName)) then begin
      let file2 = read_all (currentDir ^ "/" ^ localFileName) in
      if(remoteFileContent <> file2) then begin
        write_all (currentDir ^ "/" ^ localFileName ^ ".cl") ~data:file2;
        write_all (currentDir ^ "/" ^ localFileName ^ ".cr") ~data:remoteFileContent;
        res := false
      end
    end
    else write_all (currentDir ^ "/" ^ localFileName) ~data:remoteFileContent
  in
  let rec aux ~currentDir ~obj ~name =
    match obj with
    | Text(text) -> treatFile ~currentDir:currentDir ~remoteFileContent:text ~localFileName:name
    | Directory([]) -> ()
    | Directory([(nameF, isDir, _, t)]) ->
      if isDir then begin
        if (Sys.file_exists (currentDir ^ "/" ^ nameF) = false) then Sys.command ("mkdir " ^ (currentDir)) |> ignore;
        aux ~currentDir:(currentDir ^ "/" ^ nameF) ~name:nameF ~obj:t
      end
      else
        aux ~currentDir:currentDir ~name:nameF ~obj:t
    | Directory((nameF, isDir, h, t)::tl) ->
      if isDir = false then begin
        aux ~currentDir:currentDir ~name:nameF ~obj:t;
        aux ~currentDir:currentDir ~name:name  ~obj:(Directory(tl))
      end else
        if (Sys.file_exists (currentDir ^ "/" ^ nameF) = false) then Sys.command ("mkdir " ^ (currentDir ^ "/" ^ nameF)) |> ignore;
        aux ~currentDir:currentDir ~name:nameF ~obj:(Directory([(nameF, isDir, h, t)]));
        aux ~currentDir:currentDir ~name:name  ~obj:(Directory(tl))
  in
  match _obj with
  | Text(_) -> failwith("L'objet en paramètre doit être un repertoire")
  | Directory([]) -> !res
  | Directory((nameF, isDir, h, t)::tl) -> aux ~currentDir:"." ~name:nameF ~obj:(Directory((nameF, isDir, h, t)::tl)); !res
  
