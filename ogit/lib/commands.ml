(** fichier commands.ml **)
(** fonctions représentant les commandes ogit **)

let ogit_init () = 
  if Sys.file_exists ".ogit" = false then begin
  Sys.command "mkdir .ogit" |> ignore;
  Sys.command "touch .ogit/HEAD" |> ignore;
  Sys.command "mkdir .ogit/objects" |> ignore;
  Sys.command "mkdir .ogit/logs" |> ignore;
  let commitHash = Logs.store_commit (Logs.init_commit ()) in
  Logs.set_head [commitHash]
  end
  else failwith "Already initialized"

let ogit_commit _msg = 
  let commit = {
    Logs.parents = Logs.get_head ();
    date = Unix.time ();
    message = _msg;
    content = Objects.store_work_directory ()} in
  let commitHash = Logs.store_commit commit in
  Logs.set_head [commitHash]

let ogit_checkout _hash = 
  Objects.clean_work_directory ();
  let commit = Logs.read_commit (Digest.from_hex _hash) in
  Objects.restore_work_directory (Objects.read_directory_object commit.content);
  Logs.set_head [Digest.from_hex _hash]

let ogit_log () = failwith "TODO"

let ogit_merge remoteHash =
  let rec check_ancesters (list: Digest.t list) (clist: string list) =
    match list with
    | [] -> true
    | [elem] -> begin
        let nElem = Digest.to_hex elem in
        try
          let commit = Logs.read_commit elem in
          if List.mem nElem clist then false
          else check_ancesters commit.parents clist
        with Logs.CommitNotFound _ -> true
      end
    | hd::tl ->
        check_ancesters [hd] clist &&
        check_ancesters tl clist
  in
  let head = Logs.get_head () in
  Logs.set_head ((Digest.from_hex remoteHash)::head);
  if Sys.file_exists (".ogit/logs/" ^ remoteHash) = false then Printf.printf "Branch %s doesn't exist!" remoteHash
  else if  check_ancesters head [remoteHash] = false then begin
     Printf.printf "Same branch!";
     Logs.set_head head
  end
  else
    let commit = Logs.read_commit (Digest.from_hex remoteHash) in
    let remoteObj = Objects.read_directory_object (commit.content) in
    if Objects.merge_work_directory_I remoteObj = false then begin 
      Printf.printf "Automatic merge failed; fix conflicts and then commit the result\n"
    end else begin
    let commit = {
        Logs.parents = Logs.get_head ();
        date = Unix.time ();
        message = "Merged branch " ^ remoteHash ^ " into HEAD";
        content = Objects.store_work_directory ()
      } in
      Logs.set_head [Logs.store_commit commit];
    end