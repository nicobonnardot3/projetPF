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
  let rec check_if_not_ancester (list: Digest.t list) (clist: string list) =
    Objects.printList list;
    match list with
    | [] -> true
    | [elem] -> begin
        let nElem = Digest.to_hex elem in
        try
          let commit = Logs.read_commit elem in
          if List.mem nElem clist then false
          else check_if_not_ancester commit.parents clist
        with Logs.CommitNotFound _ -> true
      end
    | hd::tl ->
        check_if_not_ancester [hd] clist &&
        check_if_not_ancester tl clist
  in
  let head = Logs.get_head () in
  if Sys.file_exists (".ogit/logs/" ^ remoteHash) = false then failwith ( "Branch " ^ remoteHash ^ " doesn't exist!" )
  else if  check_if_not_ancester head [remoteHash] = false then failwith "Same branch!"
  else 
    try
      let commit = Logs.read_commit (Digest.from_hex remoteHash) in
      let remoteObj = Objects.read_directory_object (commit.content) in
      if Objects.merge_work_directory_I remoteObj = false then failwith "There are conflicts to resolve!"
      else Logs.set_head ((Digest.from_hex remoteHash)::head)
    with Logs.CommitNotFound _ -> 
      let remoteObj = Objects.Directory ([]) in
      if Objects.merge_work_directory_I remoteObj = false then failwith "There are conflicts to resolve!"
      else Logs.set_head ((Digest.from_hex remoteHash)::head)