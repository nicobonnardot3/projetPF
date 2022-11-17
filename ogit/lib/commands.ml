(** fichier commands.ml **)
(** fonctions représentant les commandes ogit **)

let ogit_init () = 
  if Sys.file_exists ".ogit" = false then begin
  Sys.command "mkdir .ogit" |> ignore;
  Sys.command "mkdir .ogit/objects" |> ignore;
  Sys.command "mkdir .ogit/logs" |> ignore;
  set_head [store_work_directory ()];
  store_commit (init_commit ()) |> ignore
  end
  else failwith "Already initialized"

let ogit_commit _msg = 
  let commit = {
    parents = get_head ();
    date = Unix.time ();
    message = _msg;
    content = store_work_directory ()} in
  let commitHash = store_commit commit in
  set_head [commitHash]

let ogit_checkout _hash = 
  clean_work_directory ();
  restore_work_directory (read_directory_object (Digest.from_hex _hash));
  set_head [Digest.from_hex _hash]

let ogit_log () = failwith "TODO"

let ogit_merge _hash = failwith "TODO"