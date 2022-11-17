(** fichier commands.ml **)
(** fonctions représentant les commandes ogit **)

let ogit_init () = 
  if Sys.file_exists ".ogit" = false then begin
  Sys.command "mkdir .ogit" |> ignore;
  Sys.command "mkdir .ogit/objects" |> ignore;
  Sys.command "mkdir .ogit/logs" |> ignore;
  Logs.set_head [Objects.store_work_directory ()];
  Logs.store_commit (Logs.init_commit ()) |> ignore
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
  Objects.restore_work_directory (Objects.read_directory_object (Digest.from_hex _hash));
  Logs.set_head [Digest.from_hex _hash]

let ogit_log () = failwith "TODO"

let ogit_merge _hash = failwith "TODO"