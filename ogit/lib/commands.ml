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

let ogit_commit _msg = failwith "TODO"

let ogit_checkout _hash = failwith "TODO"

let ogit_log () = failwith "TODO"

let ogit_merge _hash = failwith "TODO"