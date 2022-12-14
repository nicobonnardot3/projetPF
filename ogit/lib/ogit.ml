(** fichier ogit.ml : le coeur de l'exécutable: parse la ligne de commande et appelle les commandes correspondantes (cf https://v2.ocaml.org/api/Arg.html) **)

open Ogitlib
open Commands

let usage_msg = "usage: ogit command [options]"
let command = ref ""
let msg = ref ""
let branch = ref ""

let help_msg = "commands are :
    - init: initialize a new repository\n
    - commit <msg>: commit the current state of the repository
        options:
        -msg: commit message \n
    - checkout <branch>: checkout a branch
        options:
        -branch: branch to merge from\n
    - merge <branch>: merge a branch into the current branch
        options:
        -branch: branch to merge from\n
    - log: Not yet implemented
    "

let () = 
    Arg.parse [("-msg", Arg.Set_string msg, "commit message"); ("-branch", Arg.Set_string branch, "Branch to checkout to / merge")] (fun cmd -> command := cmd) usage_msg;
    match !command with
    | "init" -> ogit_init ()
    | "commit" -> ogit_commit !msg
    | "checkout" -> ogit_checkout !branch
    | "merge" -> ogit_merge !branch
    | "log" -> ogit_log ()
    | "help" -> print_endline help_msg
    | _ -> print_endline help_msg