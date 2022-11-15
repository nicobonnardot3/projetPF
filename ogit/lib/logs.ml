type commit = {
    parents : Digest.t list;
    date : float;
    message : string;
    content : Digest.t
}
    
let date_fm _d = 
    let time = Unix.localtime _d in
    Printf.sprintf "%02d:%02d:%02d-%02d-%02d-%04d" time.tm_hour time.tm_min time.tm_sec time.tm_mday (time.tm_mon + 1) (time.tm_year + 1900) 
        
let set_head _l =
    let content = (List.fold_left (fun x y -> x ^ ";" ^ y) "" _l) in
    let len = String.length content in
    let treatOc ic = Out_channel.output_string ic (String.sub content 1 (len - 1) ) in
    Out_channel.with_open_text ".ogit/HEAD" treatOc
    

let get_head () = 
    let treatIc ic = In_channel.input_all ic in
    In_channel.with_open_text ".ogit/HEAD" treatIc

let make_commit _s _h = 
    let head = get_head () in
    let parents = String.split_on_char ';' head in
    let date = Unix.time () in
    {parents; date; message = _s; content = _h}

let init_commit () = failwith "TODO"

let store_commit _c = failwith "TODO"

let read_commit _h = failwith "TODO" 