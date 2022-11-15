type commit = {
    parents : Digest.t list;
    date : float;
    message : string;
    content : Digest.t
}
    
let date_fm _d = 
    let time = Unix.localtime _d in
    Printf.sprintf "%02d:%02d:%02d-%02d/%02d/%04d" time.tm_hour time.tm_min time.tm_sec time.tm_mday (time.tm_mon + 1) (time.tm_year + 1900) 
        
let set_head _l =
    let content = (List.fold_left (fun x y -> x ^ ";" ^ y) "" (List.map Digest.to_hex _l)) in
    let len = String.length content in
    let treatOc ic = Out_channel.output_string ic (String.sub content 1 (len - 1) ) in
    Out_channel.with_open_text ".ogit/HEAD" treatOc
    
let get_head () = 
    let treatIc ic = In_channel.input_all ic in
    let head = In_channel.with_open_text ".ogit/HEAD" treatIc in
    let tmpstr = String.split_on_char ';' head in
    List.map Digest.from_hex tmpstr

let make_commit _s _h = 
    let parents = get_head () in
    let date = Unix.time () in
    {parents; date; message = _s; content = _h}

let init_commit () = 
    let workDirHash = Objects.store_work_directory () in
    make_commit "Initial commit" workDirHash

let store_commit _c = 
    let tmpstr = (List.fold_left (fun x y -> x ^ ";" ^ y) "" (List.map Digest.to_hex _c.parents)) in
    let parents = String.sub tmpstr 1 (String.length tmpstr - 1)  in
    let date = date_fm _c.date in
    let content = parents ^ "\n" ^ date ^ "\n" ^ _c.message ^ "\n" ^ (Digest.to_hex _c.content) in
    
    let path = (".ogit/logs/" ^ (Digest.to_hex (Digest.string content))) in
    let treatOc ic = Out_channel.output_string ic content in
    Out_channel.with_open_text path treatOc;
    Digest.string content

let read_commit _h = 
    let fileName = (".ogit/logs/" ^ (Digest.to_hex _h)) in
    let treatIc ic = In_channel.input_all ic in
    let content = In_channel.with_open_text fileName treatIc in
    let lines = String.split_on_char '\n' content in
    let parents = List.map Digest.from_hex (String.split_on_char ';' (List.nth lines 0)) in
    let dateValues = String.split_on_char '-' (List.nth lines 1) in
    let dayValues = String.split_on_char ':' (List.nth dateValues 0) in
    let yearValues = String.split_on_char '/' (List.nth dateValues 1) in
    let date = fst (Unix.mktime {
        Unix.tm_sec = int_of_string (List.nth dayValues 2); 
        tm_min = int_of_string (List.nth dayValues 1); 
        tm_hour = int_of_string (List.nth dayValues 0); 
        tm_mday = int_of_string (List.nth yearValues 0); 
        tm_mon = int_of_string (List.nth yearValues 1) - 1; 
        tm_year = int_of_string (List.nth yearValues 2) - 1900; 
        tm_wday = 0; tm_yday = 0; 
        tm_isdst = false}) in
    let message = List.nth lines 2 in
    let content = Digest.from_hex (List.nth lines 3) in
    {parents; date; message; content}