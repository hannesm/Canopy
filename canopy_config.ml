open Canopy_utils

exception Required_config of string

let decompose_git_url url =
    match String.rindex url '#' with
    | exception Not_found -> (url, None)
    | i ->
      let remote_url = String.sub url 0 i in
      let branch = String.sub url (i + 1) (String.length url - i - 1) in
      (remote_url, Some branch)

let remote_uri () = fst (decompose_git_url (Key_gen.remote ()))
let remote_branch () = snd (decompose_git_url (Key_gen.remote ()))

let index_page cache =
  match KeyMap.find_config_opt cache [".config";"index_page"] with
    None -> "Index"
  | Some p -> p

let blog_name cache =
  match KeyMap.find_config_opt cache [".config";"blog_name"] with
    None -> "Canopy"
  | Some n -> n

let uuid cache =
  match KeyMap.find_config_opt cache [".config";"uuid"] with
    None -> raise (Required_config "uuid")
  | Some u -> u

let root cache =
  match KeyMap.find_config_opt cache [".config";"root"] with
    None -> "http://localhost"
  | Some r -> r

let port () = Key_gen.port ()

let push_hook_path () = Key_gen.push_hook ()

let tls_port () = Key_gen.tls_port ()
