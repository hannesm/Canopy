open Canopy_utils
open Tyxml.Html

type t = {
  title : string option;
  content : string;
  author : string option;
  abstract : string option;
  uri : string;
  created: Ptime.t;
  updated: Ptime.t;
  tags: string list;
  uuid: string;
}

let of_string base_uuid meta uri created updated content =
  try
    let split_tags = Re_str.split (Re_str.regexp ",") in
    let content = Omd.to_html (Omd.of_string content) in
    let author = assoc_opt "author" meta in
    let title = assoc_opt "title" meta in
    let tags = assoc_opt "tags" meta |> map_opt split_tags [] |> List.map String.trim in
    let abstract = match assoc_opt "abstract" meta with
        | None -> None
        | Some x -> Some (Omd.to_html (Omd.of_string x))
    in
    let uuid =
      let open Uuidm in
      let stamp = Ptime.to_rfc3339 created in
      let entry_id = to_string (v5 (create (`V5 (ns_dns, stamp))) base_uuid) in
      Printf.sprintf "urn:uuid:%s" entry_id
    in
    Some {title; content; author; uri; abstract; created; updated; tags; uuid}
  with
  | _ -> None

let to_tyxml article =
  let title = match article.title with
    | None -> []
    | Some t -> [ h2 [ pcdata t ] ]
  in
  let author = match article.author with
    | None -> []
    | Some auth ->
      [ span ~a:[a_class ["author"]] [pcdata ("Written by " ^ auth)] ; br () ]
  in
  let created = ptime_to_pretty_date article.created in
  let updated = ptime_to_pretty_date article.updated in
  let updated = String.concat " "
      [ "Published:" ; created ; "(last updated:" ; updated ^ ")" ]
  in
  let tags = Canopy_templates.taglist article.tags in
  [div ~a:[a_class ["post"]] (
      title @ author @
      tags @ [
      span ~a:[a_class ["date"]] [pcdata updated] ;
      Tyxml.Html.article [Unsafe.data article.content]
    ])]

let to_tyxml_listing_entry article =
  let title = match article.title with
    | None -> []
    | Some t -> [ h2 ~a:[a_class ["list-group-item-heading"]] [pcdata t] ]
  in
  let author = match article.author with
    | None -> []
    | Some auth ->
      [ span ~a:[a_class ["author"]] [pcdata ("Written by " ^ auth)] ; br () ]
  in
  let abstract = match article.abstract with
    | None -> []
    | Some abstract -> [p ~a:[a_class ["list-group-item-text abstract"]] [Unsafe.data abstract]] in
  let content = title @ author in
  a ~a:[a_href ("/" ^ article.uri); a_class ["list-group-item"]] (content ++ abstract)

let to_tyxml_tags tags =
  let format_tag tag =
    let taglink = Printf.sprintf "/tags/%s" in
    a ~a:[taglink tag |> a_href; a_class ["list-group-item"]] [pcdata tag] in
  let html = match tags with
    | [] -> div []
    | tags ->
      let tags = List.map format_tag tags in
      p ~a:[a_class ["tags"]] tags
  in
  [div ~a:[a_class ["post"]] [
      h2 [pcdata "Tags"];
      div ~a:[a_class ["list-group listing"]] [html]]]

let to_atom cache ({ title; author; abstract; uri; created; updated; tags; content; uuid}) =
  let text x : Syndic.Atom.text_construct = Syndic.Atom.Text x in
  let summary = match abstract with
    | Some x -> Some (text x)
    | None -> None
  in
  let root = Canopy_config.root cache
  in
  let categories =
    List.map
      (fun x -> Syndic.Atom.category ~scheme:(Uri.of_string (root ^ "/tags/" ^ x)) x)
      tags
  in
  let author = match author with None -> "canopy" | Some a -> a in
  let title = match title with None -> "no title" | Some t -> t in
  Syndic.Atom.entry
    ~id:(Uri.of_string uuid)
    ~content:(Syndic.Atom.Html (None, content))
    ~authors:(Syndic.Atom.author author, [])
    ~title:(text title)
    ~published:created
    ~updated
    ?summary
    ~categories
    ~links:[Syndic.Atom.link ~rel:Syndic.Atom.Alternate (Uri.of_string uri)]
    ()
