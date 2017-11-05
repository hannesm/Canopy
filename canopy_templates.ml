open Canopy_config
open Canopy_utils
open Tyxml.Html

let taglist tags =
  let format_tag tag =
    let taglink = Printf.sprintf "/tags/%s" in
    a ~a:[taglink tag |> a_href; a_class ["tag"]] [pcdata tag] in
  match tags with
  | [] -> []
  | tags ->
     let tags = List.map format_tag tags in
     [ div ~a:[a_class ["tags"]] ([pcdata "Classified under: "] ++ tags) ]

let links keys =
  let format_link link =
    li [ a ~a:[a_href ("/" ^ link)] [span [pcdata link]]] in
  List.map format_link keys

let main ~cache ~content ?footer ~title ~keys =
  let idx = index_page cache in
  let links = links keys in
  let footer = match footer with
    | None -> []
    | Some f ->
      let html = Omd.to_html (Omd.of_string f) in
      [ div ~a:[a_class ["footer"]] [Unsafe.data html] ]
  in
  let page =
    html
      (head
         (Tyxml.Html.title (pcdata title))
         ([
           meta ~a:[a_charset "UTF-8"] ();
           (* link ~rel:[`Stylesheet] ~href:"/static/css/bootstrap.min.css" (); *)
           link ~rel:[`Stylesheet] ~href:"/static/css/style.css" ();
           (* link ~rel:[`Stylesheet] ~href:"/static/css/highlight.css" (); *)
           (* script ~a:[a_src "/static/js/highlight.pack.js"] (pcdata ""); *)
           (* script (pcdata "hljs.initHighlightingOnLoad();"); *)
           link ~rel:[`Alternate] ~href:"/atom" ~a:[a_title title; a_mime_type "application/atom+xml"] ();
         ])
      )
      (body
         ([
           nav ~a:[a_class ["navbar navbar-default navbar-fixed-top"]] [
             div ~a:[a_class ["container"]] [
               div ~a:[a_class ["navbar-header"]] [
                 a ~a:[a_class ["navbar-brand"]; a_href ("/" ^ idx)][pcdata (blog_name cache)]
               ];
               div ~a:[a_class ["collapse navbar-collapse collapse"]] [
                 ul ~a:[a_class ["nav navbar-nav navbar-right"]] links
               ]
             ]
           ];
           main [
             div ~a:[a_class ["flex-container"]] content
           ]
         ] @ footer)
      )
  in
  let buf = Buffer.create 500 in
  let fmt = Format.formatter_of_buffer buf in
  pp () fmt page ;
  Buffer.contents buf

let listing entries =
  [div ~a:[a_class ["flex-container"]] [
	 div ~a:[a_class ["list-group listing"]] entries
       ]
  ]

let error msg =
  [div ~a:[a_class ["alert alert-danger"]] [pcdata msg]]
