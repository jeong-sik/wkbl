(** Type-safe HTML DSL using Buffer combinators.

    Core types: [html = Buffer.t -> unit] and [attr = Buffer.t -> unit].
    Zero-copy composition via continuation passing — each combinator
    appends directly to a shared buffer without intermediate strings.

    HTMX attributes are first-class. Phase 5 island markers are inert. *)

(** A fragment that writes HTML into a buffer. *)
type html = Buffer.t -> unit

(** An attribute that writes into a buffer (space-prefixed). *)
type attr = Buffer.t -> unit

(* ── Internal helpers ─────────────────────────────── *)

let escape_html s =
  let needs_escape = ref false in
  for i = 0 to String.length s - 1 do
    match String.unsafe_get s i with
    | '<' | '>' | '&' | '"' | '\'' -> needs_escape := true
    | _ -> ()
  done;
  if not !needs_escape then s
  else begin
    let buf = Buffer.create (String.length s + 8) in
    String.iter (function
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '&' -> Buffer.add_string buf "&amp;"
      | '"' -> Buffer.add_string buf "&quot;"
      | '\'' -> Buffer.add_string buf "&#x27;"
      | c -> Buffer.add_char buf c) s;
    Buffer.contents buf
  end

let write_attrs buf attrs =
  List.iter (fun (a : attr) -> a buf) attrs

let write_children buf children =
  List.iter (fun (c : html) -> c buf) children

(* ── Text nodes ───────────────────────────────────── *)

let text s buf =
  Buffer.add_string buf (escape_html s)

let raw s buf =
  Buffer.add_string buf s

let int n buf =
  Buffer.add_string buf (string_of_int n)

let float_ ?(decimals=1) f buf =
  Buffer.add_string buf (Printf.sprintf "%.*f" decimals f)

let empty _ = ()

(* ── Combinators ──────────────────────────────────── *)

let concat children buf =
  List.iter (fun c -> c buf) children

let list = concat

let maybe f = function
  | None -> empty
  | Some v -> f v

let when_ cond child =
  if cond then child else empty

(* ── Attributes ───────────────────────────────────── *)

let attr name value buf =
  Buffer.add_char buf ' ';
  Buffer.add_string buf name;
  Buffer.add_string buf "=\"";
  Buffer.add_string buf (escape_html value);
  Buffer.add_char buf '"'

let cls value = attr "class" value
let id value = attr "id" value
let href value = attr "href" value
let src value = attr "src" value
let alt value = attr "alt" value
let style value = attr "style" value
let title value = attr "title" value
let type_ value = attr "type" value
let name value = attr "name" value
let value value = attr "value" value
let placeholder value = attr "placeholder" value
let action value = attr "action" value
let method_ value = attr "method" value
let for_ value = attr "for" value
let role value = attr "role" value
let tabindex n = attr "tabindex" (string_of_int n)
let aria label value = attr ("aria-" ^ label) value
let data key value = attr ("data-" ^ key) value

(** Boolean attribute (no value). *)
let flag name buf =
  Buffer.add_char buf ' ';
  Buffer.add_string buf name

let disabled = flag "disabled"
let checked = flag "checked"
let selected = flag "selected"
let required = flag "required"
let defer = flag "defer"
let async_ = flag "async"

(* ── HTMX first-class ────────────────────────────── *)

let hx_get url = attr "hx-get" url
let hx_post url = attr "hx-post" url
let hx_put url = attr "hx-put" url
let hx_delete url = attr "hx-delete" url
let hx_trigger value = attr "hx-trigger" value
let hx_swap value = attr "hx-swap" value
let hx_target value = attr "hx-target" value
let hx_push_url value = attr "hx-push-url" value
let hx_indicator value = attr "hx-indicator" value
let hx_vals value = attr "hx-vals" value
let hx_select value = attr "hx-select" value
let hx_confirm value = attr "hx-confirm" value

(* ── Phase 5 island marker (inert in Phase 4) ────── *)

let island name = data "island" name

(* ── Elements ─────────────────────────────────────── *)

(** Generic element: [el "div" attrs children]. *)
let el tag attrs children buf =
  Buffer.add_char buf '<';
  Buffer.add_string buf tag;
  write_attrs buf attrs;
  Buffer.add_char buf '>';
  write_children buf children;
  Buffer.add_string buf "</";
  Buffer.add_string buf tag;
  Buffer.add_char buf '>'

(** Void element (self-closing): [void_el "img" attrs]. *)
let void_el tag attrs buf =
  Buffer.add_char buf '<';
  Buffer.add_string buf tag;
  write_attrs buf attrs;
  Buffer.add_char buf '>'

(* Common elements *)
let div = el "div"
let span = el "span"
let p = el "p"
let h1 = el "h1"
let h2 = el "h2"
let h3 = el "h3"
let h4 = el "h4"
let h5 = el "h5"
let h6 = el "h6"
let a = el "a"
let nav = el "nav"
let header = el "header"
let footer = el "footer"
let main = el "main"
let section = el "section"
let article = el "article"
let aside = el "aside"

(* Table elements *)
let table = el "table"
let thead = el "thead"
let tbody = el "tbody"
let tfoot = el "tfoot"
let tr = el "tr"
let th = el "th"
let td = el "td"

(* List elements *)
let ul = el "ul"
let ol = el "ol"
let li = el "li"
let dl = el "dl"
let dt = el "dt"
let dd = el "dd"

(* Form elements *)
let form = el "form"
let label = el "label"
let button = el "button"
let select = el "select"
let option = el "option"
let textarea = el "textarea"

(* Void elements *)
let img attrs = void_el "img" attrs
let input attrs = void_el "input" attrs
let br attrs = void_el "br" attrs
let hr attrs = void_el "hr" attrs
let meta attrs = void_el "meta" attrs
let link attrs = void_el "link" attrs

(* Script/style *)
let script attrs children = el "script" attrs children
let style_el attrs children = el "style" attrs children

(* ── Rendering ────────────────────────────────────── *)

let to_string (h : html) =
  let buf = Buffer.create 4096 in
  h buf;
  Buffer.contents buf

let to_buffer (h : html) buf =
  h buf
