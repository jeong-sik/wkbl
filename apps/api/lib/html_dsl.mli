(** Type-safe HTML DSL using Buffer combinators.

    Core abstraction: [html = Buffer.t -> unit].
    Compose HTML fragments via function application — zero intermediate strings.
    All [text] output is auto-escaped. Use [raw] only for trusted literals. *)

(** {1 Core Types} *)

(** An HTML fragment. Writes content into a buffer when applied. *)
type html = Buffer.t -> unit

(** An HTML attribute. Writes a space-prefixed attribute into a buffer. *)
type attr = Buffer.t -> unit

(** {1 Text Nodes} *)

val text : string -> html
(** [text s] renders [s] with HTML entity escaping (XSS-safe). *)

val raw : string -> html
(** [raw s] renders [s] verbatim. Use only for trusted/pre-escaped content. *)

val int : int -> html
(** [int n] renders the integer [n] as text. *)

val float_ : ?decimals:int -> float -> html
(** [float_ ~decimals f] renders [f] with [decimals] decimal places (default 1). *)

val empty : html
(** Produces no output. Identity for [concat]. *)

(** {1 Combinators} *)

val concat : html list -> html
(** [concat cs] renders each child in order. *)

val list : html list -> html
(** Alias for [concat]. *)

val maybe : ('a -> html) -> 'a option -> html
(** [maybe f opt] renders [f v] if [opt = Some v], else [empty]. *)

val when_ : bool -> html -> html
(** [when_ cond h] renders [h] if [cond] is true, else [empty]. *)

(** {1 Attributes} *)

val attr : string -> string -> attr
(** [attr name value] renders [ name="escaped-value"]. *)

val cls : string -> attr
val id : string -> attr
val href : string -> attr
val src : string -> attr
val alt : string -> attr
val style : string -> attr
val title : string -> attr
val type_ : string -> attr
val name : string -> attr
val value : string -> attr
val placeholder : string -> attr
val action : string -> attr
val method_ : string -> attr
val for_ : string -> attr
val role : string -> attr
val tabindex : int -> attr
val aria : string -> string -> attr
val data : string -> string -> attr

(** Boolean attributes (no value). *)

val flag : string -> attr
val disabled : attr
val checked : attr
val selected : attr
val required : attr
val defer : attr
val async_ : attr

(** {1 HTMX Attributes} *)

val hx_get : string -> attr
val hx_post : string -> attr
val hx_put : string -> attr
val hx_delete : string -> attr
val hx_trigger : string -> attr
val hx_swap : string -> attr
val hx_target : string -> attr
val hx_push_url : string -> attr
val hx_indicator : string -> attr
val hx_vals : string -> attr
val hx_select : string -> attr
val hx_confirm : string -> attr

(** {1 Phase 5 Island Marker} *)

val island : string -> attr
(** [island name] renders [data-island="name"].
    island-loader.js discovers these markers and loads
    the corresponding Wasm module from /static/wasm/{name}/. *)

(** {1 Elements} *)

val el : string -> attr list -> html list -> html
(** [el tag attrs children] renders [<tag attrs>children</tag>]. *)

val void_el : string -> attr list -> html
(** [void_el tag attrs] renders [<tag attrs>] (self-closing). *)

val div : attr list -> html list -> html
val span : attr list -> html list -> html
val p : attr list -> html list -> html
val h1 : attr list -> html list -> html
val h2 : attr list -> html list -> html
val h3 : attr list -> html list -> html
val h4 : attr list -> html list -> html
val h5 : attr list -> html list -> html
val h6 : attr list -> html list -> html
val a : attr list -> html list -> html
val nav : attr list -> html list -> html
val header : attr list -> html list -> html
val footer : attr list -> html list -> html
val main : attr list -> html list -> html
val section : attr list -> html list -> html
val article : attr list -> html list -> html
val aside : attr list -> html list -> html

val table : attr list -> html list -> html
val thead : attr list -> html list -> html
val tbody : attr list -> html list -> html
val tfoot : attr list -> html list -> html
val tr : attr list -> html list -> html
val th : attr list -> html list -> html
val td : attr list -> html list -> html

val ul : attr list -> html list -> html
val ol : attr list -> html list -> html
val li : attr list -> html list -> html
val dl : attr list -> html list -> html
val dt : attr list -> html list -> html
val dd : attr list -> html list -> html

val form : attr list -> html list -> html
val label : attr list -> html list -> html
val button : attr list -> html list -> html
val select : attr list -> html list -> html
val option : attr list -> html list -> html
val textarea : attr list -> html list -> html

val img : attr list -> html
val input : attr list -> html
val br : attr list -> html
val hr : attr list -> html
val meta : attr list -> html
val link : attr list -> html

val script : attr list -> html list -> html
val style_el : attr list -> html list -> html

(** {1 Rendering} *)

val to_string : html -> string
(** Render an HTML fragment to a string. Allocates a 4 KB buffer. *)

val to_buffer : html -> Buffer.t -> unit
(** Append an HTML fragment to an existing buffer (zero extra allocation). *)
