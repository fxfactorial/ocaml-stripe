(** Stripe API for OCaml, each module corresponds to Stripe's REST
    method *)

open Lwt

type t = { authed_headers : Cohttp.Header.t;
           end_point : string; }

(* TODO Need to do this by category or something *)
type currency = [ `AUD (** Australian Dollar *)
                | `CAD (** Canadian Dollar *)
                | `USD (** United States Dollar *)
                | `DKK (** Danish Krone *)
                | `NOK (** Norwegian Krone *)
                | `SEK (** Swedish Krona *)
                | `EUR (** Euro *)
                | `GBP (** British Pound *) ]

let string_of_currency (c: currency) = match c with
  | `AUD -> "aud"
  | `CAD -> "cad"
  | `USD -> "usd"
  | `DKK -> "dkk"
  | `NOK -> "nok"
  | `SEK -> "sek"
  | `EUR -> "eur"
  | `GBP -> "gbp"

let make ~a_key =
  let auth_me k =
    let starter = Cohttp.Header.init () in
    Cohttp.Auth.credential_of_string ("Bearer " ^ k)
    |> Cohttp.Header.add_authorization starter
  in
  match a_key with
  | Some key ->
    {authed_headers = auth_me key;
     end_point = "https://api.stripe.com/v1/"}
  | None ->
    { authed_headers =
        (try Sys.getenv "STRIPE_KEY" with Not_found ->
          failwith "Can't get environment variable STRIPE_KEY")
        |> auth_me;
      end_point = "https://api.stripe.com/v1/"}

(* Will come back to this, hassle for now *)
(* module type STRIPE_API_METHOD = sig *)

(*   val customize : t -> t *)

(* end *)

(* module Charges : sig *)

(*   include STRIPE_API_METHOD *)

(* end = struct *)
module Charges = struct

  type charge_t = [`Customer_id of string | `Source_token of string]
  type charge_val_t = [`Int of int | `Float of float]

  let customize = function
    | h -> {h with end_point = h.end_point ^ "charges"}

  let create ?description amount currency c_id handle =
    let specialized = customize handle in
    let this_uri = Uri.of_string specialized.end_point in
    [(match amount with
      | `Int i -> ("amount", string_of_int i);
      | `Float f -> ("amount", floor f |> int_of_float |> string_of_int));
     ("currency", string_of_currency currency);
     (match c_id with
      | `Customer_id id -> ("customer", id)
      | `Source_token tk -> ("source", tk))
    ] @ (match description with None -> [] | Some msg -> [("description", msg)])
    |> Uri.add_query_params' this_uri
    |> Cohttp_lwt_unix.Client.post ~headers:specialized.authed_headers >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >|= Yojson.Basic.from_string

  let retrieve charge_id handle =
    let specialized = customize handle in
    Uri.of_string (specialized.end_point ^ "/" ^ charge_id)
    |> Cohttp_lwt_unix.Client.get ~headers:specialized.authed_headers >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >|= Yojson.Basic.from_string

end

module Refunds = struct

  let customize = function
    | h -> {h with end_point = h.end_point ^ "refunds"}

end

module Customers = struct

  let customize = function
    | h -> {h with end_point = h.end_point ^ "customers"}

  let list_all ?limit handle =
    let specialized = customize handle in
    (match limit with
    | None -> Uri.of_string specialized.end_point
    | Some k ->
      assert (k > 0);
      Uri.of_string specialized.end_point |> Uri.add_query_param'
      |> fun u -> u ("limit", string_of_int k))
    |> Cohttp_lwt_unix.Client.get ~headers:specialized.authed_headers >>= fun (resp, body) ->
    Cohttp_lwt_body.to_string body >|= Yojson.Basic.from_string

end

module Cards = struct

  let customize = function
    | h -> {h with end_point = h.end_point ^ "cards"}

end

module Subscriptions = struct

  let customize = function
    | h -> {h with end_point = h.end_point ^ "subscriptions"}

end

module Plans = struct

  let customize = function
    | h -> {h with end_point = h.end_point ^ "plans"}

end

module Coupons = struct

  let customize = function
    | h -> {h with end_point = h.end_point ^ "plans"}

end

module Discounts = struct

  let customize = function
    | h -> {h with end_point = h.end_point ^ "discounts"}

end

module Invoices = struct

  let customize = function
    | h -> {h with end_point = h.end_point ^ "invoices"}

end

module Invoice_items = struct

  let customize = function
    | h -> {h with end_point = h.end_point ^ "invoiceitems"}

end

module Disputes = struct

  let customize = function
    | h -> {h with end_point = h.end_point ^ "disputes"}

end

module Transfers = struct

  let customize = function
    | h -> {h with end_point = h.end_point ^ "transfers"}

end

(* Something special about this one *)
(* module Transfers_reversals = struct *)
(*   let customize = function *)
(*     | h -> {h with end_point = h.end_point ^ "transfersreversals"} *)

(* end *)
