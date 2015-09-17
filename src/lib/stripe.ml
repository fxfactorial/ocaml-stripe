(** Stripe API for OCaml, each module corresponds to Stripe's REST
    method **)

open Lwt

type t = { authed_headers : Cohttp.Header.t;
           end_point : string; }

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
    { authed_headers = Sys.getenv "STRIPE_KEY" |> auth_me;
      end_point = "https://api.stripe.com/v1/"}

module Charges = struct

  let customize = function
    | h -> {h with end_point = h.end_point ^ "charges"}

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
