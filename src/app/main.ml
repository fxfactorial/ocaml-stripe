open Lwt

let program =
  Stripe.make None |> return >>= fun handle ->
  Stripe.Customers.list_all ~limit:3 handle >>= fun j ->
  Yojson.Basic.pretty_to_string j |> Lwt_io.printl

let () =
  Lwt_main.run program
