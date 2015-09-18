open Lwt

let program =
  Stripe.make None |> return >>= fun handle ->
  (* If I use 1.0 it fucks up *)
  let charge_id = "ch_16mHMRJDURztdKY9c5rqO1YR" in
  (* Stripe.Charges.create ~description:"Hello World" (`Int 450) `USD authing handle >>= fun j -> *)
  Stripe.Charges.retrieve charge_id handle  >>= fun j ->
  (* Stripe.Customers.list_all ~limit:3 handle >>= fun j -> *)
  Yojson.Basic.pretty_to_string j |> Lwt_io.printl

let () =
  Lwt_main.run program
