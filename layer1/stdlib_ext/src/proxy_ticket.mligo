[@private] let proxy_transfer (type vt whole_p) (mk_param : vt ticket -> whole_p) ( (p,_) : ((vt * nat) * address) * unit ) : operation list * unit =
  let ((v,amt),dst_addr) = p in
  let t = mk_param (Tezos.create_ticket v amt) in
  let c : whole_p contract = Tezos.get_contract_with_error dst_addr "Testing proxy: you provided a wrong address" in
  let op = Tezos.transaction t 1mutez c in
  [op], ()

type 'v proxy_address = (('v * nat) * address , unit) typed_address
let init (type vt whole_p) : (vt ticket -> whole_p) -> ((vt * nat) * address , unit) typed_address =
  fun (mk_param: vt ticket -> whole_p) ->
    let proxy_ : ((vt * nat) * address) * unit -> operation list * unit = proxy_transfer mk_param in
    let (taddr_proxy, _, _) = Test.originate proxy_ () 1tez in
    (* the 1tez will allow for ~1M transfers... should be enough ? *)
    taddr_proxy

let transfer (type vt) : vt proxy_address -> (vt * nat) * address -> unit =
  fun (taddr_proxy : vt proxy_address) ((v,amt),addr : (vt * nat) * address) ->
    let _ = Test.transfer_to_contract_exn (Test.to_contract taddr_proxy) ((v,amt) , addr) 1mutez in
    ()
