#import "../src/one_step_proof.mligo" "One_step_proof"
#import "../../stdlib_ext/src/unit_test.mligo" "Unit"

type originated = Unit.originated
type originated_one_step_proof = (One_step_proof.parameter, One_step_proof.storage) originated

let one_step_proof_default_storage (owner : address) =
    { owner = owner
    ; is_expected_state = false
    }

let originate_one_step_proof (storage : One_step_proof.storage) =
    Unit.originate One_step_proof.main (one_step_proof_default_storage storage.owner) 0tez

let chusai_entity_contract (b_input, _b_storage : One_step_proof.chusai_arg  * One_step_proof.chusai_storage) =
  let o_input = (Bytes.unpack b_input : nat option) in
  let o_output = match o_input with
  | None -> None
  | Some input -> (
    let res = input + 1 in
    let b_out = Bytes.pack res in
    Some b_out)
  in
  (([] : operation list), o_output)

let get_is_expected_state (contract: originated_one_step_proof) =
  let storage = Test.get_storage contract.originated_typed_address in
  storage.is_expected_state

let _test_proof () =
   let _operator,users = Unit.init_default () in
   let referee, alice, _ = users in
   let storage = one_step_proof_default_storage referee.address in
   let contract = originate_one_step_proof storage in

   let chusai_storage = (None : bytes option) in
   let chusai_arg = Bytes.pack 10 in
   let chusai_state = Bytes.pack (alice.address, chusai_storage, 0n) in
   let chusai_contract = Bytes.pack chusai_entity_contract in

   let chusai_expected_storage = Bytes.pack 11 in
   let chusai_expected_state = Bytes.pack (alice.address, chusai_expected_storage, 0n) in

   let _state =
     Unit.act_as referee
     (fun () -> Unit.transfer_to_contract_
                  contract.originated_contract
                  (One_step_proof (chusai_contract, chusai_arg, chusai_state, chusai_expected_state))
                  0tez
       )
   in
   Unit.and_list
   [ Unit.assert_cond contract get_is_expected_state "should be true"
   ]

let _test_binary_from_layer2 () =
   let _operator,users = Unit.init_default () in
   let referee, alice, _ = users in
   let storage = one_step_proof_default_storage referee.address in
   let contract = originate_one_step_proof storage in

   let chusai_storage = Some (Bytes.pack 1n) in
   let chusai_arg = Bytes.pack 2n in
   let chusai_state = Bytes.pack (alice.address, chusai_storage, 0n) in

   (* see pack_add.mligo to generate expected result *)
   let chusai_contract = 0x05020000004e037a050d0362072f02000000060320053e03690200000030034c072f02000000060320053e0369020000001c050d0362072f02000000060320053e036902000000060312030c0346053d036d0342 in

   let chusai_expected_storage = Bytes.pack 3n in
   let chusai_expected_state = Bytes.pack (alice.address, chusai_expected_storage, 0n) in

   let _state =
     Unit.act_as referee
     (fun () -> Unit.transfer_to_contract_
                  contract.originated_contract
                  (One_step_proof (chusai_contract, chusai_arg, chusai_state, chusai_expected_state))
                  0tez
       )
   in
   Unit.and_list
   [ Unit.assert_cond contract get_is_expected_state "should be true"
   ]

let suite =
  Unit.make_suite
  "One step proof sc"
  "test of the smart contract"
  [ Unit.make_test
    "Test a state transition"
    "Test a simple state transition and check the new state"
    _test_proof
  ; Unit.make_test
    "Test a state transition"
    "Test a from l2"
    _test_binary_from_layer2
  ]

let test = 
  Unit.run_suites 
  (  [ suite ]
  )
