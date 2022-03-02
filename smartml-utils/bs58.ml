(* Copyright 2019-2021 Smart Chain Arena LLC. *)

(* This code was adapted from https://github.com/tqtezos/TZComet (Copyright (c) 2020 Seb Mondet) *)

module B58_crypto = struct
  let sha256 s = Digestif.SHA256.(to_raw_string (digest_string s))

  let sha512 s = Digestif.SHA512.(to_raw_string (digest_string s))
end

let b58_crypto = (module B58_crypto : Base58.CRYPTO)

type types =
  | Key_Hash
  | Address
  | Key
  | Signature
  | Secret_Key
  | Script_Expr_Hash

(* https://gitlab.com/tezos/tezos/-/blob/master/src/lib_crypto/base58.ml *)
module Prefix = struct
  (* 32 *)
  let block_hash = "\001\052" (* B(51) *)

  let operation_hash = "\005\116" (* o(51) *)

  let operation_list_hash = "\133\233" (* Lo(52) *)

  let operation_list_list_hash = "\029\159\109" (* LLo(53) *)

  let protocol_hash = "\002\170" (* P(51) *)

  let context_hash = "\079\199" (* Co(52) *)

  let block_metadata_hash = "\234\249" (* bm(52) *)

  let operation_metadata_hash = "\005\183" (* r(51) *)

  let operation_metadata_list_hash = "\134\039" (* Lr(52) *)

  let operation_metadata_list_list_hash = "\029\159\182" (* LLr(53) *)

  (* 20 *)
  let ed25519_public_key_hash = "\006\161\159" (* tz1(36) *)

  let secp256k1_public_key_hash = "\006\161\161" (* tz2(36) *)

  let p256_public_key_hash = "\006\161\164" (* tz3(36) *)

  (* 16 *)
  let cryptobox_public_key_hash = "\153\103" (* id(30) *)

  (* 32 *)
  let ed25519_seed = "\013\015\058\007" (* edsk(54) *)

  let ed25519_public_key = "\013\015\037\217" (* edpk(54) *)

  let secp256k1_secret_key = "\017\162\224\201" (* spsk(54) *)

  let p256_secret_key = "\016\081\238\189" (* p2sk(54) *)

  (* 56 *)
  let ed25519_encrypted_seed = "\007\090\060\179\041" (* edesk(88) *)

  let secp256k1_encrypted_secret_key = "\009\237\241\174\150" (* spesk(88) *)

  let p256_encrypted_secret_key = "\009\048\057\115\171" (* p2esk(88) *)

  (* 33 *)
  let secp256k1_public_key = "\003\254\226\086" (* sppk(55) *)

  let p256_public_key = "\003\178\139\127" (* p2pk(55) *)

  let secp256k1_scalar = "\038\248\136" (* SSp(53) *)

  let secp256k1_element = "\005\092\000" (* GSp(54) *)

  (* 64 *)
  let ed25519_secret_key = "\043\246\078\007" (* edsk(98) *)

  let ed25519_signature = "\009\245\205\134\018" (* edsig(99) *)

  let secp256k1_signature = "\013\115\101\019\063" (* spsig1(99) *)

  let p256_signature = "\054\240\044\052" (* p2sig(98) *)

  let generic_signature = "\004\130\043" (* sig(96) *)

  (* 4 *)
  let chain_id = "\087\082\000" (* Net(15) *)

  let script_expr_hash =
    (* Taken from src/proto_006_PsCARTHA/lib_protocol/script_expr_hash.ml *)
    (* expr(54) *)
    "\013\044\064\027"

  let contract_hash =
    (* src/proto_006_PsCARTHA/lib_protocol/contract_hash.ml KT1(36) *)
    "\002\090\121"

  (*
    Added in protocol 8
  *)

  (* 169 *)
  let sapling_spending_key = "\011\237\020\092" (* sask(241) *)

  (* 43 *)
  let sapling_address = "\018\071\040\223"

  (* zet1(69) *)

  (*
     Added in protocol 9
  *)
end

let static_addresses =
  [| "KT1TezoooozzSmartPyzzSTATiCzzzwwBFA1"
   ; "KT1Tezooo1zzSmartPyzzSTATiCzzzyfC8eF"
   ; "KT1Tezooo2zzSmartPyzzSTATiCzzzwqqQ4H"
   ; "KT1Tezooo3zzSmartPyzzSTATiCzzzseJjWC"
   ; "KT1Tezooo4zzSmartPyzzSTATiCzzzyPVdv3"
   ; "KT1Tezooo5zzSmartPyzzSTATiCzzzz48Z4p"
   ; "KT1Tezooo6zzSmartPyzzSTATiCzzztY1196"
   ; "KT1Tezooo7zzSmartPyzzSTATiCzzzvTbG1z"
   ; "KT1Tezooo8zzSmartPyzzSTATiCzzzzp29d1"
   ; "KT1Tezooo9zzSmartPyzzSTATiCzzztdBMLX"
   ; "KT1Tezoo1ozzSmartPyzzSTATiCzzzw8CmuY"
   ; "KT1Tezoo11zzSmartPyzzSTATiCzzzzYMWmQ"
   ; "KT1Tezoo12zzSmartPyzzSTATiCzzzw2ZWPM"
   ; "KT1Tezoo13zzSmartPyzzSTATiCzzzyrBos2"
   ; "KT1Tezoo14zzSmartPyzzSTATiCzzzrnXGeA"
   ; "KT1Tezoo15zzSmartPyzzSTATiCzzzzjjBRr"
   ; "KT1Tezoo16zzSmartPyzzSTATiCzzzyHEQ7h"
   ; "KT1Tezoo17zzSmartPyzzSTATiCzzzrQkWKN"
   ; "KT1Tezoo18zzSmartPyzzSTATiCzzzt4Mx4x"
   ; "KT1Tezoo19zzSmartPyzzSTATiCzzzzvx27N"
   ; "KT1Tezoo2ozzSmartPyzzSTATiCzzzt3UDWP"
   ; "KT1Tezoo21zzSmartPyzzSTATiCzzzzPeb4T"
   ; "KT1Tezoo22zzSmartPyzzSTATiCzzzzKyojP"
   ; "KT1Tezoo23zzSmartPyzzSTATiCzzzzLNLUR"
   ; "KT1Tezoo24zzSmartPyzzSTATiCzzzuimc59"
   ; "KT1Tezoo25zzSmartPyzzSTATiCzzzrmbVsM"
   ; "KT1Tezoo26zzSmartPyzzSTATiCzzzwDQDCm"
   ; "KT1Tezoo27zzSmartPyzzSTATiCzzztVDNZ1"
   ; "KT1Tezoo28zzSmartPyzzSTATiCzzzurgreu"
   ; "KT1Tezoo29zzSmartPyzzSTATiCzzzoiUVra"
   ; "KT1Tezoo3ozzSmartPyzzSTATiCzzzzpy1LW"
   ; "KT1Tezoo31zzSmartPyzzSTATiCzzzxZF7L4"
   ; "KT1Tezoo32zzSmartPyzzSTATiCzzzzJfoLr"
   ; "KT1Tezoo33zzSmartPyzzSTATiCzzzt9n3Ca"
   ; "KT1Tezoo34zzSmartPyzzSTATiCzzzxZRxk5"
   ; "KT1Tezoo35zzSmartPyzzSTATiCzzzxEBbbo"
   ; "KT1Tezoo36zzSmartPyzzSTATiCzzzzHx9Mg"
   ; "KT1Tezoo37zzSmartPyzzSTATiCzzzyWyuj1"
   ; "KT1Tezoo38zzSmartPyzzSTATiCzzzwrpEt4"
   ; "KT1Tezoo39zzSmartPyzzSTATiCzzzsxXjKw"
   ; "KT1Tezoo4ozzSmartPyzzSTATiCzzzwJqoPS"
   ; "KT1Tezoo41zzSmartPyzzSTATiCzzzsmVb4M"
   ; "KT1Tezoo42zzSmartPyzzSTATiCzzzwtnVrK"
   ; "KT1Tezoo43zzSmartPyzzSTATiCzzzqz2ouC"
   ; "KT1Tezoo44zzSmartPyzzSTATiCzzztSh3PS"
   ; "KT1Tezoo45zzSmartPyzzSTATiCzzzzteuw9"
   ; "KT1Tezoo46zzSmartPyzzSTATiCzzzyu6S2X"
   ; "KT1Tezoo47zzSmartPyzzSTATiCzzzxjfKR8"
   ; "KT1Tezoo48zzSmartPyzzSTATiCzzzxeybmd"
   ; "KT1Tezoo49zzSmartPyzzSTATiCzzzsYs8mm"
   ; "KT1Tezoo5ozzSmartPyzzSTATiCzzzwq7cM4"
   ; "KT1Tezoo51zzSmartPyzzSTATiCzzzsie8hD"
   ; "KT1Tezoo52zzSmartPyzzSTATiCzzzy6jfJn"
   ; "KT1Tezoo53zzSmartPyzzSTATiCzzzpzJxjJ"
   ; "KT1Tezoo54zzSmartPyzzSTATiCzzzsC58yB"
   ; "KT1Tezoo55zzSmartPyzzSTATiCzzzzB9g1d"
   ; "KT1Tezoo56zzSmartPyzzSTATiCzzzvJRMF4"
   ; "KT1Tezoo57zzSmartPyzzSTATiCzzzyRNXjo"
   ; "KT1Tezoo58zzSmartPyzzSTATiCzzzuoQJPC"
   ; "KT1Tezoo59zzSmartPyzzSTATiCzzzxrzq7o"
   ; "KT1Tezoo6ozzSmartPyzzSTATiCzzzxn4BAQ"
   ; "KT1Tezoo61zzSmartPyzzSTATiCzzzsCVUrJ"
   ; "KT1Tezoo62zzSmartPyzzSTATiCzzzzNq6gW"
   ; "KT1Tezoo63zzSmartPyzzSTATiCzzzvo99GZ"
   ; "KT1Tezoo64zzSmartPyzzSTATiCzzzxyJU8a"
   ; "KT1Tezoo65zzSmartPyzzSTATiCzzzzadcFa"
   ; "KT1Tezoo66zzSmartPyzzSTATiCzzzxPC5nQ"
   ; "KT1Tezoo67zzSmartPyzzSTATiCzzzzyq8B1"
   ; "KT1Tezoo68zzSmartPyzzSTATiCzzzxpBPfQ"
   ; "KT1Tezoo69zzSmartPyzzSTATiCzzzsPcuJf"
   ; "KT1Tezoo7ozzSmartPyzzSTATiCzzzxgiesf"
   ; "KT1Tezoo71zzSmartPyzzSTATiCzzzyGZS4Q"
   ; "KT1Tezoo72zzSmartPyzzSTATiCzzzzc5bP5"
   ; "KT1Tezoo73zzSmartPyzzSTATiCzzzzjqD8v"
   ; "KT1Tezoo74zzSmartPyzzSTATiCzzzqjbsH6"
   ; "KT1Tezoo75zzSmartPyzzSTATiCzzzweg7Kr"
   ; "KT1Tezoo76zzSmartPyzzSTATiCzzzzJQexk"
   ; "KT1Tezoo77zzSmartPyzzSTATiCzzzxzCsXF"
   ; "KT1Tezoo78zzSmartPyzzSTATiCzzzx3Yt9E"
   ; "KT1Tezoo79zzSmartPyzzSTATiCzzzy1eJ9s"
   ; "KT1Tezoo8ozzSmartPyzzSTATiCzzzuGszvB"
   ; "KT1Tezoo81zzSmartPyzzSTATiCzzzyn1ZHZ"
   ; "KT1Tezoo82zzSmartPyzzSTATiCzzzyr5ES9"
   ; "KT1Tezoo83zzSmartPyzzSTATiCzzzxUVHDg"
   ; "KT1Tezoo84zzSmartPyzzSTATiCzzzw2LQfK"
   ; "KT1Tezoo85zzSmartPyzzSTATiCzzzrZRBHF"
   ; "KT1Tezoo86zzSmartPyzzSTATiCzzzwwr66A"
   ; "KT1Tezoo87zzSmartPyzzSTATiCzzzx2asuv"
   ; "KT1Tezoo88zzSmartPyzzSTATiCzzzvVdzo9"
   ; "KT1Tezoo89zzSmartPyzzSTATiCzzzxMckJy"
   ; "KT1Tezoo9ozzSmartPyzzSTATiCzzzxX37Py"
   ; "KT1Tezoo91zzSmartPyzzSTATiCzzzyegtLQ"
   ; "KT1Tezoo92zzSmartPyzzSTATiCzzzvtdVek"
   ; "KT1Tezoo93zzSmartPyzzSTATiCzzzznaYBh"
   ; "KT1Tezoo94zzSmartPyzzSTATiCzzzwqPLoM"
   ; "KT1Tezoo95zzSmartPyzzSTATiCzzzrn7SPm"
   ; "KT1Tezoo96zzSmartPyzzSTATiCzzzw2aWmL"
   ; "KT1Tezoo97zzSmartPyzzSTATiCzzzyYc6Fd"
   ; "KT1Tezoo98zzSmartPyzzSTATiCzzzvpPECg"
   ; "KT1Tezoo99zzSmartPyzzSTATiCzzzzYoJJi"
   ; "KT1Tezo1oozzSmartPyzzSTATiCzzzw6i9jd" |]

let dynamic_addresses =
  [| "KT1TezoooozzSmartPyzzDYNAMiCzzpLu4LU"
   ; "KT1Tezooo1zzSmartPyzzDYNAMiCzztcr8AZ"
   ; "KT1Tezooo2zzSmartPyzzDYNAMiCzzxyHfG9"
   ; "KT1Tezooo3zzSmartPyzzDYNAMiCzzvqsJQk"
   ; "KT1Tezooo4zzSmartPyzzDYNAMiCzzywTMhC"
   ; "KT1Tezooo5zzSmartPyzzDYNAMiCzzvwBH3X"
   ; "KT1Tezooo6zzSmartPyzzDYNAMiCzzvyu5w3"
   ; "KT1Tezooo7zzSmartPyzzDYNAMiCzztDqbVQ"
   ; "KT1Tezooo8zzSmartPyzzDYNAMiCzzq2URWu"
   ; "KT1Tezooo9zzSmartPyzzDYNAMiCzzwMosaF"
   ; "KT1Tezoo1ozzSmartPyzzDYNAMiCzzzknqsi"
   ; "KT1Tezoo11zzSmartPyzzDYNAMiCzzufK1yA"
   ; "KT1Tezoo12zzSmartPyzzDYNAMiCzzp8MwtN"
   ; "KT1Tezoo13zzSmartPyzzDYNAMiCzzuFSRii"
   ; "KT1Tezoo14zzSmartPyzzDYNAMiCzzz7RGrK"
   ; "KT1Tezoo15zzSmartPyzzDYNAMiCzzvGSR2o"
   ; "KT1Tezoo16zzSmartPyzzDYNAMiCzzwFpt7J"
   ; "KT1Tezoo17zzSmartPyzzDYNAMiCzzykrvy7"
   ; "KT1Tezoo18zzSmartPyzzDYNAMiCzztCAt8v"
   ; "KT1Tezoo19zzSmartPyzzDYNAMiCzzs7uP4i"
   ; "KT1Tezoo2ozzSmartPyzzDYNAMiCzzwSnrN6"
   ; "KT1Tezoo21zzSmartPyzzDYNAMiCzzs5DfTE"
   ; "KT1Tezoo22zzSmartPyzzDYNAMiCzzrduLfP"
   ; "KT1Tezoo23zzSmartPyzzDYNAMiCzzwZfMMD"
   ; "KT1Tezoo24zzSmartPyzzDYNAMiCzzwjiGTo"
   ; "KT1Tezoo25zzSmartPyzzDYNAMiCzzyT75uA"
   ; "KT1Tezoo26zzSmartPyzzDYNAMiCzzwgevwP"
   ; "KT1Tezoo27zzSmartPyzzDYNAMiCzzuNESHj"
   ; "KT1Tezoo28zzSmartPyzzDYNAMiCzzstgZLG"
   ; "KT1Tezoo29zzSmartPyzzDYNAMiCzzua3qt8"
   ; "KT1Tezoo3ozzSmartPyzzDYNAMiCzzzydbTn"
   ; "KT1Tezoo31zzSmartPyzzDYNAMiCzzzV6Fv1"
   ; "KT1Tezoo32zzSmartPyzzDYNAMiCzztwKPMA"
   ; "KT1Tezoo33zzSmartPyzzDYNAMiCzztAevd2"
   ; "KT1Tezoo34zzSmartPyzzDYNAMiCzzyHyztq"
   ; "KT1Tezoo35zzSmartPyzzDYNAMiCzzzseQRS"
   ; "KT1Tezoo36zzSmartPyzzDYNAMiCzzxG7VFz"
   ; "KT1Tezoo37zzSmartPyzzDYNAMiCzzvanMaF"
   ; "KT1Tezoo38zzSmartPyzzDYNAMiCzzxow7N5"
   ; "KT1Tezoo39zzSmartPyzzDYNAMiCzzseasUf"
   ; "KT1Tezoo4ozzSmartPyzzDYNAMiCzzwCUzLT"
   ; "KT1Tezoo41zzSmartPyzzDYNAMiCzzuKdM6z"
   ; "KT1Tezoo42zzSmartPyzzDYNAMiCzzvoxbXQ"
   ; "KT1Tezoo43zzSmartPyzzDYNAMiCzzxHNrWB"
   ; "KT1Tezoo44zzSmartPyzzDYNAMiCzzqzWSLJ"
   ; "KT1Tezoo45zzSmartPyzzDYNAMiCzzwoma3L"
   ; "KT1Tezoo46zzSmartPyzzDYNAMiCzzwagsDr"
   ; "KT1Tezoo47zzSmartPyzzDYNAMiCzzx9gczA"
   ; "KT1Tezoo48zzSmartPyzzDYNAMiCzzujzDEL"
   ; "KT1Tezoo49zzSmartPyzzDYNAMiCzzyzNqLS"
   ; "KT1Tezoo5ozzSmartPyzzDYNAMiCzzyhNzVP"
   ; "KT1Tezoo51zzSmartPyzzDYNAMiCzztanVCy"
   ; "KT1Tezoo52zzSmartPyzzDYNAMiCzzukPXQf"
   ; "KT1Tezoo53zzSmartPyzzDYNAMiCzzwertzS"
   ; "KT1Tezoo54zzSmartPyzzDYNAMiCzzxrXndJ"
   ; "KT1Tezoo55zzSmartPyzzDYNAMiCzzwZDrJ6"
   ; "KT1Tezoo56zzSmartPyzzDYNAMiCzzz7MLT7"
   ; "KT1Tezoo57zzSmartPyzzDYNAMiCzzx1SNRa"
   ; "KT1Tezoo58zzSmartPyzzDYNAMiCzzspyLrZ"
   ; "KT1Tezoo59zzSmartPyzzDYNAMiCzzpFMM5S"
   ; "KT1Tezoo6ozzSmartPyzzDYNAMiCzzzc7Vf5"
   ; "KT1Tezoo61zzSmartPyzzDYNAMiCzzxQGTz1"
   ; "KT1Tezoo62zzSmartPyzzDYNAMiCzzzCFRxE"
   ; "KT1Tezoo63zzSmartPyzzDYNAMiCzzxSbaFw"
   ; "KT1Tezoo64zzSmartPyzzDYNAMiCzzrZxQjf"
   ; "KT1Tezoo65zzSmartPyzzDYNAMiCzzuE8zrh"
   ; "KT1Tezoo66zzSmartPyzzDYNAMiCzzxL3RNg"
   ; "KT1Tezoo67zzSmartPyzzDYNAMiCzzxDHbQ2"
   ; "KT1Tezoo68zzSmartPyzzDYNAMiCzzv9uuPs"
   ; "KT1Tezoo69zzSmartPyzzDYNAMiCzzziAEo3"
   ; "KT1Tezoo7ozzSmartPyzzDYNAMiCzzssee8N"
   ; "KT1Tezoo71zzSmartPyzzDYNAMiCzzy8o38T"
   ; "KT1Tezoo72zzSmartPyzzDYNAMiCzzvJQgMd"
   ; "KT1Tezoo73zzSmartPyzzDYNAMiCzzzQKejF"
   ; "KT1Tezoo74zzSmartPyzzDYNAMiCzzxGc1Le"
   ; "KT1Tezoo75zzSmartPyzzDYNAMiCzzwS4VD5"
   ; "KT1Tezoo76zzSmartPyzzDYNAMiCzzsu5trk"
   ; "KT1Tezoo77zzSmartPyzzDYNAMiCzzx7fRph"
   ; "KT1Tezoo78zzSmartPyzzDYNAMiCzzubSbwk"
   ; "KT1Tezoo79zzSmartPyzzDYNAMiCzzwzStmF"
   ; "KT1Tezoo8ozzSmartPyzzDYNAMiCzzq35Rq4"
   ; "KT1Tezoo81zzSmartPyzzDYNAMiCzzzQyrWb"
   ; "KT1Tezoo82zzSmartPyzzDYNAMiCzzuk3Zgh"
   ; "KT1Tezoo83zzSmartPyzzDYNAMiCzzzamuiH"
   ; "KT1Tezoo84zzSmartPyzzDYNAMiCzztuhJts"
   ; "KT1Tezoo85zzSmartPyzzDYNAMiCzzsEUusH"
   ; "KT1Tezoo86zzSmartPyzzDYNAMiCzzywXBn3"
   ; "KT1Tezoo87zzSmartPyzzDYNAMiCzzto1Zr6"
   ; "KT1Tezoo88zzSmartPyzzDYNAMiCzzzYK2k5"
   ; "KT1Tezoo89zzSmartPyzzDYNAMiCzzvEXcr3"
   ; "KT1Tezoo9ozzSmartPyzzDYNAMiCzzzhhryG"
   ; "KT1Tezoo91zzSmartPyzzDYNAMiCzzzsUuqY"
   ; "KT1Tezoo92zzSmartPyzzDYNAMiCzzvE65Pi"
   ; "KT1Tezoo93zzSmartPyzzDYNAMiCzzyo94pw"
   ; "KT1Tezoo94zzSmartPyzzDYNAMiCzzz3TFhN"
   ; "KT1Tezoo95zzSmartPyzzDYNAMiCzzuzjFR1"
   ; "KT1Tezoo96zzSmartPyzzDYNAMiCzzsS2Wj8"
   ; "KT1Tezoo97zzSmartPyzzDYNAMiCzzywXC4c"
   ; "KT1Tezoo98zzSmartPyzzDYNAMiCzzuttMz3"
   ; "KT1Tezoo99zzSmartPyzzDYNAMiCzzr8xqaW"
   ; "KT1Tezo1oozzSmartPyzzDYNAMiCzzxmuJG3" |]

let build_kt1_address target =
  let init = Prefix.contract_hash ^ String.make 20 '\000' in
  let init = Bytes.of_string init in
  let current = ref "" in
  let current_bin = ref "" in
  for i = 0 to 19 do
    let set j = Bytes.set init (i + 3) (Char.chr j) in
    let ok j =
      set j;
      let next = Bytes.to_string init in
      let (`Base58 encoded) = Base58.of_bytes b58_crypto next in
      encoded <= target
    in
    let rec find a b =
      if b - a <= 1
      then begin
        if ok b then set b else set a;
        let next = Bytes.to_string init in
        let (`Base58 encoded) = Base58.of_bytes b58_crypto next in
        current_bin := next;
        current := encoded
      end
      else
        let c = (a + b) / 2 in
        if ok c
        then find c b
        else begin
          set a;
          find a c
        end
    in
    find 0 255
  done;
  (!current, Misc.Hex.hexcape !current_bin)

let build_address_of_contract_id ~static contract_id =
  let target =
    if static
    then Printf.sprintf "KT1Tez%04izzSmartPyzzSTATiCzzzzzzzzzzzz"
    else Printf.sprintf "KT1Tez%04izzSmartPyzzDYNAMiCzzzzzzzzzzz"
  in
  let target = target contract_id in
  let target =
    String.map
      (function
        | '0' -> 'o'
        | c -> c)
      target
  in
  build_kt1_address target

let address_of_contract_id ~static contract_id entrypoint =
  let base =
    if contract_id < 100
    then
      if static
      then static_addresses.(contract_id)
      else dynamic_addresses.(contract_id)
    else fst (build_address_of_contract_id ~static contract_id)
  in
  let entrypoint = Option.cata "" (Printf.sprintf "%%%s") entrypoint in
  base ^ entrypoint

let build_kt1_addresses n =
  let build_fake_kt1_address ~static =
    for i = 0 to n do
      let kt1, _ = build_address_of_contract_id ~static i in
      Printf.ksprintf print_endline "%S;" kt1
    done
  in
  print_endline "let static_addresses = [|";
  build_fake_kt1_address ~static:true;
  print_endline "|]\nlet dynamic_addresses = [|";
  build_fake_kt1_address ~static:false;
  print_endline "|]"

let get_kt1_suffix data =
  match Stdlib.String.index_opt data '%' with
  | None -> (data, "00")
  | Some i ->
      let address = String.sub data 0 i in
      let suffix = String.sub data (i + 1) (String.length data - i - 1) in
      (address, "00" ^ Misc.Hex.hexcape suffix)

let find_prefix data ~data_type ~prefix_length =
  match (data_type, String.sub data 0 prefix_length) with
  | Key_Hash, "tz1" -> ("00", data, Prefix.ed25519_public_key_hash, "")
  | Key_Hash, "tz2" -> ("01", data, Prefix.secp256k1_public_key_hash, "")
  | Key_Hash, "tz3" -> ("02", data, Prefix.p256_public_key_hash, "")
  | Address, "tz1" -> ("0000", data, Prefix.ed25519_public_key_hash, "")
  | Address, "tz2" -> ("0001", data, Prefix.secp256k1_public_key_hash, "")
  | Address, "tz3" -> ("0002", data, Prefix.p256_public_key_hash, "")
  | Address, "KT1" ->
      let address, suffix = get_kt1_suffix data in
      ("01", address, Prefix.contract_hash, suffix)
  | Key, "edpk" -> ("00", data, Prefix.ed25519_public_key, "")
  | Key, "sppk" -> ("01", data, Prefix.secp256k1_public_key, "")
  | Key, "p2pk" -> ("02", data, Prefix.p256_public_key, "")
  | Signature, "sig" -> ("", data, Prefix.generic_signature, "")
  | Signature, "edsig" -> ("", data, Prefix.ed25519_signature, "")
  | Signature, "spsig1" -> ("", data, Prefix.secp256k1_signature, "")
  | Signature, "p2sig" -> ("", data, Prefix.p256_signature, "")
  | Secret_Key, "ede" -> ("", data, Prefix.ed25519_encrypted_seed, "")
  | Secret_Key, "spe" -> ("", data, Prefix.secp256k1_encrypted_secret_key, "")
  | Secret_Key, "p2e" -> ("", data, Prefix.p256_encrypted_secret_key, "")
  | Secret_Key, "eds" ->
      let prefix =
        if String.length data = 32
        then Prefix.ed25519_seed
        else Prefix.ed25519_secret_key
      in
      ("", data, prefix, "")
  | Secret_Key, "spsk" -> ("", data, Prefix.secp256k1_secret_key, "")
  | Secret_Key, "p2s" -> ("", data, Prefix.p256_secret_key, "")
  | Script_Expr_Hash, "expr" -> ("", data, Prefix.script_expr_hash, "")
  | _ -> Fmt.failwith "[Decoding] Unknown prefix for data: %s" data

let find_prefix_reverse rawData ~data_type ~prefix_length =
  let prefix = String.sub rawData 0 prefix_length in
  let data =
    String.sub rawData prefix_length (String.length rawData - prefix_length)
  in
  match (data_type, prefix) with
  | Key_Hash, "00" -> (data, Prefix.ed25519_public_key_hash, "")
  | Key_Hash, "01" -> (data, Prefix.secp256k1_public_key_hash, "")
  | Key_Hash, "02" -> (data, Prefix.p256_public_key_hash, "")
  | Address, "00" ->
      let prefix = String.sub data 0 2 in
      let data = String.sub data 2 (String.length data - 2) in
      ( match prefix with
      | "00" -> (data, Prefix.ed25519_public_key_hash, "")
      | "01" -> (data, Prefix.secp256k1_public_key_hash, "")
      | "02" -> (data, Prefix.p256_public_key_hash, "")
      | _ -> Fmt.failwith "Unknown address: %s" rawData )
  | Address, "01" ->
      let address = String.sub data 0 40 in
      let suffix =
        if String.length data > 42
        then
          "%" ^ (String.sub data 42 (String.length data - 42) |> Misc.Hex.unhex)
        else ""
      in
      (address, Prefix.contract_hash, suffix)
  | Key, "00" -> (data, Prefix.ed25519_public_key, "")
  | Key, "01" -> (data, Prefix.secp256k1_public_key, "")
  | Key, "02" -> (data, Prefix.p256_public_key, "")
  | Signature, _ -> (data, Prefix.generic_signature, "")
  | Script_Expr_Hash, _ -> (data, Prefix.script_expr_hash, "")
  | _ -> Fmt.failwith "[Encoding] Unknown prefix for data : %s" rawData

let decode data ~data_type ~prefix_length =
  let prefix, data_to_decode, prefix_bytes, suffix =
    find_prefix data ~data_type ~prefix_length
  in
  let bytes = Base58.to_bytes_exn b58_crypto (`Base58 data_to_decode) in
  let hash = Base.String.chop_prefix bytes ~prefix:prefix_bytes in
  let (`Hex result) =
    match hash with
    | Some h -> Hex.of_string h
    | None -> Fmt.failwith "Could not decode data: %s" data
  in
  prefix ^ result ^ suffix

let encode data ~data_type ~prefix_length =
  let data_to_encode, prefix_bytes, suffix =
    find_prefix_reverse data ~data_type ~prefix_length
  in
  let bytes = prefix_bytes ^ Misc.Hex.unhex data_to_encode in
  let (`Base58 encoded) = Base58.of_bytes b58_crypto bytes in
  encoded ^ suffix

let decode_to_hex data =
  Misc.Hex.hexcape (Base58.to_bytes_exn b58_crypto (`Base58 data))

let decode_secret_key data = decode data ~data_type:Secret_Key ~prefix_length:3

let decode_key_hash data = decode data ~data_type:Key_Hash ~prefix_length:3

let decode_address data = decode data ~data_type:Address ~prefix_length:3

let decode_key data = decode data ~data_type:Key ~prefix_length:4

let decode_signature data =
  match String.sub data 0 3 with
  | "sig" -> decode data ~data_type:Signature ~prefix_length:3
  | "sps" -> decode data ~data_type:Signature ~prefix_length:6 (* spsig1 *)
  | _ -> decode data ~data_type:Signature ~prefix_length:5

let encode_by_prefix ~prefix data =
  let bytes = prefix ^ Misc.Hex.unhex data in
  let (`Base58 encoded) = Base58.of_bytes b58_crypto bytes in
  encoded

let encode_hex data =
  let bytes = Misc.Hex.unhex data in
  let (`Base58 encoded) = Base58.of_bytes b58_crypto bytes in
  encoded

let encode_key_hash data = encode data ~data_type:Key_Hash ~prefix_length:2

let encode_address data = encode data ~data_type:Address ~prefix_length:2

let encode_key data = encode data ~data_type:Key ~prefix_length:2

let encode_signature data = encode data ~data_type:Signature ~prefix_length:0

let encode_expr data = encode data ~data_type:Script_Expr_Hash ~prefix_length:0

(*
 *  PACK
 *
 *  - tezos-client --endpoint https://edonet.smartpy.io hash data '"KT1DieU51jzXLerQx5AqMCiLC1SsCeM8yRat"' of type 'address'
 *  - tezos-client --endpoint https://edonet.smartpy.io hash data '"tz1fextP23D6Ph2zeGTP8EwkP5Y8TufeFCHA"' of type 'key_hash'
 *  - tezos-client --endpoint https://edonet.smartpy.io hash data '"edpktppVJVhoLCs27UwX9BFEPN4Q3BTiLpv8y4ipHUQmxPki17w79A"' of type 'key'
 *  - tezos-client --endpoint https://edonet.smartpy.io hash data '"edsigthw6sSCfcZbjKCqGE9CZ9PoTsW1Kh5cgGu5SSU1AzcKyJ37oubeKVnDfY291minBiui7khzr8pzhoFVtF9ULs3hnUVKXGx"' of type 'signature'
 *  - tezos-client --endpoint https://falphanet.smartpy.io hash data '"SG1jfZeHRzeWAM1T4zrwunEyUpwWc82D4tbv"' of type 'baker_hash'
 *
 *  UNPACK
 *
 *  - tezos-client --endpoint https://edonet.smartpy.io unpack michelson data '0x050a000000404d6738931b59605ca0449b7b76a01d210ace9220051821ecf43382a7024ed99063c5bedb85c81b919d33a213c6d8fb47f2c9b4deaf6f68f56dee038ea739740f'
 *  - tezos-client --endpoint https://edonet.smartpy.io normalize data '0x4d6738931b59605ca0449b7b76a01d210ace9220051821ecf43382a7024ed99063c5bedb85c81b919d33a213c6d8fb47f2c9b4deaf6f68f56dee038ea739740f' of type 'signature'
 *)
