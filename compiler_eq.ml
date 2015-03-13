
type 'a result =
  [ `Ok of 'a
  | `Error of bool * string ]

let return x = `Ok x

let err s = `Error(false, s)

let success = `Ok ()

let (>>=) m k =
  match m with
  | `Error s as e -> e
  | `Ok x -> k x

let (/) = Filename.concat

let check_system cmd = Unix.(match system cmd with
  | WSIGNALED _ -> err ("'"^cmd^"' was killed by signal")
  | WSTOPPED _  -> err ("'"^cmd^"' was stopped by signal")
  | WEXITED 0 -> success
  | WEXITED k -> err (Printf.sprintf "'%s' exited with code %d\n%!" cmd k)
)

let get_line cmd =
  let ic = Unix.open_process_in cmd in
  try
    let line = input_line ic in
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> return line
    | _ -> err ("error with '"^cmd^"'")
  with End_of_file -> err ("error with '"^cmd^"'")

let switch1 = "cmp-tree1"

let switch2 = "cmp-tree2"

let switch sw c =
  check_system
    ("opam switch " ^ sw ^ "-y -A " ^ c ^ " --no-switch")

let init c1 c2 =
  switch switch1 c1 >>= fun () ->
  switch switch2 c2 >>= fun () ->
  return ()

let force_bin_annot () =
  Unix.putenv "OCAMLPARAM" "_,bin-annot=1"

let install sw pkgs =
  check_system
    ("opam install -y -b --switch=" ^ sw ^ " " ^ String.concat " " pkgs)

let install pkgs =
  force_bin_annot ();
  install switch1 pkgs >>= fun () ->
  install switch2 pkgs >>= fun () ->
  return ()

let clean_attributes _ _ = []

let clean_structure mp items =
  let open Parsetree in
  let open Ast_mapper in
  let items =
    List.filter
      (function
        | { pstr_desc = Pstr_attribute _ } -> false
        | _ -> true)
      items
  in
    default_mapper.structure mp items

let clean_signature mp items =
  let open Parsetree in
  let open Ast_mapper in
  let items =
    List.filter
      (function
        | { psig_desc = Psig_attribute _ } -> false
        | _ -> true)
      items
  in
    default_mapper.signature mp items

let clean_class_structure mp cstr =
  let open Parsetree in
  let open Ast_mapper in
  let pcstr_fields =
    List.filter
      (function
        | { pcf_desc = Pcf_attribute _ } -> false
        | _ -> true)
      cstr.pcstr_fields
  in
    default_mapper.class_structure
      mp { cstr with pcstr_fields }

let clean_class_signature mp csig =
  let open Parsetree in
  let open Ast_mapper in
  let pcsig_fields =
    List.filter
      (function
        | { pctf_desc = Pctf_attribute _ } -> false
        | _ -> true)
      csig.pcsig_fields
  in
    default_mapper.class_signature
      mp { csig with pcsig_fields }

let clean_class_field mp cf =
  let open Parsetree in
  let open Ast_mapper in
  let pcf_attributes =
    mp.attributes mp cf.pcf_attributes
  in
    default_mapper.class_field
      mp { cf with pcf_attributes }

let clean_class_type_field mp cf =
  let open Parsetree in
  let open Ast_mapper in
  let pctf_attributes =
    mp.attributes mp cf.pctf_attributes
  in
    default_mapper.class_type_field
      mp { cf with pctf_attributes }

let clean_mapper =
  let open Ast_mapper in
    { default_mapper with
        attributes = clean_attributes;
        structure = clean_structure;
        signature = clean_signature;
        class_structure = clean_class_structure;
        class_signature = clean_class_signature;
        class_field = clean_class_field;
        class_type_field = clean_class_type_field; }

let clean_signature sg =
  let open Ast_mapper in
    clean_mapper.signature clean_mapper
      (Untypeast.untype_signature sg)

let clean_structure str =
  let open Ast_mapper in
    clean_mapper.structure clean_mapper
      (Untypeast.untype_structure str)

let eq cmt1 cmt2 =
  let open Cmt_format in
  let tt1 = read_cmt cmt1 in
  let tt2 = read_cmt cmt2 in
    match tt1.cmt_annots, tt2.cmt_annots with
    | Interface sg1, Interface sg2 ->
        (clean_signature sg1) = (clean_signature sg2)
    | Implementation str1, Implementation str2 ->
        (clean_structure str1) = (clean_structure str2)
    | Packed _, Packed _  -> true
    | _ -> false

let build_dir sw pkg =
  get_line "opam config var root" >>= fun root ->
  get_line ("opam show -f version " ^ pkg) >>= fun version ->
  let build = root / sw / "build" in
  let pkgv = pkg ^ "." ^ version in
    return (build / pkgv)

let read_files base path =
  let rec loop acc dh =
    match Unix.readdir dh with
    | exception End_of_file -> acc
    | file -> loop (file::acc) dh
  in
  try
    let dh = Unix.opendir (base / path) in
    let files = loop [] dh in
    Unix.closedir dh;
    files
  with Unix.Unix_error (Unix.ENOTDIR, _, _) -> []

let read_files_rec base path =
  let rec loop acc path =
    List.fold_left
      (fun acc -> function
         | "." | ".." -> acc
         | file ->
           let path = path / file in
             loop (path :: acc) path)
      acc
      (read_files base path)
  in
    loop [] path

let is_cmt file =
  Filename.check_suffix file ".cmt"
  || Filename.check_suffix file ".cmti"

let collect pkg =
  build_dir switch1 pkg >>= fun build1 ->
  let files1 = read_files_rec build1 "." in
  let cmts1 = List.filter is_cmt files1 in
  let cmts1 = List.sort compare cmts1 in
  build_dir switch2 pkg >>= fun build2 ->
  let files2 = read_files_rec build2 "." in
  let cmts2 = List.filter is_cmt files2 in
  let cmts2 = List.sort compare cmts2 in
    if cmts1 <> cmts2 then err ("Cmt sets not equal: " ^ pkg)
    else
      return (List.map
            (fun cmt -> (cmt, build1 / cmt, build2 / cmt))
            cmts1)

let compare_pkg pkg =
  collect pkg >>= fun cmts ->
    return
      (List.fold_left
         (fun acc (name, cmt1, cmt2) ->
            (name, eq cmt1 cmt2)  :: acc)
         [] cmts)

let compare_pkgs pkgs =
  List.fold_left
    (fun acc pkg ->
       compare_pkg pkg >>= fun names ->
       acc >>= fun acc ->
       return (names @ acc))
    (return []) pkgs

let print_results names =
  List.iter
    (fun (name, result) ->
       if result then Printf.printf "'%s': equal\n" name
       else Printf.printf "'%s': not equal\n" name)
    names

let check pkgs =
  compare_pkgs pkgs >>= fun results ->
  print_results results;
  return ()

let write_signature sg =
  let tmp, ch = Filename.open_temp_file "compiler_eq" ".sg" in
  let fmt = Format.formatter_of_out_channel ch in
    Printast.interface fmt sg;
    close_out ch;
    tmp

let diff_signature sg1 sg2 =
  let tmp1 = write_signature sg1 in
  let tmp2 = write_signature sg2 in
    check_system
      ("diff -u " ^ tmp1 ^ " " ^ tmp2)

let write_structure str =
  let tmp, ch = Filename.open_temp_file "compiler_eq" ".str" in
  let fmt = Format.formatter_of_out_channel ch in
    Printast.implementation fmt str;
    close_out ch;
    tmp

let diff_structure str1 str2 =
  let tmp1 = write_structure str1 in
  let tmp2 = write_structure str2 in
    check_system
      ("diff -u " ^ tmp1 ^ " " ^ tmp2)

let diff pkg file =
  build_dir switch1 pkg >>= fun build1 ->
  build_dir switch2 pkg >>= fun build2 ->
  let cmt1 = build1 / file in
  let cmt2 = build2 / file in
  let open Cmt_format in
  let tt1 = read_cmt cmt1 in
  let tt2 = read_cmt cmt2 in
    match tt1.cmt_annots, tt2.cmt_annots with
    | Interface sg1, Interface sg2 ->
        diff_signature (clean_signature sg1) (clean_signature sg2)
    | Implementation str1, Implementation str2 ->
        diff_structure (clean_structure str1) (clean_structure str2)
    | Packed _, Packed _  -> err "Packed"
    | _ -> err "Unmatched"

open Cmdliner

let compiler1 =
  let docv = "COMPILER1" in
  let doc = "first compiler" in
  Arg.(required (pos 0 (some string) None & info ~docv ~doc []))

let compiler2 =
  let docv = "COMPILER2" in
  let doc = "second compiler" in
  Arg.(required (pos 1 (some string) None & info ~docv ~doc []))

let packages =
  let docv = "PACKAGES" in
  let doc = "packages" in
  Arg.(non_empty (pos_all string [] & info ~docv ~doc []))

let package =
  let docv = "PACKAGE" in
  let doc = "package" in
  Arg.(required (pos 0 (some string) None & info ~docv ~doc []))

let file =
  let docv = "FILE" in
  let doc = "file" in
  Arg.(required (pos 1 (some string) None & info ~docv ~doc []))

let init_term =
  Term.(ret (pure init $ compiler1 $ compiler2))

let init_info =
  let doc = "initialise the compilers for comparision" in
    Term.info "init" ~doc

let init_cmd = (init_term, init_info)

let install_term =
  Term.(ret (pure install $ packages))

let install_info =
  let doc = "install packages for comparison" in
    Term.info "install" ~doc

let install_cmd = (install_term, install_info)

let check_term =
  Term.(ret (pure check $ packages))

let check_info =
  let doc = "compare compilers" in
    Term.info "check" ~doc

let check_cmd = (check_term, check_info)

let diff_term =
  Term.(ret (pure diff $ package $ file))

let diff_info =
  let doc = "diff package file" in
    Term.info "diff" ~doc

let diff_cmd = (diff_term, diff_info)

let info =
  let doc = "compare compilers on opam packages" in
    Term.info "compiler_eq" ~doc

let () =
  let ret =
    Term.eval_choice (check_term, info)
      [init_cmd; install_cmd; check_cmd; diff_cmd]
  in
    match ret with
    | `Ok () | `Version | `Help -> exit 0
    | `Error _ -> exit 1
