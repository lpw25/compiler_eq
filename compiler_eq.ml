
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

let check_system_relaxed cmd = Unix.(match system cmd with
  | WSIGNALED _ -> err ("'"^cmd^"' was killed by signal")
  | WSTOPPED _  -> err ("'"^cmd^"' was stopped by signal")
  | WEXITED _ -> success
)

let get_line cmd =
  let ic = Unix.open_process_in cmd in
  try
    let line = input_line ic in
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> return line
    | _ -> err ("error with '"^cmd^"'")
  with End_of_file -> err ("error with '"^cmd^"'")

let switch1 = "compiler_eq1"

let switch2 = "compiler_eq2"

let switch sw c =
  check_system
    ("opam switch " ^ sw ^ " -y -A " ^ c ^ " --no-switch")

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
  if pkgs = [] then return ()
  else begin
    force_bin_annot ();
    install switch1 pkgs >>= fun () ->
    install switch2 pkgs >>= fun () ->
    return ()
  end

let get_packages sw =
  let opam_list =
    Unix.open_process_in ("opam list --switch=" ^ sw ^ " -S -s")
  in
  let rec next_line acc =
    let rec next_pkg acc line =
      match String.index line ' ' with
      | exception Not_found ->
          if line = "" then next_line acc
          else next_line (line :: acc)
      | k ->
        let pkg = String.sub line 0 k in
        let rest = String.sub line (k+1) (String.length line - k - 1) in
          next_pkg (pkg :: acc) rest
    in
      match input_line opam_list with
      | exception End_of_file -> begin
          match Unix.close_process_in opam_list with
          | Unix.WEXITED 0 -> return acc
          | _ -> err ("error with 'opam list --switch=" ^ sw ^ " -S -s'")
        end
      | line -> next_pkg acc line
  in
    next_line []

let dummy_position =
  let open Lexing in
  { pos_fname = "";
    pos_lnum = 0;
    pos_bol = 0;
    pos_cnum = 0; }

let dummy_location =
  let open Location in
  { loc_start = dummy_position;
    loc_end = dummy_position;
    loc_ghost = true; }

let clean_location cattr cloc mp loc =
  let open Ast_mapper in
  let loc =
    if cloc then dummy_location
    else loc
  in
    default_mapper.location mp loc

let clean_attributes cattr cloc mp attr =
  let open Ast_mapper in
    if cattr then []
    else default_mapper.attributes mp attr

let clean_structure cattr cloc mp items =
  let open Parsetree in
  let open Ast_mapper in
  let items =
    if cattr then
      List.filter
        (function
          | { pstr_desc = Pstr_attribute _ } -> false
          | _ -> true)
        items
    else items
  in
    default_mapper.structure mp items

let clean_signature cattr cloc mp items =
  let open Parsetree in
  let open Ast_mapper in
  let items =
    if cattr then
      List.filter
        (function
          | { psig_desc = Psig_attribute _ } -> false
          | _ -> true)
        items
    else items
  in
    default_mapper.signature mp items

let clean_class_structure cattr cloc mp cstr =
  let open Parsetree in
  let open Ast_mapper in
  let pcstr_fields =
    if cattr then
      List.filter
        (function
          | { pcf_desc = Pcf_attribute _ } -> false
          | _ -> true)
        cstr.pcstr_fields
    else cstr.pcstr_fields
  in
    default_mapper.class_structure
      mp { cstr with pcstr_fields }

let clean_class_signature cattr cloc mp csig =
  let open Parsetree in
  let open Ast_mapper in
  let pcsig_fields =
    if cattr then
      List.filter
        (function
          | { pctf_desc = Pctf_attribute _ } -> false
          | _ -> true)
        csig.pcsig_fields
    else csig.pcsig_fields
  in
    default_mapper.class_signature
      mp { csig with pcsig_fields }

let clean_class_field cattr cloc mp cf =
  let open Parsetree in
  let open Ast_mapper in
  let pcf_attributes =
    mp.attributes mp cf.pcf_attributes
  in
    default_mapper.class_field
      mp { cf with pcf_attributes }

let clean_class_type_field cattr cloc mp cf =
  let open Parsetree in
  let open Ast_mapper in
  let pctf_attributes =
    mp.attributes mp cf.pctf_attributes
  in
    default_mapper.class_type_field
      mp { cf with pctf_attributes }

let clean_mapper cattr cloc =
  let open Ast_mapper in
    { default_mapper with
        location = clean_location cattr cloc;
        attributes = clean_attributes cattr cloc;
        structure = clean_structure cattr cloc;
        signature = clean_signature cattr cloc;
        class_structure = clean_class_structure cattr cloc;
        class_signature = clean_class_signature cattr cloc;
        class_field = clean_class_field cattr cloc;
        class_type_field = clean_class_type_field cattr cloc; }

let clean_signature cattr cloc sg =
  let open Ast_mapper in
  let mapper = clean_mapper cattr cloc in
    mapper.signature mapper
      (Untypeast.untype_signature sg)

let clean_structure cattr cloc str =
  let open Ast_mapper in
  let mapper = clean_mapper cattr cloc in
    mapper.structure mapper
      (Untypeast.untype_structure str)

let eq cattr cloc cmt1 cmt2 =
  let open Cmt_format in
  let tt1 = read_cmt cmt1 in
  let tt2 = read_cmt cmt2 in
    match tt1.cmt_annots, tt2.cmt_annots with
    | Interface sg1, Interface sg2 ->
        (clean_signature cattr cloc sg1)
        = (clean_signature cattr cloc sg2)
    | Implementation str1, Implementation str2 ->
        (clean_structure cattr cloc str1)
        = (clean_structure cattr cloc str2)
    | Packed _, Packed _  -> true
    | _ -> false

let build_dir sw pkg =
  get_line ("opam config var root") >>= fun root ->
  get_line ("opam show --switch=" ^ sw ^ " -f version " ^ pkg) >>= fun ver ->
  let build = root / sw / "build" in
  let pkgv = pkg ^ "." ^ ver in
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
  with
  | Unix.Unix_error (Unix.ENOTDIR, _, _) -> []
  | Unix.Unix_error (Unix.ENOENT, _, _) -> []

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

let compare_pkg cattr cloc pkg =
  collect pkg >>= fun cmts ->
    return
      (List.fold_left
         (fun acc (name, cmt1, cmt2) ->
            (name, eq cattr cloc cmt1 cmt2)  :: acc)
         [] cmts)

let print_result utf8 color pkg results =
  let blue s =
    if color then Printf.sprintf "\027[1;34m%s\027[m" s
    else s
  in
  let red s =
    if color then Printf.sprintf "\027[31m%s\027[m" s
    else s
  in
  let green s =
    if color then Printf.sprintf "\027[32m%s\027[m" s
    else s
  in
  let space = " " in
  let newline = "\n" in
  let top_left = if utf8 then "\226\148\140" else "+" in
  let top_centre = if utf8 then "\226\148\172" else "+" in
  let top_right = if utf8 then "\226\148\144" else "+" in
  let middle_left = if utf8 then "\226\148\156" else "+" in
  let middle_right = if utf8 then "\226\148\164" else "+" in
  let bottom_left = if utf8 then "\226\148\148" else "+" in
  let bottom_centre = if utf8 then "\226\148\180" else "+" in
  let bottom_right = if utf8 then "\226\148\152" else "+" in
  let horizontal = if utf8 then "\226\148\128" else "-" in
  let vertical = if utf8 then "\226\148\130" else "|" in
  let equal = if utf8 then "\226\156\147" else "Equal    "  in
  let not_equal = if utf8 then "\226\156\151" else "Not equal" in
  let equal_length = if utf8 then 1 else 9 in
  if results = [] then ()
  else begin
    let colored_pkg =
      if List.for_all (fun (_, result) -> result) results then green pkg
      else red pkg
    in
    let name_length =
      List.fold_left
        (fun acc (name, _) -> max acc (String.length name))
        0 results
    in
    let name_length = max name_length (String.length pkg) in
    let name_width = name_length - 1 in
    let equal_width = equal_length - 1 in
    print_string (blue top_left);
    print_string (blue horizontal);
    for i = 0 to name_width do
      print_string (blue horizontal) done;
    print_string (blue horizontal);
    print_string (blue horizontal);
    print_string (blue horizontal);
    for i = 0 to equal_width do
      print_string (blue horizontal) done;
    print_string (blue horizontal);
    print_string (blue top_right);
    print_string newline;
    print_string (blue vertical);
    print_string space;
    print_string colored_pkg;
    for i = String.length pkg to name_width do
      print_string space done;
    print_string space;
    print_string space;
    print_string space;
    for i = 0 to equal_width do
      print_string space done;
    print_string space;
    print_string (blue vertical);
    print_string newline;
    print_string (blue middle_left);
    print_string (blue horizontal);
    for i = 0 to name_width do
      print_string (blue horizontal) done;
    print_string (blue horizontal);
    print_string (blue top_centre);
    print_string (blue horizontal);
    for i = 0 to equal_width do
      print_string (blue horizontal) done;
    print_string (blue horizontal);
    print_string (blue middle_right);
    print_string newline;
    List.iter
      (fun (name, result) ->
         let colored_name =
           if result then green name else red name
         in
         print_string (blue vertical);
         print_string space;
         print_string colored_name;
         for i = String.length name to name_width do
           print_string space done;
         print_string space;
         print_string (blue vertical);
         print_string space;
         if result then print_string (green equal)
         else print_string (red not_equal);
         print_string space;
         print_string (blue vertical);
         print_string newline)
      results;
    print_string (blue bottom_left);
    print_string (blue horizontal);
    for i = 0 to name_width do
      print_string (blue horizontal) done;
    print_string (blue horizontal);
    print_string (blue bottom_centre);
    print_string (blue horizontal);
    for i = 0 to equal_width do
      print_string (blue horizontal) done;
    print_string (blue horizontal);
    print_string (blue bottom_right);
    print_endline newline
  end

let check cattr cloc utf8 color pkgs =
  let pkgs =
    if pkgs = [] then get_packages switch1
    else return pkgs
  in
  pkgs >>= fun pkgs ->
  let res =
    List.fold_left
      (fun acc pkg ->
         acc >>= fun () ->
         compare_pkg cattr cloc pkg >>= fun results ->
         print_result utf8 color pkg results;
         success)
      success pkgs
  in
  res >>= fun () -> success

let write_signature sg =
  let tmp, ch = Filename.open_temp_file "compiler_eq" ".sg" in
  let fmt = Format.formatter_of_out_channel ch in
    Printast.interface fmt sg;
    close_out ch;
    tmp

let diff_signature sg1 sg2 =
  let tmp1 = write_signature sg1 in
  let tmp2 = write_signature sg2 in
    check_system_relaxed
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
    check_system_relaxed
      ("diff -u " ^ tmp1 ^ " " ^ tmp2)

let diff cattr cloc pkg file =
  build_dir switch1 pkg >>= fun build1 ->
  build_dir switch2 pkg >>= fun build2 ->
  let cmt1 = build1 / file in
  let cmt2 = build2 / file in
  let open Cmt_format in
  let tt1 = read_cmt cmt1 in
  let tt2 = read_cmt cmt2 in
    match tt1.cmt_annots, tt2.cmt_annots with
    | Interface sg1, Interface sg2 ->
        diff_signature
          (clean_signature cattr cloc sg1)
          (clean_signature cattr cloc sg2)
    | Implementation str1, Implementation str2 ->
        diff_structure
          (clean_structure cattr cloc str1)
          (clean_structure cattr cloc str2)
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
  Arg.(value (pos_all string [] & info ~docv ~doc []))

let package =
  let docv = "PACKAGE" in
  let doc = "package" in
  Arg.(required (pos 0 (some string) None & info ~docv ~doc []))

let file =
  let docv = "FILE" in
  let doc = "file" in
  Arg.(required (pos 1 (some string) None & info ~docv ~doc []))

let unicode =
  let docv = "BOOL" in
  let doc = "unicode output" in
  Arg.(value (opt ~vopt:true bool true & info ~docv ~doc ["unicode"]))

let color =
  let docv = "BOOL" in
  let doc = "colored output" in
  Arg.(value (opt ~vopt:true bool true & info ~docv ~doc ["color"]))

let ignore_attributes =
  let docv = "BOOL" in
  let doc = "ignore attributes" in
  Arg.(value (opt ~vopt:true bool false
                & info ~docv ~doc ["ignore-attributes"]))

let ignore_locations =
  let docv = "BOOL" in
  let doc = "ignore locations" in
  Arg.(value (opt ~vopt:true bool false
                & info ~docv ~doc ["ignore-locations"]))

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
  Term.(ret (pure check
             $ ignore_attributes
             $ ignore_locations
             $ unicode
             $ color
             $ packages))

let check_info =
  let doc = "compare compilers" in
    Term.info "check" ~doc

let check_cmd = (check_term, check_info)

let diff_term =
  Term.(ret (pure diff
             $ ignore_attributes
             $ ignore_locations
             $ package
             $ file))

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
