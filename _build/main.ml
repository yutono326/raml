
let sys_time = Sys.time

open Core

open Format
open Toolbox
open Rconfig

let raml_version = "1.5.0"
let raml_release_date = "June 2020"
let raml_authors = ["Jan Hoffmann (Carnegie Mellon)"
		   ;"Shu-Chun Weng (Google)"
                   ;"Benjamin Lichtman (Carnegie Mellon)"
                   ;"Chan Ngo (Carnegie Mellon)"
                   ;"Ankush Das (Carnegie Mellon)"
                   ;"Yue Niu (Carnegie Mellon)"
                   ;"Di Wang (Carnegie Mellon)"
		   ]
let raml_website = "http://www.raml.co"

let _ = Config.load_path := [""; !Rpath.ocaml_raml_runtime]

let print_welcome () =
  printf "\nResource Aware ML, Version %s, %s\n\n" raml_version raml_release_date

let sys_name = Sys.executable_name

let print_usage () =
  printf ("Usage:\n" ^^
          "  %s action [-m] [prog.raml] [func_name]\n" ^^
          "\n" ^^
          "    prog.raml            Input file.\n" ^^
          "    -m                   Module mode: print bounds for all toplevel functions.\n" ^^
          "    func_name            Under the module mode: print the bound for the specified toplevel function.\n" ^^
          "\n" ^^
          "  If the file name is absent, %s reads from the standard input.\n" ^^
          "\n" ^^
          "  Actions: \n" ^^
          "    eval cost            Evaluate the input program and print resource-usage\n" ^^
          "                         information for the built-in metrics.\n" ^^
          "\n" ^^
          "    eval                 Evaluate the input program. Print the return value and\n" ^^
          "                         resource-usage information for the built-in metrics.\n" ^^
          "\n" ^^
          "    analyze [mode] <metric> [<d1>] <d2> [-print (all | none | consume | level <lev> )]\n" ^^
	  "                         analyze the input program and print bounds.\n" ^^
          "                         [mode]      The analysis mode: upper, lower, or constant.\n" ^^
          "                                     The default mode is upper.\n" ^^
          "                         <metric>    Metric for the analysis. Built-in metrics\n" ^^
          "                                     are heap, steps, ticks, and flips.\n" ^^
          "                         [<d1>] <d2> Maximal degree of the candidate bounds.\n" ^^
	  "                                     RAML successevly looks for bounds of maximal\n" ^^
          "                                     degree d1, d1+1, ..., d2. If d1 is omitted\n"^^
	  "                                     then d1 = d2.\n" ^^
          "                         -print      Print the types used in function applications.\n" ^^
          "                                     all          Print all.\n" ^^
          "                                     none         Print none.\n" ^^
          "                                     consume      Print types of consume functions.\n" ^^
          "                                     level <lev>  Print types withing depth <lev> in\n" ^^
          "                                                  the syntax tree.\n" ^^
          "\n" ^^
          "    usage                Print this message.\n" ^^
          "    gen-runtime          Generates the raml runtime\n" ^^
          "    version              Print version information.\n" ^^
          "    typecheck            Typecheck the input program.\n" ^^
          "    print simple[+t]     Print simplified syntax tree [with types].\n" ^^
          "    print sharelet[+t]   Print syntax tree in share-let-normal form [with types].\n" ^^
          "\n" ^^
          "  Example usage:\n" ^^
          "    %s analyze heap 4\n" ^^
          "    %s analyze lower heap 2 bfs.raml\n" ^^
          "    %s analyze steps 3 -print level 1 -m quicksort.raml\n" ^^
          "    %s eval quicksort.raml\n@.")
          sys_name sys_name sys_name sys_name sys_name sys_name


let print_usage_error error_msg =
  printf (
    "%s\n" ^^
    "Run '%s usage' for usage information.\n@."
  ) error_msg sys_name


let print_info () =
  let rec print_authors authors pref =
    match authors with
      | [] -> ()
      | auth::auths ->
	printf "%s%s\n" pref auth;
	print_authors auths pref
  in
  printf "Authors:\n";
  print_authors raml_authors "  ";
  printf (
    "Website:\n" ^^
    "  %s\n@."
  ) raml_website


let print_expression print_types form e =
  printf "%s:\n@." form
   ; Pprint.print_expression ~print_types e
   ; printf "\n@."


let print_module print_types form m =
  let fprint_raml_type f t = Pprint.fprint_raml_type f t
  in printf "%s:\n@." form
   ; List.iter m (fun (f, e) ->
         printf "===== %s : %a =====@."
           f fprint_raml_type e.Expressions.exp_type
       ; Pprint.print_expression ~print_types e
       ; printf "\n@.")


let tcheck_prog e env =
  printf "Typechecking expression ...\n"
  ; Typecheck.typecheck ~linear:false e env
  ; printf "  Typecheck successful.\n"
  ; ignore @@ Typecheck.typecheck_stack ~linear:false e env
  ; printf "  Stack-based typecheck successful.\n@."


let tcheck_module fn m env =
  printf "Typechecking module %s ...\n" fn
  ; let f (g,e) =
      Typecheck.typecheck ~linear:false e env
    in
    List.iter ~f m
  ; printf "  Typecheck successful.\n"
  ; let f (g,e) =
      ignore @@ Typecheck.typecheck_stack ~linear:false e env
    in
    List.iter ~f m
  ; printf "  Stack-based typecheck successful.\n@."


let eval ?(cost_only=false) e =
    printf "Evaluating expression ...\n"
  ; let metrics = [Metric.m_eval; Metric.m_tick; Metric.m_heap; Metric.m_flip] in
    let (result, costs) = Eval.evaluate e metrics in
    if cost_only then
      ()
    else begin
      printf "\n  Return value:\n    "
      ; match result with
        | Some loc_heap -> Pprint.print_value loc_heap
        | None -> printf "Exception (undefined)"
    end;
    match costs with
    | [(eval1,_); (tick1,_); (heap1,_); (flip1,_)] ->
      printf (
        "\n" ^^
	"  Evaluation steps: %.2f\n" ^^
        "  Ticks:            %.2f\n" ^^
        "  Heap space:       %.2f\n" ^^
        "  Flips:            %.2f\n@."
      ) eval1 tick1 heap1 flip1
    | _ -> Misc.fatal_error "This is dead code."

let print_data amode_name m_name degree time constr =
  printf (
    "\n" ^^
      "  Mode:          %s\n" ^^
      "  Metric:        %s\n" ^^
      "  Degree:        %d\n" ^^
      "  Run time:      %.2f seconds\n" ^^
      "  #Constraints:  %d\n@."
  ) amode_name m_name degree time constr


let analyze_prog analysis_mode m_name metric deg1 deg2 collect_fun_types e env =
  let start_time = sys_time () in
  let amode_name =
    match analysis_mode with
    | Mupper -> "upper"
    | Mlower -> "lower"
    | Mconstant -> "constant"
  in
  let () = tcheck_prog e env in
  let e_normal = Shareletnormal.share_let_normal "#" e in
  let e_normal_stack = Typecheck.typecheck_stack ~linear:true e_normal env in
  let () = printf "Analyzing expression ...\n" in
  let rec analyze_exp deg deg_max =
    assert (deg <= deg_max);
    assert (deg >= 1);
    printf "%i" deg;
    let module Clp = (
      val (
        match analysis_mode with
        | Mlower -> (
            module Solver.Clp( Solver.Clp_std_maximize )
          )
        | Mupper
        | Mconstant -> (
            module Solver.Clp( Solver.Clp_std_options )
          )
      ) : Solver.SOLVER
    )
    in
    let module Amode =
    struct
      let mode = analysis_mode
    end
    in
    let module Analysis = Analysis.Make( Clp )(Amode) in
    match (try Analysis.analyze_expression e_normal_stack ~metric ~degree:deg ~collect_fun_types with Analysis.Anno_exn _ | Analysis.Analysis_error _ -> None) with
    | None ->
      let _ = if deg < deg_max then printf ", %!" else printf "\n%!" in
      if deg < deg_max then
	analyze_exp (deg+1) deg_max
      else
	begin
	  let _ = printf "\n  No bound could be derived. The linear program is infeasible.\n" in
	  let constr = Clp.get_num_constraints () in
	  let time = sys_time () -. start_time in
	  print_data amode_name m_name deg time constr
	end
    | Some (q,fun_type_list) ->
      let _ =
	match fun_type_list with
	| [] -> ()
	| _ ->
	  begin
	    let () = printf "\n\n  Function types:\n" in
	    let print_fun_types atype =
	      Pprint.print_anno_funtype ~indent:("  ") ~simple_name:true atype
	    in
	    let _ = List.iter fun_type_list print_fun_types in
	    printf "===="
	  end
      in
      let _ = printf "\n\n  Derived %s bound: %.2f\n" amode_name q in
      let constr = Clp.get_num_constraints () in
      let time = sys_time () -. start_time in
      print_data amode_name m_name deg time constr
  in
  printf "\n  Trying degree: ";
  analyze_exp deg1 deg2

let analyze_module analysis_mode m_name metric deg1 deg2 collect_fun_types fname m env =

  let amode_name =
    match analysis_mode with
    | Mupper -> "upper"
    | Mlower -> "lower"
    | Mconstant -> "constant"
  in

  let analyze_fun f_name e =
    let start_time = sys_time () in
    let e_normal = Shareletnormal.share_let_normal "#" e in
    let e_normal_stack = Typecheck.typecheck_stack ~linear:true e_normal env in
    let () = printf "Analyzing function %s ...\n" f_name in
    let rec analyze_f deg deg_max =
      assert (deg <= deg_max);
      assert (deg >= 1);
      printf "%i" deg;
      let module Clp = (
        val (
          match analysis_mode with
          | Mlower -> (
              module Solver.Clp( Solver.Clp_std_maximize )
            )
          | Mupper
          | Mconstant -> (
              module Solver.Clp( Solver.Clp_std_options )
            )
        ) : Solver.SOLVER
      )
      in
      let module Amode =
      struct
        let mode = analysis_mode
      end
      in
      let module Analysis = Analysis.Make( Clp )(Amode) in
      match (try Analysis.analyze_function e_normal_stack ~metric ~degree:deg ~collect_fun_types with Analysis.Anno_exn _ | Analysis.Analysis_error _ -> None) with
      | None ->
	let _ = if deg < deg_max then printf ", %!" else printf "\n%!" in
	if deg < deg_max then
	  analyze_f (deg+1) deg_max
	else
	  begin
	    let _ = printf "\n  A bound for %s could not be derived. The linear program is infeasible.\n" f_name in
	    let constr = Clp.get_num_constraints () in
	    let time = sys_time () -. start_time in
	    printf "\n--";
	    print_data amode_name m_name deg time constr;
	    printf "====\n\n"
	  end
      | Some (atarg,atres,fun_type_list) ->
	begin
	  printf "\n%!";
	  let _ = Pprint.print_anno_funtype ~indent:("  ") (f_name, atarg, atres) in
	  let constr = Clp.get_num_constraints () in
	  let time = sys_time () -. start_time in
	  printf "--";
	  print_data amode_name m_name deg time constr;
          let () =
            if List.length fun_type_list = 0 then
              printf "====\n\n"
            else
	      match fun_type_list with
	      | [] -> ()
	      | _ ->
	        begin
	          let () = printf "-- Function types:\n" in
	          let print_fun_types atype =
	            Pprint.print_anno_funtype ~indent:("  ") ~simple_name:true atype
	          in
	          let _ = List.iter fun_type_list print_fun_types in
                  printf "====\n\n"
	        end
          in
          ()
	end
    in
    let _ = printf "\n  Trying degree: " in
    analyze_f deg1 deg2
  in
  let () = tcheck_module fname m env in
  let f (f_name,e) =
    match e.Expressions.exp_type with
    | Rtypes.Tarrow _ -> analyze_fun f_name e
    | _ -> ()
  in
  List.iter ~f m


let rec open_implicit_module m env =
  try
    Env.open_pers_signature m env
  with Not_found ->
  try
      gen_pervasives (String.lowercase m ^ ".mli");
      Env.open_pers_signature m env
  with Not_found ->
    Misc.fatal_error (Printf.sprintf "cannot open implicit module %S" m)

and initial_env (() : unit) : Env.t =
  Ident.reinit();
  let env =
    if !Clflags.nopervasives
    then Env.initial
    else
      open_implicit_module "Pervasives" Env.initial
  in env

and parse_interface (env : Env.t) (mli : string) : Parsetree.signature =
  let file_name = Filename.concat !Rpath.ocaml_raml_runtime mli in
  let ch = In_channel.create file_name in
  let buf = Lexing.from_channel ch in
  let _ = Location.init buf file_name in
  let parsetree = Parse.interface buf in
  let _ = In_channel.close ch in
  parsetree

and interface sourcefile env_inital =
  Location.input_name := sourcefile;
  let modulename =
    String.capitalize(Filename.basename (Misc.chop_extension_if_any sourcefile)) in
  let outputprefix = Misc.chop_extension_if_any sourcefile in
  Env.set_unit_name modulename;
  let ast = parse_interface env_inital sourcefile in
  let tsg = Typemod.transl_signature env_inital ast in
  let sg = tsg.sig_type in
  ignore (Includemod.signatures env_inital sg sg);
  begin
    let _ = printf "  Compiling %s " modulename in
    let sg = Env.save_signature sg modulename
        (Filename.concat !Rpath.ocaml_raml_runtime (outputprefix ^ ".cmi")) in
    let _ =
      Typemod.save_signature modulename tsg outputprefix sourcefile
        env_inital sg ;
    in
    printf "... done.\n"
  end

and gen_pervasives sourcefile = interface sourcefile Env.initial
and gen_runtime sourcefile = interface sourcefile (initial_env ())

let main argv =
  let args = List.tl_exn (Array.to_list argv) in

  let main_cont args action_p action_m =
    match args with
    | [] ->
      let buf = Lexing.from_channel In_channel.stdin in
      let _ = Location.init buf "<stdin>" in
      let (e, env) = Parseraml.parse_raml buf in
      action_p e env
    | ["-m"]
    | ["-module"] ->
      print_usage_error "Expecting file name of the module."
    | [file] ->
      if Sys.file_exists file <> `Yes then
	print_usage_error ("File '" ^ file ^ "' not found.")
      else
	let (e, env) = Parseraml.parse_raml_from_file file in
	action_p e env
    | ["-m";file]
    | ["-module";file] ->
      if Sys.file_exists file <> `Yes then
	print_usage_error ("File '" ^ file ^ "' not found.")
      else
	let (m, env) = Parseraml.parse_raml_module file in
	action_m file m env
    | ["-m";file;f_name]
    | ["-module";file;f_name] ->
      if Sys.file_exists file <> `Yes then
        print_usage_error ("File '" ^ file ^ "' not found.")
      else
        let (m, env) = Parseraml.parse_raml_module file in
        begin
          match List.find m ~f:(fun (f_name', _) -> String.equal f_name f_name') with
          | None -> print_usage_error ("Top-level function '" ^ f_name ^ "' not found.")
          | Some func -> action_m file [func] env
        end
    | _ ->
      let arg_string =
	let f str acc =
	  str ^ " " ^ acc
	in
	List.fold ~init:"" ~f args
      in
      print_usage_error ("Too many arguments: '"^ arg_string ^ "'.")
  in

  let () = print_welcome () in
  match args with
  | [] -> print_usage_error "Expecting an action to execute."
  | action::args ->
    match action with
    | "usage" -> print_usage ()
    | "gen-runtime" ->
      begin match List.hd args with
        | Some sourcefile ->
          gen_runtime sourcefile
        | None ->
          let () = printf "Generating runtime mli files.\n\n" in
          let () =
            List.iter persistent_modules (fun m -> gen_runtime (String.lowercase m ^ ".mli"))
          in
          printf "\n"
      end
    | "info"
    | "version" -> print_info ()
    | "type-check"
    | "typecheck" ->
      main_cont args tcheck_prog tcheck_module
    | "evaluate"
    | "eval" ->
      let (cost_only,args) =
	match args with
	| "cost"::args' -> (true,args')
	| _ -> (false,args)
      in
      let eval_p e env =
	tcheck_prog e env;
	eval ~cost_only e
      in
      let eval_m _ _ _ =
	print_usage_error "Modules cannot be evaluated."
      in
      main_cont args eval_p eval_m
    | "analyse"
    | "analyze" ->
      begin
        let (args,analysis_mode) =
          match args with
          | [] ->
	    print_usage_error ("The action 'analyze' as to be followed by a metric.")
	  ; exit(-1)
          | mode::args ->
            match mode with
            | "lower" -> (args,Mlower)
            | "constant" -> (args,Mconstant)
            | "upper" -> (args,Mupper)
            | _ -> (mode::args,Mupper) (*default*)
        in
        match args with
	| m_name::deg_str::args ->
	  let metric =
	    match m_name with
	    | "steps" -> Metric.m_eval
	    | "heap"  -> Metric.m_heap
	    | "ticks" -> Metric.m_tick
      | "flips" -> Metric.m_flip
	    | _ ->
	      print_usage_error ("The metric '" ^ m_name ^ "' is not a built-in metric.")
	    ; exit(-1)
	  in
	  let (args,deg1,deg2) =
	    let deg = Int.of_string deg_str in
	    if deg > 0 then
	      match args with
	      | arg::args when is_int arg ->
		let deg2 = Int.of_string arg in
		if deg2 > 0 then
		  if deg2 >= deg then
		    (args,deg,deg2)
		  else begin
		    print_usage_error  ("The degree '" ^ arg ^ "' has to be greater or equal to '" ^ deg_str ^ "'.")
      		  ; exit(-1)
		  end
		else begin
		  print_usage_error  ("The degree '" ^ arg ^ "' is not a positive number.")
		; exit(-1)
		end
	      | _ -> (args,deg,deg)
	    else begin
	      print_usage_error  ("The degree '" ^ deg_str ^ "' is not a positive number.")
	    ; exit(-1)
	    end
	  in
	  let (pmode,args) =
	    match args with
	    | "-print"::"all"::args ->
	      (Rconfig.Pall,args)
	    | "-print"::"none"::args ->
	      (Rconfig.Pnone,args)
	    | "-print"::"consume"::args ->
	      (Rconfig.Pconsume,args)
	    | "-print"::"level"::lev::args -> begin
		try
		  let lev = Int.of_string lev in
		  if lev >= 0 then
		    (Rconfig.Plevel lev,args)
		  else
		    raise (Invalid_argument "Negative number or zero.")
		with _ ->
		  print_usage_error  ("The level '" ^ lev ^ "' is not a non-negative number.")
		; exit(-1) end
	    | "-print"::_ ->
	      print_usage_error "The usage for the print mode is '-print (all | none | level <lev> )'."
	    ; exit(-1)
	    | _ -> (Rconfig.Pnone,args)
	  in
	  let analyze_p = analyze_prog analysis_mode m_name metric deg1 deg2 pmode in
	  let analyze_m = analyze_module analysis_mode m_name metric deg1 deg2 pmode in
	  main_cont args analyze_p analyze_m
	| _ ->
	  print_usage_error
	    "The action analyze has to be followed by a metric and a degree."
      end
    | "print" -> begin
	match args with
	| ("simple" as a)::args
	| ("simple+t" as a)::args ->
	  let with_types =
	    if String.is_suffix a "+t" then
	      true
	    else
	      false
	  in
	  let p_print e _ =
	    print_expression with_types "Simplified expression" e
	  in
	  let m_print _ m _ =
	    print_module with_types "Simplified module" m
	  in
	  main_cont args p_print m_print
	| ("sharelet" as a)::args
	| ("sharelet+t" as a)::args ->
	  let with_types =
	    if String.is_suffix a "+t" then
	      true
	    else
	      false
	  in
	  let p_print e env =
	    let _ = Typecheck.typecheck ~linear:false e env in
	    let e_normal = Shareletnormal.share_let_normal "#" e in
	    print_expression with_types "Expression in share-let-normal form" e_normal
	  in
	  let m_print _ m env =
	    let f (g,e) =
	      let _ = Typecheck.typecheck ~linear:false e env in
	      (g, Shareletnormal.share_let_normal "#" e)
	    in
	    let m_normal = List.map ~f m in
	    print_module with_types "Module in share-let-normal form" m_normal
	  in
	  main_cont args p_print m_print
	| [] -> print_usage_error "The action 'print' needs an argument."
	| a::args -> print_usage_error ("'"^a^"' is not a valid argument for the action 'print'.")
      end
    | _ -> print_usage_error ("'" ^ action ^ "' is not a valid action.")


let _ =
  Random.self_init ();
  main Sys.argv
