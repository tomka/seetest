(*
 This tool tests the user for questions of Germany's basic boat driving
 licence "Sportbootführerschein Binnen" (SBF Binnen) and
 "Sportbootführerschein See" (SBF See). The questions are presented as
 a multiple choice test, which in fact is the official test structure
 since of May 2012. *)

(* Initialization *)

let version = "0.1";;

Printf.printf "Seetest -- Version %s\n\n" version;;

let mode_sbf_binnen = 1;;
let mode_sbf_see = 2;;
let mode_exit = 3;;

let log msg =
	print_string msg;;

let display msg =
	print_string msg;;

let displayln msg =
	display msg; display "\n"

(* Mode selection *)

let print_menu () =
	displayln "Bitte wähle ob du den SBF Binnen oder den SBF See testen willst.";
	displayln "1. SBF Binnen";
	displayln "2. SBF See";
	displayln "3. Beenden";
	display   "? " ;;

let rec get_mode () =
	let choice = read_int () in
	let valid_choice = choice=1 || choice=2 || choice=3 in
	if valid_choice then
		choice
	else begin
		display "Wie bitte? Ich habe die Eingabe nicht verstanden.\n? ";
		get_mode ()
	end ;;

let start ask =
	print_string "\n";
	ask ();;

(* Support *)

let load_config =
    ();;

let finish () =
	displayln "\nFragen beendet";;

let load_data path =
	(* Loads data from the given path as YAML file. *)
    displayln "Lade Daten...";
	displayln path;
	let chan = open_in path in
	let content = Std.input_all chan in
	let parser = YamlParser.make () in
	  try
		let root =
		  YamlParser.parse_string
			parser
			content
			(* "---\n- one\n- two\n- three\n" *)
		in
		  match root with
		  | YamlNode.SCALAR (_, value) ->
			  displayln value
		  | YamlNode.SEQUENCE (_, nodes) ->
			  List.iter
				(function
				 | YamlNode.SCALAR (_, value) ->
					 print_endline value
				 | _ -> ())
				nodes
		  | YamlNode.MAPPING ( _, map) ->
			  List.iter
				(function
				 | (YamlNode.SCALAR (_, "date"), YamlNode.SCALAR (_, value)) ->
					 (** Read out the date of the data **)
					 displayln value;
				 | _ -> ())
				map
		  | _ -> ()
	  with YamlParser.Error (msg) ->
		prerr_endline msg;;


(* Questions *)

let ask_questions_binnen () =
	displayln "Fragen zum Sportbootführerschein Binnen";;

let ask_questions_see () =
	displayln "Fragen zum Sportbootführerschein See";;

(* Main entry point *)

let main () =
	print_menu ();
	let mode = get_mode () in
	if mode=mode_sbf_binnen then
		let path = "../data/Fragenkatalog-Binnen-Mai-2012.yaml" in
		load_data path;
		start ask_questions_binnen
	else if mode=mode_sbf_see then
		start ask_questions_see
	else if mode=mode_exit then
		exit 0
	else
		display "Unbekannte Modusauswahl." ;;

main () ;;
