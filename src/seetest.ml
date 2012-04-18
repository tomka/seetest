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

let print_menu () =
	print_string "Bitte wähle ob du den SBF Binnen oder den SBF See testen willst.\n" ;
	print_string "1. SBF Binnen\n";
	print_string "2. SBF See\n";
	print_string "3. Beenden\n";
	print_string "? " ;;

let rec get_mode () =
	let choice = read_int () in
	let valid_choice = choice=1 || choice=2 || choice=3 in
	if valid_choice then
		choice
	else begin
		print_string "Wie bitte? Ich habe die Eingabe nicht verstanden.\n? ";
		get_mode ()
	end ;;

let start ask =
	print_string "\n";
	ask ();;

let ask_questions_binnen () =
	() ;;

let ask_questions_see () =
	() ;;

let main () =
	print_menu ();
	let mode = get_mode () in
	if mode=mode_sbf_binnen then
		start ask_questions_binnen
	else if mode=mode_sbf_see then
		start ask_questions_see
	else if mode=mode_exit then
		exit 0
	else
		print_string "Unbekannte Modusauswahl." ;;

main () ;;
