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
let mode_print_qa = 3;;
let mode_exit = 4;;

let log msg =
	print_string msg;;

let display msg =
	print_string msg;;

let displayln msg =
	display msg; display "\n"

(* Mode selection *)

let print_menu () =
	(* Show the main menu to the user. *)
	displayln "Bitte wähle ob du den SBF Binnen oder den SBF See testen willst.";
	displayln "1. SBF Binnen";
	displayln "2. SBF See";
	displayln "3. Fragen und Antworden ausgeben";
	displayln "4. Beenden";
	display   "? " ;;

let rec get_mode () =
	(* Require an input from the user and treat it as choice of the main menu. *)
	let choice = read_int () in
	let valid_choice = choice=1 || choice=2 || choice=3 || choice=4 in
	if valid_choice then
		choice
	else begin
		display "Wie bitte? Ich habe die Eingabe nicht verstanden.\n? ";
		get_mode ()
	end ;;

let start ask qnr =
	print_string "\n";
	ask qnr;;

(* Support *)

let finish () =
	displayln "\nFragen beendet";;


class question =
	(** A question and a some answer options. One of them is the
		correct answer, which is stored as a seperate field. **)
object
	val mutable text = ""
	val mutable wrong_answers : string list = []
	val mutable correct_answer = ""

	method get_text = text
	method set_text new_text = text <- new_text
	method set_correct_answer value = correct_answer <- value
	method get_correct_answer = correct_answer
	method add_wrong_answer value  = wrong_answers <- value :: wrong_answers
	method get_wrong_answers = wrong_answers
	method get_num_all_answers = List.length wrong_answers + 1
end;;

class question_pool =
	(** A set of related questions. **)
object(self)
	val mutable name = ""
	val mutable questions = []

	method get_name = name
	method set_name new_name = name <- new_name
	method add_question = questions <- new question :: questions; List.hd questions
	method get_num_questions = List.length questions
	method get_questions = questions
end;;

class questionaire =
	(** A question pool encapsulates a set of question pools and meta
	    information about it. **)
object
	val mutable date = "(Not set)"
	val mutable about = "(Not set)"
	val mutable pools = []
	val mutable display_correct_answer = true
	val mutable ask_until_correct = true

	method get_date = date
	method set_date value = date <- value
	method get_about = about
	method set_about value = about <- value
	method add_question_pool = pools <- new question_pool :: pools; List.hd pools
	method get_num_pools = List.length pools
	method get_pools = pools

	method should_display_correct_answer = display_correct_answer
	method should_ask_until_correct = ask_until_correct
end;;

let load_question pool seq =
	(** Updates a question object with data from the list of mappings in seq. **)
	List.iter (function
		| YamlNode.MAPPING (_, map) ->
			(
			let q = pool#add_question in
			List.iter (function
				| (YamlNode.SCALAR (_, "text"), YamlNode.SCALAR(_, value)) ->
					(** Read in text of question **)
					q#set_text value
				| (YamlNode.SCALAR (_, "correctanswer"), YamlNode.SCALAR(_, value)) ->
					(** Read in correct answer **)
					q#set_correct_answer value
				| (YamlNode.SCALAR (_, "answer"), YamlNode.SCALAR(_, value)) ->
					(** Read in other answers **)
					q#add_wrong_answer value
				| _ -> ())
				map
			)
		| _ -> ())
		seq;
		();;

let load_data path qnr =
	(* Loads data from the given path as YAML file. *)
    displayln "Lade Daten...";
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
		  | YamlNode.MAPPING ( _, map) ->
			  List.iter
				(function
				 | (YamlNode.SCALAR (_, "date"), YamlNode.SCALAR (_, value)) ->
					(** Read in the date of the data **)
					qnr#set_date value
				 | (YamlNode.SCALAR (_, "about"), YamlNode.SCALAR (_, value)) ->
					(** Read in the about information of the data **)
					qnr#set_about value
				 | (YamlNode.SCALAR (_, "question-chapters"), YamlNode.SEQUENCE (_, values)) ->
					(** Read in the about information of the data **)
					List.iter (function
						| YamlNode.MAPPING ( _, map) ->
							begin
							let pool = qnr#add_question_pool in
							List.iter (function
								| (YamlNode.SCALAR (_, "name"), YamlNode.SCALAR (_, value)) ->
									(** Read in name of chapter  **)
									pool#set_name value
								| (YamlNode.SCALAR (_, "questions"), YamlNode.SEQUENCE (_, values)) ->
									begin
									load_question pool values
									end
								| _ -> ())
								map
							end
						| _ -> ())
						values
				| _ -> ())
				map
		  | _ -> ()
	  with YamlParser.Error (msg) ->
		prerr_endline msg;;

(* Questions *)

let print_questionaire qnr =
	let pools = qnr#get_pools in
	let n_pools = string_of_int qnr#get_num_pools in
	Printf.printf "Pools: %s\n" n_pools;
	List.iter (function
		| p ->
			begin
			let n_questions = p#get_num_questions in
			Printf.printf "\tPool \"%s\"" p#get_name;
			Printf.printf " with %d question(s):\n" n_questions;
			List.iter (function
				| q ->
					begin
					Printf.printf "\t\tQuestion: %s\n" q#get_text;
					Printf.printf "\t\t* %s\n" q#get_correct_answer;
					List.iter (function
						| a ->
							Printf.printf "\t\t- %s\n" a)
						q#get_wrong_answers
					end)
				p#get_questions
			end)
		pools;;

let swap a i j =
	(* Swap two elements of an array *)
	(* From: http://www.codecodex.com/wiki/Shuffle_an_array *)
    let t = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- t;;

let shuffle_array a =
	(* Shuffle an array by swapping each element with a random element. *)
	(* From: http://www.codecodex.com/wiki/Shuffle_an_array *)
    Array.iteri (fun i _ -> swap a i (Random.int (i+1))) a;;

let print_question q =
	(* Prints out a question and returns the index of the correct answer. *)
	Printf.printf "Frage: %s\n\n" q#get_text;
	let idxCorrectAnswer = ref (-1) in
	let numAnswers = q#get_num_all_answers in
	let idxAnswers = Array.init numAnswers (fun i -> i) in
	shuffle_array idxAnswers;
	let printer idx idxAnswer =
		begin
			print_int (idx + 1);
			print_string ": ";
			match idxAnswer with
				| 0 ->
					begin
					Printf.printf "%s\n" q#get_correct_answer;
					idxCorrectAnswer := idx + 1
					end
				| i ->
					begin
					let realIdx = i - 1 in
					let txt = List.nth q#get_wrong_answers realIdx in
					Printf.printf "%s\n" txt
					end
		end in
	Array.iteri printer idxAnswers;
	(* Return index of correct answer *)
	!idxCorrectAnswer;;

let randomized_iterator qnr =
	(* Walks a questionaire in a random order. While doing this, the user
	   is prompted for the correct answar to a question. Depending on the
	   settings a wrong answer leads for asking the linked question again. *)
	while true do
	begin
		let idxPool = Random.int qnr#get_num_pools in
		let pool = List.nth qnr#get_pools idxPool in
		let idxQuestion = Random.int pool#get_num_questions in
		let q = List.nth pool#get_questions idxQuestion in
		let ask = ref true in
		while !ask do
		begin
			let idxCorrectAnswer = print_question q in
			print_string "? ";
			let choice = read_int () in
			match choice with
				| c when c=idxCorrectAnswer ->
					begin
					displayln "Richtig!\n";
					ask := false
					end
				| _ ->
					(
					displayln "Falsch!";
					if qnr#should_display_correct_answer then Printf.printf "Richtig ist Antwort %d.\n\n" idxCorrectAnswer;
					ask := qnr#should_ask_until_correct;
					);
		end;
		done;
	end;
	done;
	();;

let ask_questions_binnen qnr =
	(* Prints out general information about the "Binnen" licence and starts
	   the asking of questings. *)
	displayln "Fragen zum Sportbootführerschein Binnen";
	displayln "***************************************";
	displayln "\n";
	randomized_iterator qnr;
	();;

let ask_questions_see () =
	(* Prints out general information about the "See" licence and starts
	   the asking of questings. *)
	displayln "Fragen zum Sportbootführerschein See";;

(* Main entry point *)

let main () =
	(* The main entry point of the program. *)
	Random.self_init();
	print_menu ();
	let mode = get_mode () in
	if mode=mode_sbf_binnen then
		(
		let path = "../data/Fragenkatalog-Binnen-Mai-2012.yaml" in
		let qnr = new questionaire in
		load_data path qnr;
		start ask_questions_binnen qnr
		)
	else if mode=mode_sbf_see then
		start ask_questions_see ()
	else if mode=mode_print_qa then
		let path = "../data/Fragenkatalog-Binnen-Mai-2012.yaml" in
		let qnr = new questionaire in
		load_data path qnr;
		print_questionaire qnr
	else if mode=mode_exit then
		exit 0
	else
		display "Unbekannte Modusauswahl." ;;

(* call main entry point *)
main () ;;
