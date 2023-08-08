open! Core
open! Yojson

(* Module that will store final round questions randomly selected from JSON
   file*)

let final_round_url : string =
  {|
								•	Tarot Cards<br><span class="bb_strike">o	The River Styx<br>o	The Shadowy One</span><br>o	<br><br>•	Worn Around the Neck<br><b>o	lei<br>o	foulard</b><br><span class="bb_strike">o	fez</span><br><br>•	Hogwarts Professors<br><span class="bb_strike">o	Daryl Van Horne</span><br><b>o	Pomona Sprout</b><br><span class="bb_strike">o	Agatha Harkness</span><br><br>•	London Tube Lines<br><b>o	Jubilee Line<br>o	Metropolitan Line</b><br><b>o	Queens Line</b><br><br>•	<i>Black Mirror</i> Episodes<br><span class="bb_strike">o	“Replay”</span><br><b>o	“The National Anthem”<br>o	“Hang the DJ”</b><br><br>•	James Patterson Novels<br><b>o	Hope to Die</b><br><span class="bb_strike">o	Fuzzy Wuzzy<br>o	Old Mother Hubbard</span><br><br>•	Canadian National Holidays<br><b>o	Boxing Day</b><br><span class="bb_strike">o	Proclamation <br>o	Picnic Day</span><br><br>•	Stage Names<br><b>o	Katy Perry</b><br><span class="bb_strike">o	Elton John</span><br><b>o	Drake</b><br><br>•	Vitamins<br><span class="bb_strike">o	vitamin L<br>o	vitamin X</span><br><b>o	vitamin B3</b><br><br>•	Cats in the Musical Cats<br><span class="bb_strike">o	Vagabond Prince</span><br><b>o	Old Deuteronomy</b><br><br>•	Has an Exoskeleton<br><span class="bb_strike">o	crocodile<br>o	cobra</span><br><b>o	lobster</b><br><br>•	Boy Scout Merit Badges<br><span class="bb_strike">o	Pokémon Go</span><br><b>o	Camping<br>o	Disabilities Awareness</b><br><br>•	Lunchables<br><b>o	Mexican Style Chicken Tacos<br>o	Chicken Pizza<br>o	Extra Cheesy Pizza</b><br><br>•	Gluten-Free<br><b>o	Sour cream<br>o	Skittles<br>o	Cottage cheese</b><br><br>•	Lyrics in Billy Joel’s <i>We Didn’t Start the Fire</i><br><b>o	Buddy Holly, “Ben Hur”, space monkey Mofia</b><br><span class="bb_strike">o	Tanzania, Michigan, Mr. Roger’s cardigan<br>o	Cincinnati, John Wayne, lobotomy of the brain</span><br><br>•	Scary Stories to Tell in the Dark<br><b>o	“The Hook”<br>o	“May I Carry Your Basket?”</b><br><span class="bb_strike">o	“That’s No Moon”</span><br><br>•	Boat-Related Occupations<br><b>o	skipper<br>o	coxswain</b><br><b>o	pinsetter</b><br><br>•	Olsen Twins Movies<br><b>o	To Grandmother’s House We Go</b><br><span class="bb_strike">o	Twinning<br>o	Mary-Kate and Ashly Rock Switzerland</span><br><br>•	Pulitzer Prize Categories<br><span class="bb_strike">o	News Entertainment Program</span><br><b>o	History<br>o	Investigative Reporting</b><br><br>•	Overwatch Characters<br><b>o	D.Va</b><br><span class="bb_strike">o	Gorilla Man</span><br><b>o	Symmetra</b><br><br>•	Car Engine Parts<br><b>o	flywheel<br>o	crankshaft</b><br>o	<br><br>•	Characters “Snapped” Away in <i>Avengers: Infinity War</i><br><b>o	Black Panther</b><br><span class="bb_strike">o	Hawkeye</span><br><b>o	Winter Soldier</b><br><br>•	People Who Have Walked on the Moon<br><b>o	Eugene Cernan</b><br><span class="bb_strike">o	Buzz McCallister</span><br><b>o	Charles Duke</b><br><br>•	The Greek Muses<br><b>o	Thalia of Comedy</b><br><span class="bb_strike">o	Thespia of Theater</span><br><b>o	Erato of Lyric Poetry</b><br><br>•	Carrer Cards in the “Modern” Edition of The Game of Life<br><span class="bb_strike">o	Defense Specialist<br>o	Explorer</span><b>o	Singer</b><br><br>•	Egyptian Gods &amp; Goddesses<br><span class="bb_strike">o	Hestia<br>o	Juno</span><br><b>o	Ra</b><br><br>•	Major Greenhouse Gases<br><b>o	Tetrafluoromethane</b><br><span class="bb_strike">o	Diatomic Nitrogen<br>o	Acetylene</span><br><br>•	Stranger Thangs Characters<br><b>o	Max Mayfield</b><br><span class="bb_strike">o	Elliot Taylor</span><br><b>o	The Demogorgon</b><br><br>•	Yogurt Brands<br><b>o	Icelandic Provisions<br>o	Dannon<br>o	Fage</b><br><br>•	Ranks of Organisms in Taxonomy<br><span class="bb_strike">o	County<br>o	Group<br>o	Brand</span><br><br>•	Detective on a Law &amp; Order Show<br><span class="bb_strike">o	Det. Jessica Fletcher</span><br><b>o	Det. Fin Tutuola<br>o	Det. Lennie Briscoe</b><br><br>•	Locations in Game of Thrones<br><b>o	Winterfell</b><br><span class="bb_strike">o	Brandos</span><br><b>o	The Wall</b><br><br>•	Metamorphic Rocks<br><b>o	slate</b><br><span class="bb_strike">o	clay</span><br>o	<br><br>•	Parts of an Animal Cell<br><span class="bb_strike">o	Amniotic sac</span><br><b>o	Cell membrane</b><br><span class="bb_strike">o	Impulse inhibitors</span><br><br>•	Root Vegetables &amp; Tubers<br><span class="bb_strike">o	turmeric<br>o	ginseng<br>o	radishes</span><br><br>•	Never Married<br><b>o	Jane Austen<br>o	Sir Isaac Newton</b><br><span class="bb_strike">o	Henry the VIII</span><br><br>•	Dairy Products<br><span class="bb_strike">o	tofu</span><br><b>o	ghee<br>o	kefir</b><br><br>•	Taller Than The Eiffel Tower<br><span class="bb_strike">o	The Great Pyramid of Giza</span><br><b>o	The Kingdom Clock Tower</b><br><span class="bb_strike">o	The Wash</span><br><br>•	<i>Star Trek</i> Captains<br><b>o	Philippa Georgiou</b><br><span class="bb_strike">o	Malcolm “Mal” Reynolds</span><br><b>o	Kathryn Janeway</b><br><br>•	Countries Bordering the Indian Ocean<br><b>o	Tanzania<br>o	Australia<br>o	Madagascar</b><br><br>•	Books Written in the First Person<br><span class="bb_strike">o	The Scarlet Letter</span><br><b>o	To Kill a Mockingbird</b><br>o	<br><br>•	Animals Statistically Likely to Outlive a Moose<br><b>o	giraffe</b><br><span class="bb_strike">o	dove</span><br><b>o	baboon</b><br><br>•	Animals With More Than Two Eyes<br><b>o	praying mantis<br>o	scallops<br>o	box jellyfish</b><br><br>•	<i>Kama Sutra</i> Chapters<br><b>o	On Personal Adornment<br>o	On Kissing</b><br><span class="bb_strike">o	On Structurally Sound Surfaces</span><br><br>•	Disney Animated Movies<br><b>o	Tangled<br>o	Moana</b><br><span class="bb_strike">o	The Swan Princess</span><br><br>•	Cat Breeds<br><b>o	Russian Blue<br>o	Maine ♥♥♥♥<br>o	Scottish Fold</b><br><br>•	Muscles in the Human Body<br><span class="bb_strike">o	reticulum</span><br><b>o	rectus abdominis</b><br><span class="bb_strike">o	delphinus</span><br><br>•	Sesame Street Residents<br><b>o	Aloysius Snuffleupagus</b><br><span class="bb_strike">o	Penny Proud</span><br><b>o	Oscar the Grouch</b><br><br>•	Quadrilaterals<br><b>o	kite</b><br><span class="bb_strike">o	tetrahedron<br>o	pentagon</span><br><br>•	Nobel Prize Winning Scientists<br><span class="bb_strike">o	Charles Darwin</span><br><b>o	Alexander Fleming<br>o	Niels Bohr</b>								<div style="clear: both"></div>
							|}
;;

type t =
  { category : string
  ; right_answers : string list
  ; wrong_answers : string list
  }
[@@deriving compare, equal, sexp_of]

(* Converting JSON file into Yojson datatype*)
let get_data_from_file filename = Yojson.Basic.from_file filename

let get_question_array file_name =
  let open Yojson.Basic.Util in
  let json = get_data_from_file file_name in
  json |> to_list
;;

let pick_random_question () : t =
  let open Yojson.Basic.Util in
  let array =
    get_question_array
      "/usr/local/home/jsipuser/triviamurderparty/lib/final_round_questions.json"
  in
  let elem = List.random_element_exn array in
  let c = elem |> member "category" |> to_string in
  let wrong_answers =
    elem
    |> member "W"
    |> to_list
    |> List.map ~f:(fun elem -> Basic.to_string elem)
  in
  let right_answers =
    elem
    |> member "R"
    |> to_list
    |> List.map ~f:(fun elem -> Basic.to_string elem)
  in
  { category = c; right_answers; wrong_answers }
;;

let print_random_question () =
  let q = pick_random_question () in
  print_s [%message "Random Question: " (q : t)]
;;

(* 

   1. separate by the bullet point 2. put stuff in bb -strike in a the W list
   3. put reats in the R list *)
