
open Graphics ;;
open Unix ;;
open List ;;

(* images are lists of lists of floats between 0. (white) and 1. (black) *)
type image = float list list ;;
type size = int * int ;;
  
(* threshold thershold image -- image where pixels above the threshold
value are black *)
let threshold (img : image) (threshold : float) : image =
 	let checkthreshold (pixel: float) : float = 
 		if pixel <= threshold then 0. else 1. in
  	map (fun row -> map checkthreshold row) img 

(* dither max image -- dithered image *)
let dither (img : image) : image =
	let rolldice (pixel : float) : float = 
		if pixel > Random.float 1. then 1. else 0. in
	map (fun row -> map rolldice row) img

(* mirror image -- flip image left to right *)
let mirror (img : image) : image =
	map (fun row -> rev row) img

(* flip image -- flip image up and down *)
let flip (img : image) : image =
	rev img

(* show the image *)
let depict img =
  open_graph ""; 
  clear_graph ();
  let x, y = length (hd img), length img in resize_window x y;
  let depict_pix v r c = 
  	let lvl = int_of_float (255. *. (1. -. v)) in 
  	set_color (rgb lvl lvl lvl);
  	plot c (y - r) in
  iteri (fun rowindex row -> iteri 
  		(fun colindex pix -> depict_pix pix rowindex colindex) row) img;
  sleep 2;
  close_graph () ;;

let mona = Monalisa.image ;;
depict mona ;;
depict (threshold mona 0.75) ;;
depict (dither mona) ;;
depict (mirror mona) ;;
depict (flip mona) ;;
           
