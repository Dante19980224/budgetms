
type coordinate = int*int

type level = { pos: coordinate;
               points: int;
               walls: coordinate list; (* border *)
               mines: coordinate list; (* total of 10 *)
               places: coordinate list; 	(* all not yet revealed blocks *)
               revealed: coordinate list; (* all revealed blocks *)
               marked: coordinate list; (* marked blocks *)
               }

type direction = North | East | South | West

let next_pos (x,y) = function
  | North -> (x,y+1)
  | East -> (x+1,y)
  | South -> (x,y-1)
  | West -> (x-1,y)

let rec toggle_marked marked pos =
  match marked with 
  | [] -> [pos]
  | (x,y)::xs when (x,y)=pos -> xs
  | x::xs -> x:: toggle_marked xs pos

(* return marked *)
(* if block not marked, add to marked. If block already marked, remove from marked *)
let rec mark_block revealed marked pos =
  if (List.mem pos revealed)
  then marked
  else toggle_marked marked pos

let rec auto_unmark_h revealed marked =
  match marked with
  | [] -> []
  | x::xs when List.mem x revealed -> xs
  | x::xs -> x:: auto_unmark_h revealed xs

let auto_unmark lev =
  { pos = lev.pos; points = lev.points; walls = lev.walls; mines = lev.mines; places = lev.places; revealed = lev.revealed; marked = auto_unmark_h lev.revealed lev.marked}

let step lev dir =
  let npos = next_pos lev.pos dir
  in if (List.mem npos lev.walls) 
  then lev                             (* hit a wall, cannot move *)
  else { pos = npos; points = lev.points; walls = lev.walls; mines = lev.mines; places = lev.places; revealed = lev.revealed; marked = lev.marked}

exception End

let chk_sur (x,y) mines =
  let num = ref 0 in
  begin
    if List.mem (x-1,y-1) mines
    then num := !num+1;
    if List.mem (x-1,y) mines
    then num := !num+1;
    if List.mem (x-1,y+1) mines
    then num := !num+1;
    if List.mem (x,y-1) mines
    then num := !num+1;
    if List.mem (x,y+1) mines
    then num := !num+1;
    if List.mem (x+1,y-1) mines
    then num := !num+1;
    if List.mem (x+1,y) mines
    then num := !num+1;
    if List.mem (x+1,y+1) mines
    then num := !num+1;
    !num
  end

let rec rm_pl (x,y) pl =
  match pl with
  | [] -> []
  | (a,b)::xs when a = x && b=y -> xs
  | a::xs -> a:: rm_pl (x,y) xs

let c_pos_lst (x,y) =
  [(x-1,y-1);(x-1,y);(x-1,y+1);(x+1,y-1);(x+1,y);(x+1,y+1);(x,y-1);(x,y+1)]

let rec cln_lst lst lev =
  match lst with
  | [] -> []
  | x::xs when List.mem x lev.revealed || List.mem x lev.walls -> cln_lst xs lev
  | x::xs -> x:: cln_lst xs lev

(* unique add(no dup) *)
let rec add_blk pos revealed =
  match revealed with
  | [] -> [pos]
  | x::xs when x = pos-> x::xs
  | x::xs -> x::add_blk pos xs

(* reveal algo *)
(* reveals all blocks adjacent to the selected block, and apply the same to all blocks revealed in this process, until cannot reveal anymore blocks *)
let reveal_main po lev =
  let lev_ref = ref lev in
  
  let rec reveal p =
    match p with
    | x when List.mem x (!lev_ref).walls -> ()      (* is it a wall? *)
    | x when List.mem x (!lev_ref).mines -> lev_ref := { pos = (!lev_ref).pos; points = (!lev_ref).points; walls = (!lev_ref).walls; mines = (!lev_ref).mines; places = rm_pl p ((!lev_ref).places); revealed = add_blk x (!lev_ref).revealed; marked = (!lev_ref).marked}      (* is it a mine block? *)
    | x when List.mem x (!lev_ref).revealed -> ()   (* has it been revealed already? *)
    | x -> big_reveal x       (* x not yet revealed *)
  and big_reveal cur =
    let init_rev = { pos = (!lev_ref).pos; points = (!lev_ref).points; walls = (!lev_ref).walls; mines = (!lev_ref).mines; places = rm_pl cur ((!lev_ref).places); revealed = add_blk cur (!lev_ref).revealed; marked = (!lev_ref).marked} in
    if (chk_sur cur (!lev_ref).mines) != 0
    then lev_ref := init_rev (* ONLY added current position to revealed, regardless of whether current position is a mine block or not. *)
    else let pos_lst = cln_lst (c_pos_lst cur) !lev_ref in
          begin
            lev_ref := init_rev;
            List.iter reveal pos_lst
          end

  in begin
    reveal po;
    (!lev_ref)
  end

let process_key lev  = function
  | 'w' -> step lev  North
  | 'a' -> step lev  West
  | 's' -> step lev  South
  | 'd' -> step lev  East
  | 'm' -> { pos = lev.pos; points = lev.points; walls = lev.walls; mines = lev.mines; places = lev.places; revealed = lev.revealed; marked = (mark_block lev.revealed lev.marked lev.pos)}
  | 'n' -> reveal_main lev.pos lev
  | _ -> lev


let erase_state {pos=(x,y); points=p; walls = ws; mines= ms; places = ps; revealed = rd; marked = md}  =
  Grid.erase_coordinates (md@rd@[(x,y)])

let get_num (x,y) mines =
  let num = chk_sur (x,y) mines in
  if num=0
  then " "
  else  string_of_int num     (* if some of the surrounding blocks are mines, then draw the number of adjacent mine blocks *)
    
let rec intersect lst1 lst2 =
  match lst1 with
  | [] -> []
  | x::xs when List.mem x lst2 -> x:: intersect xs lst2
  | x::xs -> intersect xs lst2

let draw_state {pos=(x,y); points=p; walls = ws; mines= ms; places = ps; revealed = rd; marked = md}  =
  List.iter (fun c -> Grid.color_box c (Graphics.rgb 74 114 178)) rd;
  Grid.draw_string_in_box_at (x,y) "( )";
  List.iter (fun c -> Grid.draw_string_in_box_at c "M") md;
  List.iter (fun c -> Grid.draw_string_in_box_at c "X") (intersect ms rd);
  List.iter (fun c -> Grid.draw_string_in_box_at c (get_num c ms)) rd

let draw_winning_state {pos=(x,y); points=p; walls = ws; mines= ms; places = ps; revealed = rd; marked = md}  =
  List.iter (fun c -> Grid.color_box c (Graphics.rgb 74 114 178)) rd;
  List.iter (fun c -> Grid.draw_string_in_box_at c "X") ms;
  List.iter (fun c -> Grid.draw_string_in_box_at c (get_num c ms)) rd

let draw_losing_state {pos=(x,y); points=p; walls = ws; mines= ms; places = ps; revealed = rd; marked = md}  =
  List.iter (fun c -> Grid.color_box c (Graphics.rgb 74 114 178)) rd;
  List.iter (fun c -> Grid.color_box c Graphics.red) ms;
  Grid.draw_string_in_box_at (x,y) "( )";
  List.iter (fun c -> Grid.draw_string_in_box_at c "X") ms;
  List.iter (fun c -> Grid.draw_string_in_box_at c (get_num c ms)) rd



let winning_state {pos=(x,y); points=p; walls = ws; mines= ms; places = ps; revealed = rd; marked = md} =
  List.for_all (fun c -> List.mem c ms) ps  (* all of the remaining unrevealed blocks are in ms(list of blocks that are mine blocks) *)

let losing_state {pos=(x,y); points=p; walls = ws; mines= ms; places = ps; revealed = rd; marked = md} =
  intersect ms rd != []


let rec main_loop (level:level):level =
  if winning_state level
  then begin
        draw_winning_state level;
        (Grid.draw_string_at (250,520) "You won!");
        let _ = Graphics.wait_next_event [Graphics.Key_pressed]
        in     level
      end
  else begin
        draw_state level;
        if losing_state level
        then 
          begin
            draw_losing_state level;
            (Grid.draw_string_at (250,520) "You lost!");
            let _ = Graphics.wait_next_event [Graphics.Key_pressed]
            in     level
          end
        else 
          let s = Graphics.wait_next_event [Graphics.Key_pressed]
          in let key = s.Graphics.key
          in if key = 'q'
              then level
              else let _= erase_state level in
                   main_loop (auto_unmark (process_key level key))
      end


let draw_level { walls = ws; places = ps}  = 
  List.iter (fun c -> Grid.color_box c Graphics.black) ws

let rec add_row w h =
  if w = 0
  then []
  else (w, h):: add_row (w-1) h

let rec mk_col w h = 
  if h = (-1)
  then []
  else (w, h):: mk_col w (h-1)

let rec add_col w h =
  if h = 0
  then []
  else (add_row w h)@ (add_col w (h-1))

let uni_add p lst =
  let len = List.length (add_blk p lst) in
  let len2 = List.length lst in
  if len = len2
  then false
  else true

(* number of mines(less than width*height) -> width -> height -> list of mine block locations *)
let rec rand_mines num_of_mines w h lst =
  match num_of_mines with
  | 0 -> lst
  | _ -> begin
            Random.self_init ();
            let po = ( (Random.int (w-1))+1, (Random.int (h-1))+1) in
            if uni_add po lst
            then rand_mines (num_of_mines-1) w h (add_blk po lst)
            else rand_mines num_of_mines w h lst
        end

let rec gen_wall w h =
  let r1 = add_row w 0 in
  let r2 = add_row w (h+1) in
  let c1 = mk_col 0 (h+1) in
  let c2 = mk_col (w+1) (h+1) in
  r1@r2@c1@c2


(* 5x5, 5 mines *)
let level_1 =
  { pos=(1,1);
    points=0;
    walls = (gen_wall 5 5);
    mines = (rand_mines 5 5 5 []);
    places = (add_col 5 5);
    revealed = [];
    marked = []}

(* 10x10, 10 mines *)
let level_2 =
  { pos=(1,1);
    points=0;
    walls = (gen_wall 10 10);
    mines = (rand_mines 10 10 10 []);
    places = (add_col 10 10);
    revealed = [];
    marked = []}

(* 10x10, 20 mines *)
let level_3 =
  { pos=(1,1);
    points=0;
    walls = (gen_wall 10 10);
    mines = (rand_mines 20 10 10 []);
    places = (add_col 10 10);
    revealed = [];
    marked = []}


let levels = [level_1; level_2; level_3]

let main () = 
  Graphics.open_graph " 600x600";
  Grid.draw_string_at (100,550) "wsad for movement; m to mark/unmark; n to reveal block; q to quit";
  Grid.draw_grid ();
  draw_level level_1;   
  ignore @@ main_loop level_1;
  Graphics.close_graph ()

let select_level n = 
  if n < 1 || n > 3
  then failwith "available levels 1-3"
else  
  Graphics.open_graph " 600x600";
  Grid.draw_string_at (100,550) "wsad for movement; m to mark/unmark; n to reveal block; q to quit";
  Grid.draw_grid ();
  draw_level (List.nth levels (n-1));   
  ignore @@ main_loop (List.nth levels (n-1));
  Graphics.close_graph ()
