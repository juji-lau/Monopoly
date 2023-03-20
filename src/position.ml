type square =
  | Property of {
      index : int;
      name : string;
      set : string;
      cost : int;
      rent : int;
      rent_set : int;
      r_1house : int;
      r_2house : int;
      r_3house : int;
      r_4house : int;
      r_hotel : int;
      houses_cost : int;
      hotel_cost : int;
      mortgage : int;
      unmortgage : int;
    }
  | Railroad of {
      position : int;
      name : string;
      cost_owned_1 : int;
      cost_owned_2 : int;
      cost_owned_3 : int;
      cost_owned_4 : int;
      mortgage : int;
      unmortgage : int;
    }
  | Utility of {
      position : int;
      name : string;
      cost_owned_1 : int;
      cost_owned_2 : int;
      mortgage : int;
      unmortgage : int;
    }
  | Rent of {
      position : int;
      name : string;
      rent : int;
    }
  | Jail of {
      position : int;
      cost : int;
    }
  | Go_To_Jail
  | Chance
  | Free_Parking

type t = []

exception UnknownBoard of string


(*defines the types of the *)
let (mediterranean_avenue : square) =
  Property
    {
      index = 1;
      name = "Mediterranean Avenue";
      cost = 60;
      rent = 2;
      rent_set = 4;
      r_1house = 10;
      r_2houses= 30;
      r_3house = 90;
      r_4house = 160;
      r_hotel = 250;
      houses_cost = 50;
      hotel_cost = 50;
      mortgage = 30;
      unmortgage = 33;
    }

let new_board = [ mediterranean_avenue ]
let clear_board b = new_board
