type square =
  | Property of {
      position : int;
      name : string;
      cost : int;
      rent : int;
      rent_with_1_house : int;
      rent_with_2_houses : int;
      rent_with_3_houses : int;
      rent_with_4_houses : int;
      rent_with_hotel : int;
      houses_cost : int;
      hotels_cost : int;
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

let (mediterranean_avenue : square) =
  Property
    {
      position = 1;
      name = "Mediterranean Avenue";
      cost = 60;
      rent = 2;
      rent_with_1_house = 10;
      rent_with_2_houses = 30;
      rent_with_3_houses = 90;
      rent_with_4_houses = 160;
      rent_with_hotel = 250;
      houses_cost = 50;
      hotels_cost = 50;
      mortgage = 30;
      unmortgage = 33;
    }

let new_board = [ mediterranean_avenue ]
let clear_board b = new_board
