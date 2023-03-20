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
      index : int;
      name : string;
    }
  | Utility of {
      index : int;
      name : string;
    }
  | Rent of {
      index : int;
      name : string;
      rent : int;
    }
  | Jail of {
      index : int;
      cost : int;
    }
  | Go_To_Jail of {index : int}
  | Chance of {index : int}
  | Community_Chest of {index : int}
  | Free_Parking of {index : int}
  | Tax of {index : int}
  | Start of {index : int}

type t = []

exception UnknownBoard of string


(*defines the specific squares for a default board *)

let (start_sqr : square) = Start {index = 0;}
let (mediterranean_avenue : square) =
  Property
    {
      index = 1;
      name = "Mediterranean Avenue";
      set = "brown";
      cost = 60;
      rent = 2;
      rent_set = 4;
      r_1house = 10;
      r_2house= 30;
      r_3house = 90;
      r_4house = 160;
      r_hotel = 250;
      houses_cost = 50;
      hotel_cost = 50;
      mortgage = 30;
      unmortgage = 33;
    }
let first_Chest : square = Community_Chest {index = 2;}
let baltic_Avenue : square = Property
{
  index = 3;
  name = "Baltic Avenue";
  set = "brown";
  cost = 60;
  rent = 4;
  rent_set = 8;
  r_1house = 20;
  r_2house = 60; 
  r_3house = 180;
  r_4house = 320;
  r_hotel = 450;
  houses_cost = 50;
  hotel_cost = 50;
  mortgage = 30;
  unmortgage = 33;
} 
let first_Tax : square = Tax {index = 4}
let first_Rail : square = Railroad {
  index = 5;
  name = "Readings Railroad"
  }
let oriental_Avenue : square = Property
{
  index = 6;
  name = "Oriental Avenue";
  set = "l_blue";
  cost = 100;
  rent = 6;
  rent_set = 12;
  r_1house = 30;
  r_2house = 90;
  r_3house = 270;
  r_4house = 400;
  r_hotel = 550;
  houses_cost = 50;
  hotel_cost = 50;
  mortgage = 50;
  unmortgage = 55;
}
let first_Chance : square = Chance {index = 7}
let vermont_Avenue : square = Property
{
  index = 8;
  name = "Vermont Avenue";
  set = "l_blue";
  cost = 100;
  rent = 6; 
  rent_set = 12;
  r_1house = 30;
  r_2house = 90;
  r_3house = 270;
  r_4house = 400;
  r_hotel = 550;
  houses_cost = 50;
  hotel_cost = 50;
  mortgage = 50;
  unmortgage = 55;
}
let connecticut_Avenue : square = Property 
{
  index = 9;
  name = "Connecticut Avenue";
  set = "l_blue";
  cost = 120;
  rent = 8;
  rent_set = 16;
  r_1house = 40;
  r_2house = 100;
  r_3house = 300;
  r_4house = 450;
  r_hotel = 600;
  houses_cost = 50;
  hotel_cost = 50;
  mortgage = 60;
  unmortgage = 66;
}
let jail_Sqr : square = Jail {index = 10; cost = 50}
let charles_Palace : square = Property 
{
  index = 11;
  name = "St. Charles Palace";
  set = "pink";
  cost = 140;
  rent = 10;
  rent_set = 20;
  r_1house = 50;
  r_2house = 150;
  r_3house = 450;
  r_4house = 625;
  r_hotel = 750;
  houses_cost = 100;
  hotel_cost = 100;
  mortgage = 70;
  unmortgage = 77;
}
let electric : square = Utility 
{
  index = 12;
  name = "Electric Company"; 
}
let states_Avenue : square = Property
{
  index = 13;
  name = "States Avenue";
  set = "pink";
  cost = 140;
  rent = 10;
  rent_set = 20;
  r_1house = 50;
  r_2house = 150;
  r_3house = 450;
  r_4house = 625;
  r_hotel = 750;
  houses_cost = 100;
  hotel_cost = 100;
  mortgage = 70;
  unmortgage = 77;
}
let virginia_Avenue : square = Property 
{
  index = 14;
  name = "Virginia Avenue";
  set = "pink";
  cost = 160;
  rent = 12;
  rent_set = 24;
  r_1house = 60;
  r_2house = 180;
  r_3house = 500;
  r_4house = 700;
  r_hotel = 900;
  houses_cost = 100;
  hotel_cost = 100;
  mortgage = 80;
  unmortgage = 88 ;
}
let second_Rail : square = Railroad {index = 15; name = "Pennsylvania Railroad";}
let james_Place : square = Property 
{
  index = 16;
  name = "St. James Place";
  set = "orange";
  cost = 180;
  rent = 14;
  rent_set = 28;
  r_1house = 70;
  r_2house = 200;
  r_3house = 550;
  r_4house = 750;
  r_hotel = 950;
  houses_cost = 100;
  hotel_cost = 100;
  mortgage = 90;
  unmortgage = 99 ;
}
let second_Chest : square = Community_Chest {index = 17}
let tennessee_Avenue : square = Property 
{
  index = 18;
  name = "Tennessee Avenue";
  set = "orange";
  cost = 180;
  rent = 14;
  rent_set = 28;
  r_1house = 70;
  r_2house = 200;
  r_3house = 550;
  r_4house = 750;
  r_hotel = 950;
  houses_cost = 100;
  hotel_cost = 100;
  mortgage = 90;
  unmortgage = 99 ;
}
let ny_Avenue : square = Property
{
  index = 19;
  name = "New York Avenue";
  set = "orange";
  cost = 200;
  rent = 16;
  rent_set = 32;
  r_1house = 80;
  r_2house = 220;
  r_3house = 600;
  r_4house = 800;
  r_hotel = 1000;
  houses_cost = 100;
  hotel_cost = 100;
  mortgage = 100;
  unmortgage = 110 ;
}



let new_board = [start_sqr; mediterranean_avenue;first_Chest;baltic_Avenue;
first_Tax;first_Rail;oriental_Avenue;first_Chance;vermont_Avenue;
connecticut_Avenue;jail_Sqr;charles_Palace;electric;states_Avenue;
virginia_Avenue;second_Rail;james_Place;second_Chest;tennessee_Avenue;ny_Avenue ]
let clear_board b = new_board
