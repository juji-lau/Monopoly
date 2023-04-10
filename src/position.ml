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
      building_cost : int;
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
  | Go_To_Jail of { index : int }
  | Chance of { index : int }
  | Community_Chest of { index : int }
  | Free_Parking of { index : int }
  | Tax of { index : int }
  | Start of {
      index : int;
      name : string;
    }

type t = []

let get_name s =
  match s with
  | Property p -> p.name
  | Railroad r -> r.name
  | Utility u -> u.name
  | Rent r -> r.name
  | Jail j -> "Jail"
  | Go_To_Jail g -> "Go to Jail"
  | Chance c -> "Chance"
  | Community_Chest c -> "Community Chest"
  | Free_Parking f -> "Free Parking"
  | Tax t -> "Tax"
  | Start s -> s.name



exception UnknownBoard of string

(*defines the specific squares for a default board *)

let (start_sqr : square) = Start { index = 0; name = "Go" }

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
      r_2house = 30;
      r_3house = 90;
      r_4house = 160;
      r_hotel = 250;
      building_cost = 50;
      mortgage = 30;
      unmortgage = 33;
    }

let first_Chest : square = Community_Chest { index = 2 }

let baltic_Avenue : square =
  Property
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
      building_cost = 50;
      mortgage = 30;
      unmortgage = 33;
    }

let first_Tax : square = Tax { index = 4 }
let first_Rail : square = Railroad { index = 5; name = "Readings Railroad" }

let oriental_Avenue : square =
  Property
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
      building_cost = 50;
      mortgage = 50;
      unmortgage = 55;
    }

let first_Chance : square = Chance { index = 7 }

let vermont_Avenue : square =
  Property
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
      building_cost = 50;
      mortgage = 50;
      unmortgage = 55;
    }

let connecticut_Avenue : square =
  Property
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
      building_cost = 50;
      mortgage = 60;
      unmortgage = 66;
    }

let jail_Sqr : square = Jail { index = 10; cost = 50 }

let charles_Place : square =
  Property
    {
      index = 11;
      name = "St. Charles Place";
      set = "pink";
      cost = 140;
      rent = 10;
      rent_set = 20;
      r_1house = 50;
      r_2house = 150;
      r_3house = 450;
      r_4house = 625;
      r_hotel = 750;
      building_cost = 100;
      mortgage = 70;
      unmortgage = 77;
    }

let electric : square = Utility { index = 12; name = "Electric Company" }

let states_Avenue : square =
  Property
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
      building_cost = 100;
      mortgage = 70;
      unmortgage = 77;
    }

let virginia_Avenue : square =
  Property
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
      building_cost = 100;
      mortgage = 80;
      unmortgage = 88;
    }

let second_Rail : square =
  Railroad { index = 15; name = "Pennsylvania Railroad" }

let james_Place : square =
  Property
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
      building_cost = 100;
      mortgage = 90;
      unmortgage = 99;
    }

let second_Chest : square = Community_Chest { index = 17 }

let tennessee_Avenue : square =
  Property
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
      building_cost = 100;
      mortgage = 90;
      unmortgage = 99;
    }

let ny_Avenue : square =
  Property
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
      building_cost = 100;
      mortgage = 100;
      unmortgage = 110;
    }

let free_parking : square = Free_Parking { index = 20 }

let kentucky_Avenue : square =
  Property
    {
      index = 21;
      name = "Kentucky Avenue";
      set = "red";
      cost = 220;
      rent = 18;
      rent_set = 36;
      r_1house = 90;
      r_2house = 250;
      r_3house = 700;
      r_4house = 875;
      r_hotel = 1050;
      building_cost = 150;
      mortgage = 110;
      unmortgage = 121;
    }

let second_Chance : square = Chance { index = 22 }

let indiana_Avenue : square =
  Property
    {
      index = 23;
      name = "Indiana Avenue";
      set = "red";
      cost = 220;
      rent = 18;
      rent_set = 36;
      r_1house = 90;
      r_2house = 250;
      r_3house = 700;
      r_4house = 875;
      r_hotel = 1050;
      building_cost = 150;
      mortgage = 110;
      unmortgage = 121;
    }

let illinois_Avenue : square =
  Property
    {
      index = 24;
      name = "Illinois Avenue";
      set = "red";
      cost = 240;
      rent = 20;
      rent_set = 40;
      r_1house = 100;
      r_2house = 300;
      r_3house = 750;
      r_4house = 925;
      r_hotel = 1100;
      building_cost = 150;
      mortgage = 120;
      unmortgage = 132;
    }

let third_Rail : square = Railroad { index = 25; name = "B&O Railroad" }

let atlantic_Avenue : square =
  Property
    {
      index = 26;
      name = "Atlantic Avenue";
      set = "yellow";
      cost = 260;
      rent = 22;
      rent_set = 44;
      r_1house = 110;
      r_2house = 330;
      r_3house = 800;
      r_4house = 975;
      r_hotel = 1150;
      building_cost = 150;
      mortgage = 130;
      unmortgage = 143;
    }

let ventnor_Avenue : square =
  Property
    {
      index = 27;
      name = "Ventnor Avenue";
      set = "yellow";
      cost = 240;
      rent = 22;
      rent_set = 44;
      r_1house = 110;
      r_2house = 330;
      r_3house = 800;
      r_4house = 975;
      r_hotel = 1150;
      building_cost = 150;
      mortgage = 130;
      unmortgage = 143;
    }

let water_works : square = Utility { index = 28; name = "Water Works" }

let marvin_Gardens : square =
  Property
    {
      index = 29;
      name = "Marvin Gardens";
      set = "yellow";
      cost = 280;
      rent = 24;
      rent_set = 48;
      r_1house = 120;
      r_2house = 360;
      r_3house = 850;
      r_4house = 1025;
      r_hotel = 1200;
      building_cost = 150;
      mortgage = 140;
      unmortgage = 154;
    }

let go_to_jail = Go_To_Jail { index = 30 }

let pacific_Avenue : square =
  Property
    {
      index = 31;
      name = "Pacific Avenue";
      set = "green";
      cost = 300;
      rent = 26;
      rent_set = 52;
      r_1house = 130;
      r_2house = 390;
      r_3house = 900;
      r_4house = 1100;
      r_hotel = 1275;
      building_cost = 200;
      mortgage = 150;
      unmortgage = 165;
    }

let nc_Avenue : square =
  Property
    {
      index = 32;
      name = "North Carolina Avenue";
      set = "green";
      cost = 300;
      rent = 26;
      rent_set = 52;
      r_1house = 130;
      r_2house = 390;
      r_3house = 900;
      r_4house = 1100;
      r_hotel = 1275;
      building_cost = 200;
      mortgage = 150;
      unmortgage = 165;
    }

let third_Chest : square = Community_Chest { index = 33 }

let pennsylvania_Avenue : square =
  Property
    {
      index = 34;
      name = "Pennsylvania Avenue";
      set = "green";
      cost = 320;
      rent = 28;
      rent_set = 56;
      r_1house = 150;
      r_2house = 450;
      r_3house = 1000;
      r_4house = 1200;
      r_hotel = 1400;
      building_cost = 200;
      mortgage = 160;
      unmortgage = 176;
    }

let fourth_Rail : square = Railroad { index = 35; name = "Short Line Railroad" }
let third_Chance : square = Chance { index = 36 }

let park_Place : square =
  Property
    {
      index = 37;
      name = "Park Place";
      set = "blue";
      cost = 350;
      rent = 35;
      rent_set = 70;
      r_1house = 175;
      r_2house = 500;
      r_3house = 1100;
      r_4house = 1300;
      r_hotel = 1500;
      building_cost = 200;
      mortgage = 175;
      unmortgage = 193;
    }

let second_Tax : square = Tax { index = 38 }

let boardwalk : square =
  Property
    {
      index = 39;
      name = "Boardwalk";
      set = "blue";
      cost = 400;
      rent = 50;
      rent_set = 100;
      r_1house = 200;
      r_2house = 600;
      r_3house = 1400;
      r_4house = 1700;
      r_hotel = 2000;
      building_cost = 200;
      mortgage = 200;
      unmortgage = 220;
    }

let new_board =
  [
    start_sqr;
    mediterranean_avenue;
    first_Chest;
    baltic_Avenue;
    first_Tax;
    first_Rail;
    oriental_Avenue;
    first_Chance;
    vermont_Avenue;
    connecticut_Avenue;
    jail_Sqr;
    charles_Place;
    electric;
    states_Avenue;
    virginia_Avenue;
    second_Rail;
    james_Place;
    second_Chest;
    tennessee_Avenue;
    ny_Avenue;
    kentucky_Avenue;
    second_Chance;
    indiana_Avenue;
    illinois_Avenue;
    third_Rail;
    atlantic_Avenue;
    ventnor_Avenue;
    water_works;
    marvin_Gardens;
    go_to_jail;
    pacific_Avenue;
    nc_Avenue;
    third_Chest;
    pennsylvania_Avenue;
    fourth_Rail;
    third_Chance;
    park_Place;
    second_Tax;
    boardwalk;
  ]

let clear_board b = new_board
