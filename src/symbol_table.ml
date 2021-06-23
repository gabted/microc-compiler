exception DuplicateEntry

module MyMap = Map.Make(String)

type 'a t = 'a MyMap.t list

let empty_table = [MyMap.empty]

let begin_block table = MyMap.empty :: table

let end_block = function
|[] -> failwith "Illegal Symbol_table state: More blocks ended then opened"
|m::ms -> ms

let add_entry symbol data = function
  |[] -> failwith "Illegal Symbol_table state: More blocks ended then opened"
  |m::ms -> (MyMap.add symbol data m)::ms

let rec global_lookup symbol = function
|[] -> None
|m::ms ->  let d = MyMap.find_opt symbol m in
            if Option.is_some d then d
            else global_lookup symbol ms

let local_lookup symbol = function
|[]    -> failwith "Illegal Symbol_table state: More blocks ended then opened"
|m::ms -> MyMap.find_opt symbol m