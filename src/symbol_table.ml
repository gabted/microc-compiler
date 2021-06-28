exception DuplicateEntry

module StringMap = Map.Make(String)

type 'a t = 'a StringMap.t list

let empty_table = [StringMap.empty]

let begin_block table = StringMap.empty :: table

let end_block = function
|[] -> failwith "Illegal Symbol_table state: More blocks ended then opened"
|m::ms -> ms

let add_entry symbol data = function
  |[] -> failwith "Illegal Symbol_table state: More blocks ended then opened"
  |m::ms -> if StringMap.mem symbol m then
              raise DuplicateEntry
            else
              (StringMap.add symbol data m)::ms

let rec lookup symbol = function
|[] -> None
|m::ms ->  let d = StringMap.find_opt symbol m in
            if Option.is_some d then d
            else lookup symbol ms