exception DuplicateEntry

type 'a t

val empty_table : 'a t

val begin_block : 'a t -> 'a t

val end_block : 'a t -> 'a t

val add_entry : Ast.identifier -> 'a -> 'a t -> 'a t

val global_lookup : Ast.identifier -> 'a t -> 'a option

val local_lookup : Ast.identifier -> 'a t -> 'a option