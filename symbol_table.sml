signature SYMTABLE =
 sig

  val table : (string,Rational.rational)  HashTable.hash_table ;
  val add_entry : string * Rational.rational -> unit ;
  val lookup_entry : string -> Rational.rational
end ;

structure SymTable : SYMTABLE  =
struct
  open Rational ;
  open HashTable ;
  (* raised when I do a lookup and can't find something *)
  exception lookup_error

  val hash_fn : string->word = HashString.hashString

  fun cmp_fn(x : string ,y : string) = (x = y)


  val init_sz : int = 101

  val table: (string,rational) hash_table = mkTable (hash_fn, cmp_fn)(init_sz,
  lookup_error)
  fun add_entry(key, value) = insert table (key,value) ;
  fun lookup_entry(key) = lookup table key ;

end ;