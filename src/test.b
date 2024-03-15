import "io"
import "heap"

/* 
  heap's exports are:

  export { heap_printfrees, heap_free_or_used, heap_size, heap_next, heap_prev, 
           heap_free, heap_used, heap_words_remaining, heap_words_allocated }
*/

let start() be
{ let ptrs = vec(1534), skip = false, v = vec(95);
  init := new_init;
  newvec := new_newvec;
  freevec := new_freevec;
  init(v, 1534);

  for i = 0 to 127 do
    ptrs ! i := 0;
  out("x to exit.\n");
  out("# blah blah blah for an unobtrusive comment.\n");
  out("?N to examine any chunk. N is its real address, which is the value returned by newvec minus two.\n");
  out("vf to freevec the variable named v. Variable names are single lower-case letters.\n");
  out("v? to see the value of variable v.\n");
  out("vnN to newvec(N), N is number of words wanted, v is the variable in which the returned value will be stored,\n");
  out("                  the response is v = A, where A is the pointer returned by newvec.\n\n");
  heap_print();
  while true do
  { let v, c, n, p, realp;
    test skip then
      outs("> ")
    else
    { outs("\n> ") }
    skip := false;

    v := inch();
    if v = '#' then
    { c := inch() repeatuntil c = '\n';
      skip := true;
      loop }

    test v = '?' then
    { realp := inno();
      //chunk_print(realp);
      loop }

    else if v = 'x' then
      finish;

    c := inch();
    test c = '?' then
    { inch();
      realp := ptrs ! v - 2;
      if realp = -2 then
      { outs("unassigned\n");
        loop }
      //chunk_print(realp);
      loop }

    else test c = 'n' then
    { n := inno();
      p := newvec(n);
      ptrs ! v := p;
      out("%c = %d\n", v, p);
      realp := p - 1 //;
      //chunk_print(realp) 
     }

    else test c = 'f' then
    { c := inch();
      p := ptrs ! v;
      if p = 0 then
      { outs("%c not yet defined\n", v);
        loop }
      realp := p - 1;
      if ((realp ! chunk_header) bitand USED_CHUNK) <> USED_CHUNK then
      { out("That is not a used chunk\n");
        loop }
      freevec(p) }

    else
    { outs("???\n");
      if c <> '\n' then
      { c := inch() repeatuntil c = '\n' }
      loop }

    heap_print() } }

