import "io"

manifest
{
  chunk_header = 0; 
  min_chunk_size = 5;
  // header settings header = 0b FS00 0000 0000 0000 0000 0000 0000 0000', F = free or used(1 or 0), S = (1 for split, 0 else)
  FREE_CHUNK = 0b10000000000000000000000000000000;
  USED_CHUNK = 0b00000000000000000000000000000000;
  SPLIT_CHUNK = 0b01000000000000000000000000000000
}

export { new_init, new_freevec, new_newvec, heap_print, chunk_print, chunk_header, SPLIT_CHUNK, 
           FREE_CHUNK, USED_CHUNK }

let heap = nil, size = 0;

// --------- Helper functions ----------------------------

let calc_size(level) be //calculates chunk size at level
{ resultis (size / (2 ** level)) }

let set_bit(i, w, x) be // set bit i of w to x
{ let sel = selector 1 : (31 - i);
  sel from !w := x }

let bit_cmp(i, w, x) be //compare bit i of word to value x
{ let sel = selector 1 : (31 - i);
  let bit = sel from w;
  resultis bit = x }

let is_free(chunk) be
{ resultis bit_cmp(0, chunk ! chunk_header, 1)}

let is_used(chunk) be
{ resultis bit_cmp(0, chunk ! chunk_header, 0)}

let is_split(chunk) be
{ resultis bit_cmp(1, chunk ! chunk_header, 1)}

let print_header(chunk) be
{ out("header: %032b\n", chunk ! chunk_header) }

let availability_print(chunk) be
{ let header = chunk ! chunk_header;
  for i = 2 to 31 do
  { let level_size = size / (2 ** (i-2));
    if bit_cmp(i, header, 1) do out("%d ", calc_size(i - 2))}
  out("\n") }
  
let chunk_print(chunk, level) be
{ let header = chunk ! chunk_header;
  test is_split(chunk) then
  { out("%d: available sizes: ", (chunk - heap)); // heap - chunk gives virtual address
    for i = 2 to 31 do
    { let level_size = size / (2 ** (i-2));
      if bit_cmp(i, header, 1) do out("%d ", calc_size(i - 2))}
    out("\n") }
  else
  { out("%d: %d words, ", chunk - heap, calc_size(level));
    test is_free(chunk) then
      out("FREE\n")
    else out("USED\n") } }
     
let heap_print_recursive(chunk, level) be
{ let i = 0;
  while i < level do
  { out(" |  "); 
    i +:= 1 }
  chunk_print(chunk, level);
  if is_split(chunk) do
  { let left = chunk + 1;
    let right = chunk + (calc_size(level)/2) + 1;
    heap_print_recursive(left, level+1);
    heap_print_recursive(right, level+1) }}

let heap_print() be
{ heap_print_recursive(heap, 0) }

let split(chunk, level) be // splits a chunk and returns pointer to the left child chunk
{ let  left = chunk + 1, right = chunk + (calc_size(level) / 2) + 1; // pointer to the left and right chunks
  // set control words of new chunks
  (left ! chunk_header) := FREE_CHUNK;
  out("    created new chunk at %d, size %d\n", left - heap, calc_size(level+1));
  (right ! chunk_header) := FREE_CHUNK;
  out("    created new chunk at %d, size %d\n", right - heap, calc_size(level+1));
  // update current control word
  chunk ! chunk_header := SPLIT_CHUNK;
  set_bit(3 + level, @(chunk ! chunk_header), 1); // set the available child bits
  resultis left }

let calc_min_size(data_size) be
{ let chunk_size;
  for i = 0 to 30 do
  { chunk_size := size / (2 ** i);
    if chunk_size - 1 < data_size \/ chunk_size < min_chunk_size do
      resultis size / (2 ** (i-1)) }}

let min_available(chunk, req_size) be // calculates index in availavility for a needed chunk size
{ let header = chunk ! chunk_header, min = -1;
  for i = 2 to 31 do
  { let level_size = calc_size(i-2);
    if bit_cmp(i, header, 1) /\ level_size >= req_size do min := i;
    if level_size = req_size do resultis min}
  resultis -1}

let update_availability(chunk, level) be
{ let left = chunk + 1, right = chunk + (calc_size(level)/2) + 1;
  let sel = selector 30 : 0; // all lesser levels 
  let left_avail = sel from (left ! chunk_header), right_avail = sel from (right ! chunk_header);
  out("retagging chunk %d, split with availability ", chunk-heap);
  // reset lesser availabilities
  sel from (chunk ! chunk_header) := 0;
  // update
  if is_free(left) \/ is_free(right) do
  { set_bit(3 + level, @(chunk ! chunk_header), 1)}
  if is_split(left) do
  { sel from (chunk ! chunk_header) := (sel from (chunk ! chunk_header)) bitor left_avail }
  if is_split(right) do
  { sel from (chunk ! chunk_header) := (sel from (chunk ! chunk_header)) bitor right_avail }
  availability_print(chunk);
}  

let get_chunk(chunk, req_size, level) be // returns pointer to chunk or -1 if not available
{ let header = chunk ! chunk_header, chunk_size = calc_size(level), r;
  out("at level %d, checking chunk starting at %d\n", level, chunk - heap);
  
  // check if current chunk is free 
  test is_free(chunk) /\ (chunk_size >= req_size) then
  {  let left;
     if (chunk_size / 2) < req_size do // if current chunk is the smallest possible chunk
     { out("  a free chunk of the right size\n");
       chunk ! chunk_header := USED_CHUNK; // set chunk to used
       resultis chunk } //return chunk 

     // chunk could be smaller so split
     out("  a free chunk that must be split\n");
     left := split(chunk, level);
     r := get_chunk(left, req_size, level + 1) } // recurse on the left_child chunk

  // chunk is split
  else test is_split(chunk) then
  { let idx = min_available(chunk, req_size); // index in header of the min available sub-chunk
    let left = chunk + 1;
    let right = chunk + (chunk_size/2) + 1;
    out("  a split chunk, availability: ");
    availability_print(chunk);
    
    test idx = -1 then // no availability
    { out("  Impossible - no availability\n");
      r := -1 } // failed
    else test bit_cmp(idx, left ! chunk_header, 1) then //check left
    { r := get_chunk(left, req_size, level+1)}
    else r := get_chunk(right, req_size, level+1) }
  
  // else chunk is used
  else 
  { out("  Imposible - this is a used chunk\n");
    r := -1 } // failed 

  //update header then return
  if r /= -1 do update_availability(chunk, level);

  resultis r }

let free(cur, target, level) be
{ let left = cur + 1, right = cur + (calc_size(level)/2) + 1;
  out("looking for chunk %d in chunk %d, at level %d\n", target-heap, cur-heap, level);
  test target = cur then // found the target
  { out("  found it, marking as free\n");
    cur ! chunk_header := FREE_CHUNK; 
    return }
  else test target < right then // target is in the left branch
  { free(left, target, level+1) }
  else // target is in the right branch
  { free(right, target, level+1) }
  // check if both childern are free
  test is_free(left) /\ is_free(right) do 
  { out("  recombining %d and %d to make %d free\n", left-heap, right-heap, cur-heap);
    cur ! chunk_header := FREE_CHUNK }
  else update_availability(cur, level) }

//----------- New Heap Functions ----------------

let new_init(v, s) be
{ out("init heap with %d words\n", s);
  heap := v;
  size := s;
  for i = 0 to s do
    heap ! i := nil;
  heap ! chunk_header := FREE_CHUNK //chunk is free
}

let new_newvec(data_size) be
{ let r, chunk_size = calc_min_size(data_size);
  out("for data size of %d, need a chunk of size %d\n", data_size, chunk_size);
  r :=  get_chunk(heap, chunk_size, 0); 
  if r = -1 do
  { out("allocation failed."); 
    resultis r} 
  resultis r+1 }

let new_freevec(v) be
{ out("requested deallocation of chunk %d\n", v-1 - heap);
  free(heap, v-1, 0);}

let pre_start() be
{ let v = vec(95);
  init := new_init;
  newvec := new_newvec;
  freevec := new_freevec;
  //init(!0x101, !0x100 - !0x101) 
  init(v, 95)}
