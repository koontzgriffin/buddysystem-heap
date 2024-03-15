import "io"

let size = 95;

let bit_cmp(i, w, x) be //compare bit i of word to value x
{ let sel = selector 1 : (31 - i);
  let bit = sel from w;
  resultis bit = x }

let set_bit(i, w, x) be // set bit i of w to x
{ let sel = selector 1 : (31 - i);
  sel from !w := x }

let min_available(chunk, req_size) be // calculates index in availavility for a needed chunk size
{ let header = chunk;
  for i = 2 to 31 do
  { let level_size = size / (2 ** (i-2));
    out("level size: %d\n", level_size);
    out("bit_cmp(i, header, 1) = %d, (level_size = req_size) = %d, bit_cmp(i + 1, header, 0) = %d\n", bit_cmp(i, header, 1), level_size = req_size, bit_cmp(i + 1, header, 0));
    if bit_cmp(i, header, 1) /\ (level_size = req_size \/ (bit_cmp(i + 1, header, 0))) do
      resultis i }
  resultis -1}

let start() be
{ let chunk = 0b11000000000000000000000000000000, req_size = 23;
  out("chunk : %b\n", chunk);
  set_bit(3, @chunk, 1);
  out("result is: %b\n", chunk)}
