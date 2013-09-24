open OpenFlow0x01

module RoundTripping = struct
  TEST "OpenFlow Hello Test 1" = 
    let open Message in 
    let bs = Cstruct.create 101 in
    let m = Hello bs in 
    let x = 42l in 
    let s = marshal x m in  
    let h = Header.parse s in 
    let s' = String.sub s Header.size (Header.len h - Header.size) in 
    let x',m' = parse h s' in 
    let xid_ok = x = x' in 
    let msg_ok = 
      match m',m with 
	| Hello bs',Hello bs -> 
	  Cstruct.to_string bs = Cstruct.to_string bs'
	| _ -> 
	  false in 
    xid_ok && msg_ok 
end



