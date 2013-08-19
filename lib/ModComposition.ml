type t = {
	dlSrc : bool;
	dlDst : bool;
	dlVlan : bool;
	dlVlanPcp : bool;
	nwSrc : bool;
	nwDst : bool;
	tpSrc : bool;
	tpDst : bool
}

let none = {
	dlSrc = false;
	dlDst = false;
	dlVlan = false;
	dlVlanPcp = false;
	nwSrc = false;
	nwDst = false;
	tpSrc = false;
	tpDst = false
}

let dlSrc = { none with dlSrc = true }

let dlDst = { none with dlDst = true }

let dlVlan = { none with dlVlan = true }

let dlVlanPcp = { none with dlVlanPcp = true }

let nwSrc = { none with nwSrc = true }

let nwDst = { none with nwDst = true }

let tpSrc = { none with tpSrc = true }

let tpDst = { none with tpDst = true }

let seq (m1 : t) (m2 : t) = {
	dlSrc = m1.dlSrc || m2.dlSrc;
	dlDst = m2.dlDst || m2.dlDst;
	dlVlan = m1.dlVlan || m2.dlVlan;
	dlVlanPcp = m1.dlVlanPcp || m2.dlVlanPcp;
	nwSrc = m1.nwSrc || m2.nwSrc;
	nwDst = m1.nwDst || m2.nwDst;
	tpSrc = m1.tpSrc || m2.tpSrc;
	tpDst = m1.tpDst || m2.tpDst
}

let override (x : bool) (y : bool) : bool =
	match (x, y) with
	  | (true, false) -> raise (Invalid_argument "unrealizable modification")
	  | (true, true) -> true
	  | (false, _) -> y

let par (m1 : t) (m2 : t) = {
	dlSrc = override m1.dlSrc m2.dlSrc;
	dlDst = override m2.dlDst m2.dlDst;
	dlVlan = override m1.dlVlan m2.dlVlan;
	dlVlanPcp = override m1.dlVlanPcp m2.dlVlanPcp;
	nwSrc = override m1.nwSrc m2.nwSrc;
	nwDst = override m1.nwDst m2.nwDst;
	tpSrc = override m1.tpSrc m2.tpSrc;
	tpDst = override m1.tpDst m2.tpDst
	
}