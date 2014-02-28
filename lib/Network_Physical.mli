open Network
module type NODE = sig
  include VERTEX
  type device = Switch | Host | Middlebox

  val default : t
end

module type LINK = sig
  include EDGE
end


module Node : NODE
module Link : LINK

module Net : NETWORK
