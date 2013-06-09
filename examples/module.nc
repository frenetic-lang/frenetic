include "submodule.nc"

let dropIcmp = if frameType = 0x800 && ipProtocol = 1 then drop else pass
