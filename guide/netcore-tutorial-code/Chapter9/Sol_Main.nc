include "../Chapter8/Sol_Firewall.nc"
include "../Chapter8/Sol_Routing.nc"
include "Monitoring.nc"

monitorTable(2, (firewall; routing) + monitoring)