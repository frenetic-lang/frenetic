#!/bin/sh
echo "** Unverified serialization and low-level socket libraries **"
wc -l PacketParser.ml Platform.ml OpenFlow0x01Parser.ml
echo "** Unverified glue code *"
wc -l VerifiedNetCore.ml
echo "** Unverified surface syntax for NetCore **"
wc -l NetCoreSyntax.ml
echo "NOTE: NetCoresyntax.ml supports full NetCore, whereas our PLDI work"
echo "only supports a fragment. So, the LOC is a bit inflated."
