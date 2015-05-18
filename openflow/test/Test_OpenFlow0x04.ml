(*
  TEST "OpenFlow0x04 PortDesc.PortConfig RoundTrip" =
      let module GenPortConfig = Gen0x04.PortDesc.PortConfig in
      (openflow_quickCheck GenPortConfig.arbitrary
          GenPortConfig.to_string GenPortConfig.parse GenPortConfig.marshal)

  TEST "OpenFlow0x04 PortDesc.PortState RoundTrip" =
      let module GenPortState = Gen0x04.PortDesc.PortState in
      (openflow_quickCheck GenPortState.arbitrary
          GenPortState.to_string GenPortState.parse GenPortState.marshal)

  TEST "OpenFlow0x04 PortDesc.PortFeatures RoundTrip" =
      let module GenPortState = Gen0x04.PortDesc.PortState in
      (openflow_quickCheck GenPortState.arbitrary
          GenPortState.to_string GenPortState.parse GenPortState.marshal)

  TEST "OpenFlow0x04 PortDesc RoundTrip" =
      let module PortDesc = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.PortDesc) in
      (openflow_quickCheck PortDesc.arbitrary
          PortDesc.to_string PortDesc.parse PortDesc.marshal)

  TEST "OpenFlow0x04 PortStatus RoundTrip" =
      let module GenPortStatus = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.PortStatus) in
      (openflow_quickCheck GenPortStatus.arbitrary
          GenPortStatus.to_string GenPortStatus.parse GenPortStatus.marshal)

  TEST "OpenFlow0x04 PseudoPort RoundTrip" =
      let module GenPseudoPort = Gen0x04.PseudoPort in
      (openflow_quickCheck GenPseudoPort.arbitrary
          GenPseudoPort.to_string GenPseudoPort.parse GenPseudoPort.marshal)

  TEST "OpenFlow0x04 OfpMatch RoundTrip" =
      let module GenOfpMatch = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.OfpMatch) in
      (openflow_quickCheck GenOfpMatch.arbitrary
          GenOfpMatch.to_string GenOfpMatch.parse GenOfpMatch.marshal)

  TEST "OpenFlow0x04 OfpMatch.Oxm RoundTrip" =
      let module GenOxm = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.OfpMatch.Oxm) in
      (openflow_quickCheck GenOxm.arbitrary
          GenOxm.to_string GenOxm.parse GenOxm.marshal)
  
  TEST "OpenFlow0x04 OfpMatch.OxmHeader RoundTrip" =
      let module GenOxm = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.OfpMatch.OxmHeader) in
      (openflow_quickCheck GenOxm.arbitrary
          GenOxm.to_string GenOxm.parse GenOxm.marshal)

  TEST "OpenFlow0x04 Action RoundTrip" =
      let module GenAction = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.Action) in
      (openflow_quickCheck GenAction.arbitrary
          GenAction.to_string GenAction.parse GenAction.marshal)

  TEST "OpenFlow0x04 Instructions.Instruction RoundTrip" =
      let module GenInstruction = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.Instructions.Instruction) in
      (openflow_quickCheck GenInstruction.arbitrary
          GenInstruction.to_string GenInstruction.parse GenInstruction.marshal)

  TEST "OpenFlow0x04 Instructions RoundTrip" =
      let module GenInstructions = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.Instructions) in
      (openflow_quickCheck GenInstructions.arbitrary
          GenInstructions.to_string GenInstructions.parse GenInstructions.marshal)

  TEST "OpenFlow0x04 PortMod RoundTrip" =
      let module GenPortMod = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.PortMod) in
      (openflow_quickCheck GenPortMod.arbitrary
          GenPortMod.to_string GenPortMod.parse GenPortMod.marshal)

  TEST "OpenFlow0x04 MeterMod RoundTrip" =
      let module GenMeterMod = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.MeterMod) in
      (openflow_quickCheck GenMeterMod.arbitrary
          GenMeterMod.to_string GenMeterMod.parse GenMeterMod.marshal)

  TEST "OpenFlow0x04 FlowMod RoundTrip" =
      let module GenFlowMod = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.FlowMod) in
      (openflow_quickCheck GenFlowMod.arbitrary
          GenFlowMod.to_string GenFlowMod.parse GenFlowMod.marshal)
  TEST "OpenFlow0x04 FlowMod.FlowModCommand RoundTrip" =
      let module GenFlowModCommand = Gen0x04.FlowMod.FlowModCommand in
      (openflow_quickCheck GenFlowModCommand.arbitrary
          GenFlowModCommand.to_string GenFlowModCommand.parse GenFlowModCommand.marshal)

  TEST "OpenFlow0x04 Bucket RoundTrip" =
      let module GenBucket = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.Bucket) in
      (openflow_quickCheck GenBucket.arbitrary
          GenBucket.to_string GenBucket.parse GenBucket.marshal)

  TEST "OpenFlow0x04 GroupMod RoundTrip" =
      let module GenGroupMod = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.GroupMod) in
      (openflow_quickCheck GenGroupMod.arbitrary
          GenGroupMod.to_string GenGroupMod.parse GenGroupMod.marshal)

  TEST "OpenFlow0x04 MultipartReq.TableFeature.TableFeatureProp RoundTrip" =
      let module GenTableFeatureProp = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.MultipartReq.TableFeature.TableFeatureProp) in
      (openflow_quickCheck GenTableFeatureProp.arbitrary
          GenTableFeatureProp.to_string GenTableFeatureProp.parse GenTableFeatureProp.marshal)

  TEST "OpenFlow0x04 MultipartReq.TableFeature RoundTrip" =
      let module GenTableFeature = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.MultipartReq.TableFeature) in
      (openflow_quickCheck GenTableFeature.arbitrary
          GenTableFeature.to_string GenTableFeature.parse GenTableFeature.marshal)

  TEST "OpenFlow0x04 MultipartReq.FlowRequest RoundTrip" =
      let module GenTableFlowReq = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.MultipartReq.FlowRequest) in
      (openflow_quickCheck GenTableFlowReq.arbitrary
          GenTableFlowReq.to_string GenTableFlowReq.parse GenTableFlowReq.marshal)

  TEST "OpenFlow0x04 MultipartReq.QueueRequest RoundTrip" =
      let module GenTableQueueReq = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.MultipartReq.QueueRequest) in
      (openflow_quickCheck GenTableQueueReq.arbitrary
          GenTableQueueReq.to_string GenTableQueueReq.parse GenTableQueueReq.marshal)

  TEST "OpenFlow0x04 MultipartReq RoundTrip" =
      let module GenMultipartReq = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.MultipartReq) in
      (openflow_quickCheck GenMultipartReq.arbitrary
          GenMultipartReq.to_string GenMultipartReq.parse GenMultipartReq.marshal)

  TEST "OpenFlow0x04 MultipartReply.Flow RoundTrip" =
      let module GenFlowReply = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.MultipartReply.FlowStats) in
      (openflow_quickCheck GenFlowReply.arbitrary
          GenFlowReply.to_string GenFlowReply.parse GenFlowReply.marshal)

  TEST "OpenFlow0x04 MultipartReply.Aggregate RoundTrip" =
      let module GenAggregReply = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.MultipartReply.AggregateStats) in
      (openflow_quickCheck GenAggregReply.arbitrary
          GenAggregReply.to_string GenAggregReply.parse GenAggregReply.marshal)

  TEST "OpenFlow0x04 MultipartReply.Table RoundTrip" =
      let module GenTableReply = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.MultipartReply.TableStats) in
      (openflow_quickCheck GenTableReply.arbitrary
          GenTableReply.to_string GenTableReply.parse GenTableReply.marshal)

  TEST "OpenFlow0x04 MultipartReply.PortStats RoundTrip" =
      let module GenPortStatsReply = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.MultipartReply.PortStats) in
      (openflow_quickCheck GenPortStatsReply.arbitrary
          GenPortStatsReply.to_string GenPortStatsReply.parse GenPortStatsReply.marshal)

  TEST "OpenFlow0x04 MultipartReply.SwitchDescriptionReply RoundTrip" =
      let module GenSwDescReply = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.MultipartReply.SwitchDescriptionReply) in
      (openflow_quickCheck GenSwDescReply.arbitrary
          GenSwDescReply.to_string GenSwDescReply.parse GenSwDescReply.marshal)

  TEST "OpenFlow0x04 MultipartReply.GroupFeatures RoundTrip" =
      let module GenGroupFeaturesReply = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.MultipartReply.GroupFeatures) in
      (openflow_quickCheck GenGroupFeaturesReply.arbitrary
          GenGroupFeaturesReply.to_string GenGroupFeaturesReply.parse GenGroupFeaturesReply.marshal)
  
  TEST "OpenFlow0x04 MultipartReply.MeterStats RoundTrip" =
      let module GenMeterStatsReply = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.MultipartReply.MeterStats) in
      (openflow_quickCheck GenMeterStatsReply.arbitrary
          GenMeterStatsReply.to_string GenMeterStatsReply.parse GenMeterStatsReply.marshal)

  TEST "OpenFlow0x04 MultipartReply.MeterConfig RoundTrip" =
      let module GenMeterConfigReply = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.MultipartReply.MeterConfig) in
      (openflow_quickCheck GenMeterConfigReply.arbitrary
          GenMeterConfigReply.to_string GenMeterConfigReply.parse GenMeterConfigReply.marshal)

  TEST "OpenFlow0x04 MultipartReply.MeterFeatures RoundTrip" =
      let module GenMeterFeaturesReply = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.MultipartReply.MeterFeatures) in
      (openflow_quickCheck GenMeterFeaturesReply.arbitrary
          GenMeterFeaturesReply.to_string GenMeterFeaturesReply.parse GenMeterFeaturesReply.marshal)

  TEST "OpenFlow0x04 MultipartReply.GroupDesc RoundTrip" =
      let module GenGroupDescReply = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.MultipartReply.GroupDesc) in
      (openflow_quickCheck GenGroupDescReply.arbitrary
          GenGroupDescReply.to_string GenGroupDescReply.parse GenGroupDescReply.marshal)

  TEST "OpenFlow0x04 MultipartReply.GroupStats RoundTrip" =
      let module GenGroupStatsReply = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.MultipartReply.GroupStats) in
      (openflow_quickCheck GenGroupStatsReply.arbitrary
          GenGroupStatsReply.to_string GenGroupStatsReply.parse GenGroupStatsReply.marshal)

  TEST "OpenFlow0x04 MultipartReply RoundTrip" =
      let module GenMultipartReply = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.MultipartReply) in
      (openflow_quickCheck GenMultipartReply.arbitrary
          GenMultipartReply.to_string GenMultipartReply.parse GenMultipartReply.marshal)

  TEST "OpenFlow0x04 QueueDesc.QueueProp RoundTrip" =
      let module GenQueueProp = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.QueueDesc.QueueProp) in
      (openflow_quickCheck GenQueueProp.arbitrary
          GenQueueProp.to_string GenQueueProp.parse GenQueueProp.marshal)

  TEST "OpenFlow0x04 QueueDesc RoundTrip" =
      let module GenQueueDesc = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.QueueDesc) in
      (openflow_quickCheck GenQueueDesc.arbitrary
          GenQueueDesc.to_string GenQueueDesc.parse GenQueueDesc.marshal)

  TEST "OpenFlow0x04 QueueConfReq RoundTrip" =
      let module GenQueueConfReq = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.QueueConfReq) in
      (openflow_quickCheck GenQueueConfReq.arbitrary
          GenQueueConfReq.to_string GenQueueConfReq.parse GenQueueConfReq.marshal)

  TEST "OpenFlow0x04 QueueConfReply RoundTrip" =
      let module GenQueueConfReply = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.QueueConfReply) in
      (openflow_quickCheck GenQueueConfReply.arbitrary
          GenQueueConfReply.to_string GenQueueConfReply.parse GenQueueConfReply.marshal)

  TEST "OpenFlow0x04 PacketOut RoundTrip" =
      let module GenPacketOut = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.PacketOut) in
      (openflow_quickCheck GenPacketOut.arbitrary
          GenPacketOut.to_string GenPacketOut.parse GenPacketOut.marshal)

  TEST "OpenFlow0x04 PacketIn RoundTrip" =
      let module GenPacketIn = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.PacketIn) in
      (openflow_quickCheck GenPacketIn.arbitrary
          GenPacketIn.to_string GenPacketIn.parse GenPacketIn.marshal)

  TEST "OpenFlow0x04 RoleRequest RoundTrip" =
      let module GenRoleReq = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.RoleRequest) in
      (openflow_quickCheck GenRoleReq.arbitrary
          GenRoleReq.to_string GenRoleReq.parse GenRoleReq.marshal)

  TEST "OpenFlow0x04 SwitchConfig RoundTrip" =
      let module GenSwitchConfig = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.SwitchConfig) in
      (openflow_quickCheck GenSwitchConfig.arbitrary
          GenSwitchConfig.to_string GenSwitchConfig.parse GenSwitchConfig.marshal)

  TEST "OpenFlow0x04 TableMod RoundTrip" =
      let module GenTableMod = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.TableMod) in
      (openflow_quickCheck GenTableMod.arbitrary
          GenTableMod.to_string GenTableMod.parse GenTableMod.marshal)

  TEST "OpenFlow0x04 Hello.Element.VersionBitMap RoundTrip" =
      let module GenVersionBitmap = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.Hello.Element.VersionBitMap) in
      (openflow_quickCheck GenVersionBitmap.arbitrary
          GenVersionBitmap.to_string GenVersionBitmap.parse GenVersionBitmap.marshal)

  TEST "OpenFlow0x04 Hello.Element RoundTrip" =
      let module GenElement = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.Hello.Element) in
      (openflow_quickCheck GenElement.arbitrary
          GenElement.to_string GenElement.parse GenElement.marshal)

  TEST "OpenFlow0x04 Hello RoundTrip" =
      let module GenHello = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.Hello) in
      (openflow_quickCheck GenHello.arbitrary
          GenHello.to_string GenHello.parse GenHello.marshal)

  TEST "OpenFlow0x04 FlowRemoved RoundTrip" =
      let module GenFlowRemoved = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.FlowRemoved) in
      (openflow_quickCheck GenFlowRemoved.arbitrary
          GenFlowRemoved.to_string GenFlowRemoved.parse GenFlowRemoved.marshal)

  TEST "OpenFlow0x04 AsyncConfig RoundTrip" =
      let module GenAsyncConfig = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.AsyncConfig) in
      (openflow_quickCheck GenAsyncConfig.arbitrary
          GenAsyncConfig.to_string GenAsyncConfig.parse GenAsyncConfig.marshal)

  TEST "OpenFlow0x04 Error RoundTrip" =
      let module GenError = Gen0x04.OpenFlow0x04_Unsize(Gen0x04.Error) in
      (openflow_quickCheck GenError.arbitrary
          GenError.to_string GenError.parse GenError.marshal)
*)