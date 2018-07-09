original_scheduler = """
def scheduler() state phase(0), cur_node(0){ // Phase 0: Execute RunSw, Phase 1: Exectue FwdQ
  for p in [0..2){
    if phase == 0{
      for i in [0..k){
        if (Q_in@cur_node).size() > 0{
          return (RunSw,cur_node);
        }
        cur_node = (cur_node + 1) % k;
      }
      phase = 1;
      cur_node = 0;
    }
    if phase == 1{
      for i in [0..k){
        if (Q_out@cur_node).size() > 0{
          return (FwdQ,cur_node);
        }
        cur_node = (cur_node + 1) % k;
      }
      phase = 0;
      cur_node = 0;
    }
  }
  assert(0);
}
"""
