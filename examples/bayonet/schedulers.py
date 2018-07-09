two_phase = """
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

uniform = """
def scheduler(){
  actions := ([]: (R x R)[]);
  for i in [0..k){
    if (Q_in@i).size() > 0 { actions ~= [(RunSw,i)]; }
    if (Q_out@i).size() > 0 { actions ~= [(FwdQ,i)]; }
  }
  return actions[uniformInt(0,actions.length-1)];
}
"""
