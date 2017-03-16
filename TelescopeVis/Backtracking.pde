
public class Backtracking extends Solver {
  
  // The next slot to examine
  int slot;
  // The next target (sorted) to examine
  int[] target;
  
  public Backtracking(Instance instance,int nSlots) {
    super(instance,nSlots);
    slot = nSlots;
    target = new int[nSlots];
    for(int i=0;i<nSlots;++i) {
      target[i] = 1;
    }
  }
  
  public String getName() { return "Backtracking"; }
  
  public float next(int[] s) {
    //System.out.println(slot + " " + target[nSlots-1]);
    if(slot < 0) {
      arrayCopy(testSchedule,s);
      return 0;
    } if(slot < nSlots) {
      step_();
      return next(s);
    } else {
      arrayCopy(testSchedule,s);
      --slot;
      return calcScore(testSchedule,nSlots);
    }
  }
  
  public float step(int[] s) {
    arrayCopy(testSchedule,s);
    float r = calcScore(testSchedule,slot);
    step_();
    return r;
  }
  
  private void step_() {
    //finished, return an empty schedule
    if(slot < 0) {
      testGain = 0;
    }
    //hit the end, update best and backtrack
    else if(slot >= nSlots) {
      // move back
      --slot;
      // continue
      step_();
    }
    // out of targets here, backtrack
    else if(target[slot] >= instance.getNTargets()) {
      // undo this observation
      target[slot] = 0;
      testSchedule[slot] = -1;
      // step back
      --slot;
      // continue
      step_();
    } else {
      // everything is okay!
      // Pick the next target
      testSchedule[slot] = targetEvalsPerSlot[slot][target[slot]].target;
      ++target[slot];
      ++slot;
      // update the evaluations
      if (slot < nSlots) {
        updateTargetValuation(slot,testSchedule);
      }
      testGain = calcScore(testSchedule,slot);
    }
  }
}
