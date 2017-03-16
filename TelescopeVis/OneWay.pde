public class OneWay extends Solver {
  
  // The next slot to examine
  int slot;
  // Are we choosing an index, or completing it?
  boolean complete;
  // revert to old solution
  boolean restore;
  
  public OneWay(Instance instance,int nSlots) {
    super(instance,nSlots);
    complete=false;
    restore=false;
  }
  
  public String getName() { return "Local"; }
  
  public float step(int[] s) {
    arrayCopy(testSchedule,s);
    float r = calcScore(testSchedule,slot);
    step_();
    return r;
  }
  
  public void step_() {
    if(complete) {
      updateTargetValuation(slot,testSchedule);
      testSchedule[slot] = targetEvalsPerSlot[slot][0].target;
      ++slot;
      if(slot >= nSlots) {
        complete=false;
        if(calcScore(testSchedule,slot) < bestGain) {
          restore=true;
        } else {
          bestGain = calcScore(testSchedule,slot);
          arrayCopy(testSchedule,bestSchedule);
        }
      }
    } else if(restore) {
      arrayCopy(bestSchedule,testSchedule);
      restore=false;
    } else {  
      slot = int(random(0,nSlots-1));
      pickTarget();
      ++slot;
      complete=true;
    }
  }
  
  public float next(int[] s) {
    if(complete) {
      step_();
      return next(s);
    } else {
      arrayCopy(testSchedule,s);
      float r = calcScore(testSchedule,slot);
      step_();
      return r;
    }
  }

  private void pickTarget() {
    boolean success = false;
    updateTargetValuation(slot,testSchedule);
    TargetValuation tv = null;
    while(!success) {
      tv = targetEvalsPerSlot[slot][int(random(0,2))];//int(min(nSlots-1,random(0,instance.getNTargets()-1)))];
      success = (tv.target != testSchedule[slot]);
    }
    testSchedule[slot] = tv.target;
    for(int i=slot+1;i<nSlots;++i) {
      testSchedule[i] = -1;
    }
  }
}
