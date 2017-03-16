public class TwoWay extends Solver {
  
  // The next slot to examine
  int slot;
  // Are we choosing an index, or completing the schedule?
  boolean complete;
  boolean restore;
  // Is the test schedule flipped?
  boolean flipped;
  int[] container;
  
  public TwoWay(Instance instance,int nSlots) {
    super(instance,nSlots);
    container=new int[nSlots];
    complete=false;
    flipped=false;
  }

  public String getName() { return "Bi-directional Local"; }
  
  public float step(int[] s) {
    arrayCopy(returnSchedule(),s);
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
          arrayCopy(returnSchedule(),bestSchedule);
        }
        
        flipped=!flipped;
        arrayCopy(testSchedule,container);
        for(int i=0;i<nSlots;++i) {
          testSchedule[i] = container[nSlots-i-1];
        }
      }
      //return testSchedule;
    } else if (restore) {
      restore=false;
      if(flipped) {
        for(int i=0;i<nSlots;++i) {
          testSchedule[i] = bestSchedule[nSlots-i-1];
        }
      } else {
        arrayCopy(bestSchedule,testSchedule);
      }
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
      arrayCopy(returnSchedule(),s);
      float r = calcScore(testSchedule,slot);
      step_();
      return r;
    }
  }
  
  private int[] returnSchedule() {
    if(flipped) {
      for(int i=0;i<nSlots;++i) {
        container[i] = testSchedule[nSlots-i-1];
      }
    }
    else 
      arrayCopy(testSchedule,container);
    return container;
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
