
public abstract class Solver {
  protected Instance instance;
  protected int[] testSchedule;
  protected int[] bestSchedule;
  protected float testGain;
  protected float bestGain;
  protected TargetValuation[][] targetEvalsPerSlot;
  protected int nSlots;

  public Solver(Instance instance,int n) {
    nSlots = n;
    this.instance = instance;
    testSchedule = new int[nSlots];
    bestSchedule = new int[nSlots];
    targetEvalsPerSlot = new TargetValuation[nSlots][instance.getNTargets()];
    for(int s = 0; s < nSlots; ++s)
      for(int t = 0; t < instance.getNTargets(); ++t) {
        targetEvalsPerSlot[s][t] = new TargetValuation(t);
      }
      
    bestGain=0;
    updateTargetValuation(0,testSchedule);
    greedyCompleteSchedule(testSchedule,0,nSlots-1);
    testGain = calcScore(testSchedule,nSlots);
    //updateBest();
  }
  
  // Get the next (partial) schedule to be tested
  abstract float step(int[] s);
  // Get the next (complete) schedule to be tested
  abstract float next(int[] s);
  abstract String getName();
  
  //public float getBest() { return bestGain; }
  //public float getCurrent() { return testGain; }
  
  // evaluate the quality increased by observing target at idx in schedule
  protected double evaluateTarget(int target, int idx, int[] schedule) {
    double targetGain = instance.getGain(target);
    // for the first slot, then the gain is always the full gain of this target
    if (idx == 0) {
      return targetGain;
    }
    // Otherwise iterate backwards looking for last occurrence of target
    int lastIndex = idx-1;
    while (lastIndex >= 0) {
      if (schedule[lastIndex] == target) break;
      lastIndex--;
    }
    // If never encountered before then gain is the full gain of this target
    if (lastIndex < 0) {
      return targetGain ;   
    }
    // Otherwise calculate the time elapsed since last observation.
    int timeElapsed = 0 ;
    int targetCadence = instance.getCadence(target) ;
    for (int i = lastIndex; i < idx; i++) {
      //if(schedule[i] < 0){
      //  if(schedule[i]-timeElapsed < -2*targetCadence)
      //    return targetGain;
      //  timeElapsed -= schedule[i];
      //}
      //else
        timeElapsed += instance.getExposure(schedule[i]) ;
    }
    double result = 
      targetGain * 
      (targetCadence - abs(timeElapsed - targetCadence)) / targetCadence ;
    return result;
  }
  
  /**
   * Having made a local change at the specified slot, greedily complete
   *  the testSchedule to see if the change is an improvement
   */
  protected void greedyCompleteSchedule(int[] schedule,int from,int to) {
    //double lb = 0;

    //for(int i=0;i<from;++i) {
    //  int t = schedule[i];
    //  lb += evaluateTarget(t,i,schedule);
    //}
    // create the rest of the solution
    // Loop over the schedule slots, pick the best target each time.
    for (int i=from; i<to; i++) {        
      double bestValue = 0;
      int bestTarget = 0;
      for (int t = 0; t < instance.getNTargets(); t++) {
        double evalResult = evaluateTarget(t, i, schedule);
        if (evalResult > bestValue) {
          bestValue = evalResult;
          bestTarget = t;
        }
      }
      schedule[i] = bestTarget;
      //lb += bestValue;
    }    
    //return lb;
  }
  
  /**
   * Update the list of targets for the next slot.
   */
  protected void updateTargetValuation(int slot,int[] schedule) {
    // Obtain a valuation for each target at this point in the schedule
    for (int i = 0; i < instance.getNTargets(); ++i) {
      TargetValuation tv = targetEvalsPerSlot[slot][i];
      tv.value = evaluateTarget(tv.target, slot, schedule);
    }
    // Sort descending
    sortArr(targetEvalsPerSlot[slot]); 
  }
  
  /**
   * Save the schedule if a better one has been found.
   */
  //protected void updateBest() {
  //  if(testGain > bestGain) {
  //    arrayCopy(testSchedule,bestSchedule);
  //    bestGain=testGain;
  //  }
  //}
  
  protected float calcScore(int[] s,int hi) {
    float x = 0.;
    for(int i=0;i<hi;++i) {
      x += evaluateTarget(s[i],i,s);
    }
    return x;
  }
  
  public void sortArr(TargetValuation[] d) {
    sortArr(d,0,d.length-1);
  }
  private void sortArr(TargetValuation[] d,int lo,int hi) {
    int i = lo;
    int j = hi;
    TargetValuation pivot = d[i+int((j-i)/2)];
    while(i <= j) {
      while(d[i].cmp(pivot) < 0 ) {
        ++i;
      }
      while(d[j].cmp(pivot) > 0 ) {
        --j;
      }
      if(i <= j) {
        TargetValuation tmp = d[i];
        d[i] = d[j];
        d[j] = tmp;
        ++i;--j;
      }
    }
    if(lo < j) {
      sortArr(d,lo,j);
    }
    if(i < hi) {
      sortArr(d,i,hi);
    }
  }
}
  
