/**
 * Class to hold a pair of observation target and value relative to
 *  current position in schedule
 */
public final class TargetValuation {
  public final int target;
  public double value;
  
  public TargetValuation(int t) { target = t; }

  public int cmp(TargetValuation other) {
    if (value < other.value) {
      return 1;
    }
    if (value > other.value) {
      return -1;
    }
    return 0;
  }

}

