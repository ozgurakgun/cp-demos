public class Instance {
  private int nTargets;
  private int[] exposure;
  private int[] cadence;
  private float[] gain;
  
  public Instance(int n,int[] exp,int[] cad,float[] g) {
    nTargets = n;
    exposure = exp;
    cadence  = cad;
    gain     = g;
  }
  
  public int getExposure(int t) {
    try {
      return exposure[t];
    } catch (Exception e) {
      e.printStackTrace();
      System.exit(0);
    }
    return 0;
  }
  public int getCadence(int t) {
    return cadence[t];
  }
  public float getGain(int t) {
    return gain[t];
  }
  public int getNTargets() {
    return nTargets;
  }

}
