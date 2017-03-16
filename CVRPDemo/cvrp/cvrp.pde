String instance = "a_n37_k5";
String instanceData = instance + "/instance";
String toursDir = instance + "/snapshots";

float xLocs[] = { };
float yLocs[] = { };
float cityDemand[] = { };
float capacity;

int prevTours[][] = { };
int tours[][] = { };

float prevLoads[][] = { };
float loads[][] = { }; 

float lineWeight[][] = {};
float prevLineWeight[][] = {};

float totalDistance=0.;
float prevTotalDistance=0.;

int cityOnTour[]={};

// The color of all edges during transitions
color edgeColour[][][] = {};

ArrayList<Integer> edgesToDrawFrom = new ArrayList<Integer>();
ArrayList<Integer> edgesToDrawTo = new ArrayList<Integer>();

int step;
int transitionTime = 20;
boolean pause = true;

int nextSol = 0;
int nSnapShots;

int margins = 70;
int citySizeMin = 5;
int citySizeMax = 15;
int citySize = 10;
float citySizes[] = {};
int textSz = 20;

color indexColours[] = new color[]{
        #000000, #FFFF00, #1CE6FF, #FF34FF, #FF4A46, #008941, #006FA6, #A30059,
        #FFDBE5, #7A4900, #0000A6, #63FFAC, #B79762, #004D43, #8FB0FF, #997D87,
        #5A0007, #809693, #FEFFE6, #1B4400, #4FC601, #3B5DFF, #4A3B53, #FF2F80,
        #61615A, #BA0900, #6B7900, #00C2A0, #FFAA92, #FF90C9, #B903AA, #D16100,
        #DDEFFF, #000035, #7B4F4B, #A1C299, #300018, #0AA6D8, #013349, #00846F,
        #372101, #FFB500, #C2FFED, #A079BF, #CC0744, #C0B9B2, #C2FF99, #001E09,
        #00489C, #6F0062, #0CBD66, #EEC3FF, #456D75, #B77B68, #7A87A1, #788D66,
        #885578, #FAD09F, #FF8A9A, #D157A0, #BEC459, #456648, #0086ED, #886F4C,

        #34362D, #B4A8BD, #00A6AA, #452C2C, #636375, #A3C8C9, #FF913F, #938A81,
        #575329, #00FECF, #B05B6F, #8CD0FF, #3B9700, #04F757, #C8A1A1, #1E6E00,
        #7900D7, #A77500, #6367A9, #A05837, #6B002C, #772600, #D790FF, #9B9700,
        #549E79, #FFF69F, #201625, #72418F, #BC23FF, #99ADC0, #3A2465, #922329,
        #5B4534, #FDE8DC, #404E55, #0089A3, #CB7E98, #A4E804, #324E72, #6A3A4C,
        #83AB58, #001C1E, #D1F7CE, #004B28, #C8D0F6, #A3A489, #806C66, #222800,
        #BF5650, #E83000, #66796D, #DA007C, #FF1A59, #8ADBB4, #1E0200, #5B4E51,
        #C895C5, #320033, #FF6832, #66E1D3, #CFCDAC, #D0AC94, #7ED379, #012C58
};

//float lineWidthMin=1;
//float lineWidthMax=5;

// loads the file toursDir/id into tours
void loadTour(int id) {
  //System.out.println(id);
  String name = toursDir + "/" + id;
  BufferedReader reader = createReader(name);
  try{
    int nTours = new Integer(reader.readLine()).intValue();
    //System.out.println(nTours);
    tours = new int[nTours][];
    for(int t=0;t<nTours;++t) {
      String line[] = reader.readLine().split(" ");
      int len = new Integer(line[0]).intValue();
      //System.out.println("reading " + t + ": " + len);
      tours[t] = new int[len];
      for(int i=0;i<len;++i) {
        tours[t][i] = new Integer(line[i+1]).intValue();
        //System.out.println("reading " + i + ": " + tours[t][i]);
      }
    }
    
    // compute the load at each step
    loads = new float[nTours][];
    for(int t=0;t<nTours;++t) {
      loads[t] = new float[tours[t].length];
      loads[t][0] = capacity;
      for(int i=1;i<tours[t].length;++i) {
        loads[t][i] = loads[t][i-1] - cityDemand[tours[t][i]];
      }
    }
    lineWeight = new float[nTours][];
    for(int t=0;t<nTours;++t) {
      lineWeight[t] = new float[tours[t].length];
      for(int i=0;i<tours[t].length;++i) {
        lineWeight[t][i] = loads[t][i] / capacity;
      }
    }
    
    // Compute the total distance
    totalDistance=0.f;
    for(int t=0;t<nTours;++t) {
      for(int i=1;i<tours[t].length;++i) {
        totalDistance += abs(xLocs[tours[t][i]] - xLocs[tours[t][i-1]]) + abs(yLocs[tours[t][i]] - yLocs[tours[t][i-1]]);
      }
    }
    
    // For all edges, compute the colours
    edgesToDrawFrom.clear();
    edgesToDrawTo.clear();
    for(int u=0;u<xLocs.length;++u)
      for(int v=u+1;v<xLocs.length;++v) {
        edgeColour[0][u][v] = edgeColour[2][u][v];
        edgeColour[2][u][v] = color(255,255,255,0);
      }
        
    for(int t=0;t<nTours;++t) {
      for(int i=1;i<tours[t].length;++i) {
        int u = min( tours[t][i-1], tours[t][i] );
        int v = max( tours[t][i-1], tours[t][i] );
        color c = lerpColor( color(255,0,0,255), color(0,255,0,255), lineWeight[t][i] );
        edgeColour[2][u][v] = c;
        //edgeColour[2][u][v] = indexColours[t];
      }
    }
    
    for(int u=0;u<xLocs.length;++u)
      for(int v=u+1;v<xLocs.length;++v) {
        // blue for changes
        if(alpha(edgeColour[0][u][v]) != alpha(edgeColour[2][u][v])) {
          edgeColour[1][u][v] = color(0,0,255,255);
          edgesToDrawFrom.add(u);
          edgesToDrawTo.add(v);
        // halfway for the same
        } else if(alpha(edgeColour[0][u][v]) > 0) {
          edgeColour[1][u][v] = lerpColor(edgeColour[0][u][v], edgeColour[2][u][v], 0.5);
          edgesToDrawFrom.add(u);
          edgesToDrawTo.add(v);
        }
      }
    //for(int i=0;i<4;++i)
    //  for(int u=0;u<xLocs.length;++u)
    //    for(int v=0;v<xLocs.length;++v)
    //      edgeColour[
    
    for(int t = 0; t < tours.length; ++t)
      for(int c : tours[t]) {
        cityOnTour[c] = t+1;
        //println("" + c + " " + t);
      }
    
  } catch (Exception e) {
    System.exit(1);
  }
}

void setup() {
  size(1500,900);
  //fullScreen();
  textSize(textSz);
  textAlign(LEFT,TOP);
  step = 0;
  // the number of snapshots
  nSnapShots = new File(dataPath(toursDir)).listFiles().length;
  BufferedReader reader = createReader(instanceData);
  try{
    int nCities = new Integer(reader.readLine()).intValue();
    xLocs = new float[nCities];
    yLocs = new float[nCities];
    cityDemand = new float[nCities];
    int idx = 0;
    for(String x : reader.readLine().split(" ")) {
      xLocs[idx] = new Integer(x).intValue();
      ++idx;
    }
    idx = 0;
    for(String y : reader.readLine().split(" ")) {
      yLocs[idx] = new Integer(y).intValue();
      ++idx;
    }
    idx = 0;
    for(String y : reader.readLine().split(" ")) {
      cityDemand[idx] = new Integer(y).intValue();
      ++idx;
    }
    float mnD = cityDemand[1];
    float mxD = cityDemand[2];
    for(int i=1;i<cityDemand.length;++i) {
      mnD = min(mnD, cityDemand[i]);
      mxD = max(mxD, cityDemand[i]);
    }
    citySizes = new float[nCities];
    for(int i=1;i<cityDemand.length;++i) {
      citySizes[i] = citySizeMin + (citySizeMax-citySizeMin)*((cityDemand[i] - mnD) / (mxD-mnD));
    }
    citySizes[0] = 0.5f;
    
    capacity = new Integer(reader.readLine().split(" ")[0]);
  } catch (Exception e) {
    System.exit(1);
  }
  // normalise the locations
  float mx = max(xLocs),
        mn = min(xLocs);
  for(int i=0;i<xLocs.length;++i)
    xLocs[i] = (float)(width-margins*2)*(xLocs[i] - mn) / (mx - mn) + margins;
  mx = max(yLocs);
  mn = min(yLocs);
  for(int i=0;i<xLocs.length;++i)
    yLocs[i] = (float)(height-margins*2)*(yLocs[i] - mn) / (mx - mn) + margins;
    
  edgeColour = new color[3][][];
  for(int i=0;i<3;++i) {
    edgeColour[i] = new color[xLocs.length][];
    for(int j=0;j<xLocs.length;++j) {
      edgeColour[i][j] = new color[xLocs.length];
      for(int k=0;k<xLocs.length;++k)
        edgeColour[i][j][k] = color(255,255,255,0);
    }
  }
  cityOnTour = new int[xLocs.length];
  for(int i=0;i<xLocs.length;++i)
    cityOnTour[i] = 0;
}

void draw() {
  // background
  background(255);
  
  //for(int tour[] : tours) {
  //  stroke(153,153,153, (int)(255.*step/transitionTime));
  //  for(int i=0;i<tour.length-1;++i) {
  //    line(xLocs[tour[i]],   yLocs[tour[i]],
  //         xLocs[tour[i+1]], yLocs[tour[i+1]]);
  //  }
  //}
  
  /*
  for(int i=0;i < tours.length;++i) {
    //stroke(153,153,153, (int)(255.*step/transitionTime));
    //System.out.println( i );
    for(int j=0;j<tours[i].length-1;++j) {
      //System.out.println( lineWeight[i][j] );
      color lc = lerpColor( color(255,0,0), color(0,255,0), lineWeight[i][j] );
      lc = color( red(lc), green(lc), blue(lc), (int)(255.*step/transitionTime) );
      stroke(lc);
      //strokeWeight(lineWeight[i][j]);
      line(xLocs[tours[i][j]],   yLocs[tours[i][j]],
           xLocs[tours[i][j+1]], yLocs[tours[i][j+1]]);
    }
  }
  */
  for(int i=0;i<edgesToDrawFrom.size();++i) {
    int u = edgesToDrawFrom.get(i);
    int v = edgesToDrawTo.get(i);
    if(step < transitionTime) {
      color lc = lerpColor( edgeColour[0][u][v],
                            edgeColour[1][u][v],
                            (float) step / transitionTime );
      stroke(lc);
    } else {
      color lc = lerpColor( edgeColour[1][u][v],
                            edgeColour[2][u][v],
                            ((float) step - transitionTime) / transitionTime );
      stroke(lc);
    }
    line(xLocs[u], yLocs[u],
         xLocs[v], yLocs[v]);
  }

  //strokeWeight(1);
  // cities
  fill(150);
  stroke(0);
  for(int city=1;city<xLocs.length;++city) {
    //fill(indexColours[cityOnTour[city]]);
    ellipse(xLocs[city], yLocs[city], citySizes[city], citySizes[city]);
    textAlign(CENTER, BOTTOM);
    if(city-1 < 26)
      text("city" + (char)('A'+city-1), xLocs[city], yLocs[city] - 5);
    else
      text("cityA" + (char)('A'+city-27), xLocs[city], yLocs[city] - 5);
    textAlign(CENTER, TOP);
    if(0 < cityOnTour[city])
      text("v" + cityOnTour[city], xLocs[city], yLocs[city] + 5);
  }
  textAlign(LEFT,TOP);
  fill(0,0,255);
  ellipse(xLocs[0], yLocs[0], citySize, citySize);
  text("depot", xLocs[0], yLocs[0]);
  
  stroke(0);
  fill(250);
  rect(-10,-10,textWidth("Number of Vehicles: 100")+10, textAscent()*(1.3*6 + 1) + 30, 10);
  
  stroke(150);
  fill(150);
  float currentDist = lerp( prevTotalDistance, totalDistance, (float) step / (float) (2*transitionTime) );
  text("Distance: " + currentDist, 0, 0);
  text("Number of Vehicles: " + tours.length, 0, textAscent()*1.3);
  text("Full vehicle:", 0, textAscent()*1.3*2);
  text("Empty vehicle:", 0, textAscent()*1.3*3);
  text("Customer:", 0, textAscent()*1.3*4);
  text("CustomerID", 30, textAscent()*1.3*5);
  fill(150);
  stroke(0);
  ellipse(30 + textWidth("CustomerID")*.5,textAscent()*(1.3*5 + 1) + 10,10,10);
  textAlign(CENTER,TOP);
  text("VehicleID", 30 + textWidth("CustomerID")*.5, textAscent()*(1.3*5 + 1) + 15);
  
  
  stroke(color(0,255,0));
  line(textWidth("Empty vehicle: "), textAscent()*1.3*2.5, 200, textAscent()*1.3*2.5);
  stroke(color(255,0,0));
  line(textWidth("Empty vehicle: "), textAscent()*1.3*3.5, 200, textAscent()*1.3*3.5);
    
  //saveFrame();
  step = step+1;
  step = min(step,2*transitionTime);
  if(!pause && step == 2*transitionTime) {
    prevTours = tours;
    prevLoads = loads;
    prevLineWeight = lineWeight;
    prevTotalDistance = totalDistance;
    loadTour( min(nextSol++, nSnapShots-1) );
    if(nextSol == nSnapShots-1)
      pause = true;
    nextSol = max(0, min(nSnapShots-1, nextSol));
    step = 0;
  }
}

void keyPressed() {
  if(key == ' ') pause = !pause;
  if(step < 2*transitionTime) return;
  if(key == CODED) {
    if(pause && keyCode == RIGHT) {
      prevTours = tours;
      prevLoads = loads;
      prevLineWeight = lineWeight;
      prevTotalDistance = totalDistance;
      loadTour( min(nextSol++, nSnapShots-1) );
      nextSol = max(0, min(nSnapShots-1, nextSol));
      step = 0;
    } else if(pause && keyCode == LEFT) {
      nextSol = nextSol-2;
      prevLoads = loads;
      prevTours = tours;
      prevLineWeight = lineWeight;
      prevTotalDistance = totalDistance;
      loadTour( max(nextSol++, 0) );
      nextSol = max(0, min(nSnapShots-1, nextSol));
      step = 0;
    } else if(pause && keyCode == DOWN) {
      transitionTime = min(30, transitionTime+1);
      step = 2*transitionTime;
    } else if(pause && keyCode == UP) {
      transitionTime = max(5, transitionTime-1);
      step = 2*transitionTime;
    }
  } else if(pause && key == 's') {
    saveFrame();
  } else if(key == 'r') {
    step = 0;
    nextSol = 0;
    prevTours = new int[0][];
    tours = new int[0][];
    totalDistance = 0.f;
    edgesToDrawFrom.clear();
    edgesToDrawTo.clear();
    for(int u=0;u<xLocs.length;++u)
      for(int v=u+1;v<xLocs.length;++v)
        edgeColour[2][u][v] = color(255,255,255,0);
    for(int t = 0; t < xLocs.length; ++t)
      cityOnTour[t] = 0;
  }
}