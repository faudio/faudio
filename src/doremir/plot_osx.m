
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/thread.h>

#include <Cocoa/Cocoa.h>
#import  <CorePlot/CorePlot.h>

#define kInterval 0.05
#define kMax      10000
#define kSamples  500
#define kNumPlots 5

NSString* kPlotIds[kNumPlots] = {
  @"Signal 1",
  @"Signal 2",
  @"Signal 3",
  @"Signal 4",
  @"Signal 5"
};

static double (*gPlotFunc)(void* ct, int i, double t, double x);
static void*              gPlotData;
static long               gPlotCount;


@interface MyApplication : NSApplication
{
}

- (void)run;
// - (void)terminate:(id)sender;

@end

@implementation MyApplication

- (void)run
{             
  [[NSNotificationCenter defaultCenter]
   postNotificationName:NSApplicationWillFinishLaunchingNotification
   object:NSApp];
  [[NSNotificationCenter defaultCenter]
   postNotificationName:NSApplicationDidFinishLaunchingNotification
   object:NSApp];

  while(true)
  {
   NSEvent *event =
     [self
       nextEventMatchingMask:NSAnyEventMask
       untilDate:[NSDate distantFuture]
       inMode:NSDefaultRunLoopMode
       dequeue:YES];
   
    [self sendEvent:event];
    [self updateWindows];
    // if (plot_time > kMax)
    //   return;
  };
}

@end




@interface AEAppDelegate : NSObject<CPTPlotDataSource, CPTPlotSpaceDelegate>
{
  IBOutlet CPTGraphHostingView *hostView;
  CPTXYGraph *graph;
  NSArray *plotData;
  CPTFill *areaFill;
  CPTLineStyle *barLineStyle;
}
@end

@implementation AEAppDelegate

- (void)dealloc {
  [plotData release];
  [graph release];
  [areaFill release];
  [barLineStyle release];
  [super dealloc];
}

-(void)awakeFromNib {
  [super awakeFromNib];
  
  graph = [(CPTXYGraph *)[CPTXYGraph alloc] initWithFrame: CGRectZero];
  hostView.hostedGraph = graph;

  CPTXYPlotSpace *plotSpace = (CPTXYPlotSpace *)graph.defaultPlotSpace;
  plotSpace.xRange = [CPTPlotRange plotRangeWithLocation: CPTDecimalFromFloat(-1.1) length: CPTDecimalFromFloat(2.2)];
  plotSpace.yRange = [CPTPlotRange plotRangeWithLocation: CPTDecimalFromFloat(-1.1) length: CPTDecimalFromFloat(2.2)];
  
  CPTXYAxisSet *axisSet = (CPTXYAxisSet *)graph.axisSet;
  CPTXYAxis *x                  = axisSet.xAxis;
  x.majorIntervalLength         = CPTDecimalFromFloat(1);
  x.orthogonalCoordinateDecimal = CPTDecimalFromFloat(0);
  x.minorTicksPerInterval       = 1;
  
  CPTXYAxis *y = axisSet.yAxis;
  y.majorIntervalLength         = CPTDecimalFromFloat(1);
  y.orthogonalCoordinateDecimal = CPTDecimalFromFloat(0);
  y.minorTicksPerInterval       = 1;
  
  graph.axisSet = axisSet;
  graph.defaultPlotSpace.delegate = self;  

  CPTMutableShadow *lineShadow  = [CPTMutableShadow shadow];
  lineShadow.shadowOffset       = CGSizeMake(3.0, -3.0);
  lineShadow.shadowBlurRadius   = 4.0;
  lineShadow.shadowColor        = [CPTColor blueColor];
                     
  for(int i = 0; i < kNumPlots; ++i)
  {
    CPTScatterPlot *plot = [[[CPTScatterPlot alloc] init] autorelease];
    plot.identifier = kPlotIds[i];
    plot.dataSource = self;
    
    CPTMutableLineStyle *style = plot.dataLineStyle.mutableCopy;
    style.lineWidth = 1;
    style.lineColor = [CPTColor blueColor];
    plot.dataLineStyle = style;
    [graph addPlot: plot];
  }

  [NSTimer
    scheduledTimerWithTimeInterval:kInterval
    target:self
    selector:@selector(reload:)
    userInfo:NULL repeats:3];
}

- (void)reload:(NSTimer*)theTimer
{                    
  [graph reloadData];
  gPlotCount++;
}

-(NSUInteger)numberOfRecordsForPlot:
(CPTPlot *)plot {
  return kSamples;
}

- (NSNumber *)  numberForPlot:
  (CPTPlot *)   plot field:
  (NSUInteger)  fieldEnum recordIndex:
  (NSUInteger)  index 
{
  double t = ((float) gPlotCount) * kInterval;
  double x = ((float) index / kSamples) * 2 - 1;
  
  if (fieldEnum == CPTScatterPlotFieldX) {
    return [NSNumber numberWithDouble: x];
  }
  for(int i = 0; i < kNumPlots; ++i)
    if (plot.identifier == kPlotIds[i]) {
      return [NSNumber numberWithDouble:
        gPlotFunc(gPlotData, i, t, x)
      ];
    }

}

@end

typedef void real_void;

void start_gui()
{
  Class           principalClass          = MyApplication.class;
  NSApplication   *applicationObject      = [principalClass sharedApplication];
  NSString        *mainNibName            = @"MainMenu";
  NSNib           *mainNib                = [[NSNib alloc] initWithNibNamed:mainNibName bundle:[NSBundle mainBundle]];

  [mainNib 
    instantiateNibWithOwner:applicationObject 
    topLevelObjects:nil];
  
  if ([applicationObject respondsToSelector:@selector(run)])
  {
    [applicationObject
      performSelectorOnMainThread:@selector(run)
      withObject:nil
      waitUntilDone:YES];
  }
}   


void doremir_plot_show
(
  doremir_ternary_t   func, 
  void*               funcData, 
  doremir_nullary_t   cont, 
  doremir_ptr_t       contData
)
{
  gPlotCount  = 0;
  gPlotFunc   = func;
  gPlotData   = funcData;

  doremir_thread_create(cont, contData);
  start_gui();
}