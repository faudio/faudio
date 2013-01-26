
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/util.h>
#include <Cocoa/Cocoa.h>
#import  <CorePlot/CorePlot.h>


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
  
  // TODO more plots
  CPTScatterPlot *plot1 = [[[CPTScatterPlot alloc] init] autorelease];
  plot1.identifier = @"Signal 1";
  plot1.dataSource = self;

  CPTScatterPlot *plot2 = [[[CPTScatterPlot alloc] init] autorelease];
  plot2.identifier = @"Signal 2";
  plot2.dataSource = self;

  CPTMutableLineStyle *style1 = plot1.dataLineStyle.mutableCopy;
  style1.lineWidth = 1;
  style1.lineColor = [CPTColor blueColor];
  plot1.dataLineStyle = style1;
  [graph addPlot: plot1];
  
  CPTMutableLineStyle *style2 = plot1.dataLineStyle.mutableCopy;
  style2.lineWidth = 1;
  style2.lineColor = [CPTColor darkGrayColor];
  plot2.dataLineStyle = style2;
  [graph addPlot: plot2];

  [NSTimer
    scheduledTimerWithTimeInterval:0.1
    target:self
    selector:@selector(reload:)
    userInfo:NULL repeats:3];
}

- (void)reload:(NSTimer*)theTimer
{
  [graph reloadData];
}

#define kSamples 1000

-(NSUInteger)numberOfRecordsForPlot:
(CPTPlot *)plot {
  return kSamples;
}

- (NSNumber *) numberForPlot:
  (CPTPlot *)   plot field:
  (NSUInteger)  fieldEnum recordIndex:
  (NSUInteger)  index 
{
  double t = (((double)clock())/(double)CLOCKS_PER_SEC);
  double x = ((float)index / kSamples) * 2 - 1;
  double tau = 2 * 3.1415;
  
  if (fieldEnum == CPTScatterPlotFieldX) {
    return [NSNumber numberWithDouble: x];
  } else
  {
    if (plot.identifier == @"Signal 1") {
      return [NSNumber numberWithDouble: 
        0.1*sin(tau * x * 1 + (t*30))
      ];
    }
    if (plot.identifier == @"Signal 2") {
      return [NSNumber numberWithDouble: 
        x 
      ];
    }
  }
  assert(false);
}

@end

void MyApplicationMain()
{
  Class           principalClass          = NSApplication.class;
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

void doremir_plot_show()
{         
  // TODO setup plots
  MyApplicationMain();
}