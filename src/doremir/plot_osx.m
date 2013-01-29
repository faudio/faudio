
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/plot.h>
#include <doremir/thread.h>

#include <Cocoa/Cocoa.h>
#import  <CorePlot/CorePlot.h>

// TODO optimize constants etc, tune this

#define interval_k  0.05
#define max_k       10000
#define samples_k   1000
#define num_plots_k 5

NSString* const plot_ids_k[num_plots_k] = { @"1", @"2", @"3", @"4", @"5" };

typedef double (*plot_func_t)(void* ct, int i, double t, double x);
static plot_func_t  plot_func_g;
static void*        plot_ct_g;
static long         plot_count_g;

// -----------------------------------------------------------------------------

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
    // if (plot_time > max_k)
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

  for(int i = 0; i < num_plots_k; ++i)
  {
    CPTScatterPlot *plot = [[[CPTScatterPlot alloc] init] autorelease];
    plot.identifier = plot_ids_k[i];
    plot.dataSource = self;

    CPTMutableLineStyle *style = plot.dataLineStyle.mutableCopy;
    style.lineWidth = 1;
    style.lineColor = [CPTColor blueColor];
    plot.dataLineStyle = style;
    [graph addPlot: plot];
  }

  [NSTimer
    scheduledTimerWithTimeInterval:interval_k
    target:self
    selector:@selector(reload:)
    userInfo:NULL repeats:3];
}

- (void)reload:(NSTimer*)theTimer
{
  [graph reloadData];
  plot_count_g++;
}

-(NSUInteger)numberOfRecordsForPlot:
(CPTPlot *)plot {
  return samples_k;
}

- (NSNumber *)  numberForPlot:
  (CPTPlot *)   plot field:
  (NSUInteger)  fieldEnum recordIndex:
  (NSUInteger)  index
{
  double t = ((float) plot_count_g) * interval_k;
  double x = ((float) index / samples_k) * 2 - 1;

  if (fieldEnum == CPTScatterPlotFieldX) {
    return [NSNumber numberWithDouble: x];
  }
  for(int i = 0; i < num_plots_k; ++i)
    if (plot.identifier == plot_ids_k[i]) {
      return [NSNumber numberWithDouble:
        plot_func_g(plot_ct_g, i, t, x)
      ];
    }
    assert(false && "Not reached");
}

@end


// -----------------------------------------------------------------------------

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

/** Run a plot of the given functions.
 */
void doremir_plot_func
(
  doremir_plot_func_t func,
  doremir_ptr_t       funcData,
  doremir_nullary_t   cont,
  doremir_ptr_t       contData
)
{
  plot_count_g  = 0;
  plot_func_g   = (plot_func_t) func;
  plot_ct_g     = funcData;

  doremir_thread_create(cont, contData);
  start_gui();
}  

#define PLOTTER(T) \
    double plot_##T(void * ct, int i, double t, double x)       \
    {                                                           \
        doremir_buffer_t buf = ct;                              \
                                                                \
        size_t  sz = doremir_buffer_size(buf) / sizeof(T);      \
        T     * ds = doremir_buffer_unsafe_address(buf);        \
                                                                \
        if (i == 0) {                                           \
            return ds[((size_t)(sz * ((x + 1) / 2)))];          \
        } else if (i == 1) {                                    \
            return ds[((size_t)(sz * ((x + 1) / 2)))] * -1;     \
        } else {                                                \
            return -2;                                          \
        }                                                       \
    }                                                           \

PLOTTER(float);
PLOTTER(double);

/** Run a plot on the given buffer, treating its contents as
    32-bit floating point data.
 */
void doremir_plot_buffer_float(doremir_buffer_t  buffer,
                               doremir_nullary_t cont,
                               doremir_ptr_t     data)
{
    doremir_plot_func(plot_float, buffer, cont, data);
}

/** Run a plot on the given buffer, treating its contents as
    64-bit floating point data.
 */
void doremir_plot_buffer_double(doremir_buffer_t      buffer,
                                doremir_nullary_t     cont,
                                doremir_ptr_t         data)
{
    doremir_plot_func(plot_double, buffer, cont, data);
}

