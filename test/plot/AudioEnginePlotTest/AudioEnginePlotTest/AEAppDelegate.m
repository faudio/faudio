//
//  AEAppDelegate.m
//  AudioEnginePlotTest
//
//  Created by Hans Höglund on 2013-01-26.
//  Copyright (c) 2013 DoReMIR. All rights reserved.
//

#import "AEAppDelegate.h"

@implementation AEAppDelegate
-(void)dealloc
{
    [plotData release];
    [graph release];
    [areaFill release];
    [barLineStyle release];
    [super dealloc];
}

-(void)awakeFromNib
{
    [super awakeFromNib];
    
    // Create graph from theme
    graph = [(CPTXYGraph *)[CPTXYGraph alloc] initWithFrame:CGRectZero];
//    [graph applyTheme:[CPTTheme themeNamed:kCPTSlateTheme]];
    hostView.hostedGraph = graph;
    
    // Setup scatter plot space
    CPTXYPlotSpace *plotSpace = (CPTXYPlotSpace *)graph.defaultPlotSpace;
    plotSpace.xRange = [CPTPlotRange plotRangeWithLocation:CPTDecimalFromFloat(-1.1) length:CPTDecimalFromFloat(2.2)];
    plotSpace.yRange = [CPTPlotRange plotRangeWithLocation:CPTDecimalFromFloat(-1.1) length:CPTDecimalFromFloat(2.2)];
    
    // Axes
    CPTXYAxisSet *axisSet = (CPTXYAxisSet *)graph.axisSet;
    CPTXYAxis *x          = axisSet.xAxis;
    x.majorIntervalLength         = CPTDecimalFromFloat(1);
    x.orthogonalCoordinateDecimal = CPTDecimalFromFloat(0);
    x.minorTicksPerInterval       = 1;
        
    CPTXYAxis *y = axisSet.yAxis;
    y.majorIntervalLength         = CPTDecimalFromFloat(1);
    y.orthogonalCoordinateDecimal = CPTDecimalFromFloat(0);
    y.minorTicksPerInterval       = 1;
    
    graph.axisSet = axisSet;
    
    // Create a plot that uses the data source method
    CPTScatterPlot *myPlot = [[[CPTScatterPlot alloc] init] autorelease];
    myPlot.identifier = @"My Plot";
    myPlot.dataSource = self;
    [graph addPlot:myPlot];
    
    // Add plot
    [graph addPlot:myPlot];
    graph.defaultPlotSpace.delegate = self;
}

#pragma mark -
#pragma mark Plot Data Source Methods

-(NSUInteger)numberOfRecordsForPlot:(CPTPlot *)plot
{
    return 10000;
}

-(NSNumber *)numberForPlot:(CPTPlot *)plot field:(NSUInteger)fieldEnum recordIndex:(NSUInteger)index
{
    {
      double x = ((float)index/10000)*2-1;
      if(fieldEnum == CPTScatterPlotFieldX)
      { return [NSNumber numberWithDouble:x]; }
      else
      {
        if(plot.identifier == @"My Plot")
        { return [NSNumber numberWithDouble:sin(2*3.1415*x)]; }
        else
            ;
      }
    }
}

@end
