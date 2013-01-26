//
//  AEAppDelegate.h
//  AudioEnginePlotTest
//
//  Created by Hans Höglund on 2013-01-26.
//  Copyright (c) 2013 DoReMIR. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <CorePlot/CorePlot.h>

@interface AEAppDelegate : NSObject<CPTPlotDataSource, CPTPlotSpaceDelegate>
{
    IBOutlet CPTGraphHostingView *hostView;
    CPTXYGraph *graph;
    NSArray *plotData;
    CPTFill *areaFill;
    CPTLineStyle *barLineStyle;
}


@property (assign) IBOutlet NSWindow *window;

@end
