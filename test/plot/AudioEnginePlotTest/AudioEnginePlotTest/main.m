//
//  main.m
//  AudioEnginePlotTest
//
//  Created by Hans Höglund on 2013-01-26.
//  Copyright (c) 2013 DoReMIR. All rights reserved.
//

#import <Cocoa/Cocoa.h>


void MyApplicationMain()
{
    
    Class           principalClass      = NSApplication.class;
    NSApplication   *applicationObject  = [principalClass sharedApplication];
    NSString        *mainNibName        = @"MainMenu";
    NSNib           *mainNib            = [[NSNib alloc] initWithNibNamed:mainNibName bundle:[NSBundle mainBundle]];

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

void show_plots()
{        
    // MyApplicationMain();
    NSApplicationMain(0, (const char **)0);
}

int main(int argc, char *argv[])
{
    show_plots();
    return 0;
}
