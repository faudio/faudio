/*
 *  core.h
 *  sclaudio
 *
 *  Created by Hans Höglund on 2011-11-03.
 *  Copyright 2011 DoReMIR http://www.doremir.com/. All rights reserved.
 *
 */


void testAudioSystem()
{
    cout << "========\n";
    cout << "Testing audio system\n";

    cout << "   Audio hosts:\n";
    list<AudioHost*> hosts = AudioHost::hosts();
    for(list<AudioHost*>::iterator h = hosts.begin(); h != hosts.end(); ++h)
    {
        cout << "      " << (*h)->name() << "\n";
    }
    cout << "\n";

    cout << "   Audio devices:\n";
    list<AudioDevice*> devs = AudioHost::defaultHost()->devices();
    for(list<AudioDevice*>::iterator h = devs.begin(); h != devs.end(); ++h)
    {
        cout << "      " << (*h)->name() << "\n";
    }
    cout << "\n";
}



