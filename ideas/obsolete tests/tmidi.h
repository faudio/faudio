/*
 *  core.h
 *  sclaudio
 *
 *  Created by Hans Höglund on 2011-11-03.
 *  Copyright 2011 DoReMIR http://www.doremir.com/. All rights reserved.
 *
 */


void testDefaultMidiDevices()
{
    cout << "========\n";
    cout << "Testing to get default midi devices\n";

    MidiDevice* in = MidiDevice::defaultInputDevice();
    MidiDevice* out = MidiDevice::defaultOutputDevice();
    
    out = in = out; // kill unused warning
}

static void listMidiDevices()
{
    list<MidiDevice*> devs = MidiDevice::devices();
    for(list<MidiDevice*>::iterator d = devs.begin(); d != devs.end(); ++d)
    {
        cout << "      " 
             << (*d)->id() << " "
             << (*d)->name() << " "
             << "(" 
             << ((*d)->hasInput() ? "in" : "")  
             << (((*d)->hasInput() && (*d)->hasOutput()) ? "/" : "")
             << ((*d)->hasOutput() ? "out" : "")  
             << ")" 
             << "\n";
    }
    cout << "\n";
}

void testMidiSystem()
{
    cout << "========\n";
    cout << "Testing midi system\n";

    cout << "   Midi devices:\n";
    listMidiDevices();
}


#define SCL_TEST_CONNECT_DISCONNECT_SECS 10

void testConnectDisconnect()
{
    cout << "========\n";
    cout << "Testing connecting and disconnecting midi devices\n";

    Pm_Initialize();
    cout << Pm_CountDevices() << " devices connected\n";
//    listMidiDevices();
    
    cout << "Please change the midi setup within "
         << SCL_TEST_CONNECT_DISCONNECT_SECS 
         << " seconds...\n";
    sleepMillis(SCL_TEST_CONNECT_DISCONNECT_SECS * 1000);

    Pm_Terminate();
    Pm_Initialize();

    cout << Pm_CountDevices() << " devices connected\n";
//    listMidiDevices();

    Pm_Terminate();
    
}


void testConvertMidi()
{
    cout << "========\n";
    cout << "Testing conversion of Midi messages\n";


    Message x   = listFrom(toAtom(64), toAtom(128), toAtom(3), toAtom(32));
    PmMessage m = messageToMidi(x, 10);
    Message y   = midiToMessage(m);

    printAll(x, " "); 
    cout << "\n";

//    print(m, "\n");

    printAll(y, " "); 
    cout << "\n";

    cout << "\n";

    x = listFrom(toAtom(1), toAtom(2), toAtom(3));
    m = messageToMidi(x, 10);
    y = midiToMessage(m);

    printAll(x, " "); 
    cout << "\n";

//    print(m, "\n");

    printAll(y, " "); 
    cout << "\n";

}



void testPortmidi()
{
//    PmDeviceID devId = 4;
//    PmStream* stream;
//    PmEvent buffer[1024];
//    PmError err;
//
//    err = Pm_OpenOutput(&stream, devId, NULL, 1024, NULL, NULL, 0);
//    if (err != pmNoError)
//        throw PortmidiError(err);
//
//    for (int i = 0; i < 10; ++i)
//    {
//        buffer[i].timestamp = i * 100;
//        buffer[i].message   = Pm_Message(0x90, 60, 127);
//    }
//
//        
//    err = Pm_Write(stream, buffer, 10);
//    if (err != pmNoError)
//        throw PortmidiError(err);

}
