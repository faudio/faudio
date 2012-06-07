/*
 *  core.h
 *  sclaudio
 *
 *  Created by Hans Höglund on 2011-11-03.
 *  Copyright 2011 DoReMIR http://www.doremir.com/. All rights reserved.
 *
 */


class PrintAction : public Action
{
public:
    void accept(Time time) const
    {
        cout << "Fired at time " << time << "\n";
    }
    virtual void accept(Time time, MessageInfo info) const 
    {
        cout << "Fired at time " << time << "\n";
    }
};

class PrintReceiver : public Receiver
{
public:
    virtual void accept(Time time, Message message, MessageInfo info) const
    {
        cout << "Received at " << time << " message ";
//        printAll(message, " ");
        cout << " channel " << info.channel;
        cout << "\n";    
    }
};


#define DECAY 1000


static Stream* stream;
static MidiDevice* midiInputDevice;

void openStream()
{
    cout << "========\n";
    cout << "Opening a device stream\n";

    AudioProcessor *processor = new instruments::Fluidsynth(SCL_TEST_SOUNDFONT_PATH);
//    AudioProcessor *processor = NULL;

    stream = Stream::openDeviceStream(
                    midiInputDevice = indexList(MidiDevice::devices(), 0),
                                      indexList(MidiDevice::devices(), 2),
                                      NULL,
                                      NULL,
                                      processor,
                                      DeviceStreamOptions());
//    stream = Stream::openDeviceStream();
    cout << stream << "\n";
}

void freeStream()
{
    cout << "========\n";
    cout << "Freeing device stream\n";
    delete stream;
}


void testBasicScheduling()
{
    cout << "========\n";
    cout << "Testing basic scheduling\n";

    PrintAction    Action;
    ActionScheduler& scheduler = stream->actionScheduler();
    
    stream->start(); cout << "start\n";


    scheduler.scheduleAt(Action, 3000);
    scheduler.scheduleLater(Action, 1500);
    scheduler.scheduleNow(Action);

    sleepMillis(4500);

    scheduler.scheduleNow(Action);
    scheduler.scheduleLater(Action, 1000);

    sleepMillis(1500);

    stream->stop();
}


void testActionScheduler()
{
    cout << "========\n";
    cout << "Testing action scheduler\n";

    PrintAction    Action;
    ActionScheduler& scheduler = stream->actionScheduler();
    
    for(int i = 0; i < 50; ++i)
        scheduler.scheduleLater(Action, 10 * i);

    stream->start(); cout << "start\n";
    sleepMillis(250);
    stream->stop(); cout << "stop\n";
    sleepMillis(250);
    stream->start(); cout << "start\n";
    sleepMillis(250 + DECAY);
    stream->stop(); cout << "stop\n";
    
    cout << "\n";
}

void testInterrupts()
{
    cout << "========\n";
    cout << "Testing interruption\n";

    PrintAction    Action;
    ActionScheduler& scheduler = stream->actionScheduler();
    
    for(int i = 0; i < 50; i+=2)
    {
        Future& future = scheduler.scheduleLater(Action, 100 * i);
        if (i % 7 == 0) future.interrupt();
    }
    stream->start(); cout << "start\n";
    sleepMillis(100 * 50 + DECAY);
    stream->stop(); cout << "stop\n";
    

    
    cout << "\n";
}


void testGroupInterrupts()
{
    cout << "========\n";
    cout << "Testing group interruption\n";
    
    PrintAction    Action;
    ActionScheduler& scheduler = stream->actionScheduler();
    
    FutureGroup group1 (Simple);
    FutureGroup group2 (Simple);
    ScheduleOptions  opt1;
    ScheduleOptions  opt2;

    opt1.groups.push_back(&group1);
    opt2.groups.push_back(&group2);
    
    cout << "Starting to schedule\n";
    for(int i = 0; i < 10; i+=2)
    {
        scheduler.scheduleLater(Action, 1000 * i, opt1);
    }
    for(int i = 1; i < 10; i+=2)
    {
        scheduler.scheduleLater(Action, 1000 * i, opt2);
    }
    cout << "Finishing scheduling\n";
    

    stream->start(); cout << "start\n";
    sleepMillis(5000);
    group1.interrupt();
    sleepMillis(5000 + DECAY);
    stream->stop(); cout << "stop\n";
    
    cout << "\n";
}




void testRepetition()
{
    cout << "========\n";
    cout << "Testing repetitions\n";

    PrintAction    Action;
    ScheduleOptions  options;
    ActionScheduler& scheduler = stream->actionScheduler();

    options.repeats = 5;
    options.interval = 400;

    scheduler.scheduleLater(Action, 1000, options);
    
    stream->start(); cout << "start\n";
    sleepMillis(8000 + DECAY);
    stream->stop(); cout << "stop\n";

    cout << "\n";
}


void testMidiOutputCompleteness()
{
    cout << "========\n";
    cout << "Testing completeness of midi output\n";

    MessageScheduler& scheduler = stream->midiScheduler();

//    while (true) {
    for (int i = 0; i < 13; ++i)
        scheduler.sendLater(50 * i, listFrom(messageFrom( 0x90, 60 + i, 60 )));

    for (int i = 0; i < 13; ++i)
        scheduler.sendLater(1500, listFrom(messageFrom( 0x90, 60 + i, 0 )));

//    sleepMillis(1000);
    stream->start(); cout << "start\n";
    sleepMillis(2000 + DECAY);
    stream->stop(); cout << "stop\n";
//    }
}



void testMidiInterruption()
{
    cout << "========\n";
    cout << "Testing interruption modes\n";

    MessageScheduler& scheduler = stream->midiScheduler();

    FutureGroup *simple        = new FutureGroup(Simple);
    FutureGroup *transactional = new FutureGroup(Transactional);
    FutureGroup *forcing       = new FutureGroup(Forcing);

    SendOptions withSimple        = SendOptions();
    SendOptions withTransactional = SendOptions();
    SendOptions withForcing       = SendOptions();
    withSimple.groups.push_front(simple);
    withTransactional.groups.push_front(transactional);
    withForcing.groups.push_front(forcing);
      
          
    scheduler.sendLater(1000, listFrom(messageFrom( 0x90, 60, 60)), withSimple);
    scheduler.sendLater(2000, listFrom(messageFrom( 0x90, 62, 60)), withSimple);
    scheduler.sendLater(3000, listFrom(messageFrom( 0x90, 64, 60)), withSimple);
    scheduler.sendLater(4000, listFrom(messageFrom( 0x90, 60, 0)), withSimple);
    scheduler.sendLater(4000, listFrom(messageFrom( 0x90, 62, 0)), withSimple);
    scheduler.sendLater(4000, listFrom(messageFrom( 0x90, 64, 0)), withSimple);
    
    stream->start(); cout << "start\n";
    sleepMillis(2000 + DECAY);
    simple->interrupt();
    sleepMillis(2000 + DECAY);
    stream->stop(); cout << "stop\n";



    scheduler.sendLater(1000, listFrom(messageFrom( 0x90, 60, 60)), withTransactional);
    scheduler.sendLater(2000, listFrom(messageFrom( 0x90, 62, 60)), withTransactional);
    scheduler.sendLater(3000, listFrom(messageFrom( 0x90, 64, 60)), withTransactional);
    scheduler.sendLater(4000, listFrom(messageFrom( 0x90, 60, 0)), withTransactional);
    scheduler.sendLater(4000, listFrom(messageFrom( 0x90, 62, 0)), withTransactional);
    scheduler.sendLater(4000, listFrom(messageFrom( 0x90, 64, 0)), withTransactional);
    
    stream->start(); cout << "start\n";
    sleepMillis(2000 + DECAY);
    transactional->interrupt();
    sleepMillis(2000 + DECAY);
    stream->stop(); cout << "stop\n";




    scheduler.sendLater(1000, listFrom(messageFrom( 0x90, 60, 60)), withForcing);
    scheduler.sendLater(2000, listFrom(messageFrom( 0x90, 62, 60)), withForcing);
    scheduler.sendLater(3000, listFrom(messageFrom( 0x90, 64, 60)), withForcing);
    scheduler.sendLater(4000, listFrom(messageFrom( 0x90, 60, 0)), withForcing);
    scheduler.sendLater(4000, listFrom(messageFrom( 0x90, 62, 0)), withForcing);
    scheduler.sendLater(4000, listFrom(messageFrom( 0x90, 64, 0)), withForcing);
    
    stream->start(); cout << "start\n";
    sleepMillis(3000);
    forcing->interrupt();
    sleepMillis(2000 + DECAY);
    stream->stop(); cout << "stop\n";



    delete simple, forcing, transactional;
    cout << "\n";
}


void testMidiInterruption2()
{
    cout << "========\n";
    cout << "Testing interruption modes 2\n";

    MessageScheduler& scheduler = stream->midiScheduler();

    FutureGroup *forcing       = new FutureGroup(Forcing);
    SendOptions withForcing       = SendOptions();
    withForcing.groups.push_front(forcing);
      
    for(int i = 0; i < 70; ++i)
    {
        scheduler.sendLater(100 * i, listFrom(messageFrom( 0x90, 30 + i, 60)), withForcing);
        scheduler.sendLater(8000, listFrom(messageFrom( 0x90, 30 + i, 0)), withForcing);
    }
    
    stream->start(); cout << "start\n";
    sleepMillis(3000);
    forcing->interrupt();
    sleepMillis(DECAY);
    stream->stop(); cout << "stop\n";

    delete forcing;
    cout << "\n";
}


void testMidiInput(int seconds)
{
    cout << "========\n";
    cout << "Testing midi input\n";

    PrintReceiver receiver;
    MessageScheduler& scheduler = stream->midiScheduler();

    Future& midiIn = scheduler.receive(receiver);

    stream->start();
    cout << "Listening on device '" << midiInputDevice->name() << "' for " << seconds << " seconds\n";
    
    sleepMillis(seconds * 1000);
    
    midiIn.interrupt();
    stream->stop();
    cout << "Stopped listening\n";
    
    sleepMillis(DECAY);
    
    cout << "\n";
}



void testMidiOutput(int iterations)
{
    cout << "========\n";
    cout << "Testing midi output\n";

    MessageScheduler& scheduler = stream->midiScheduler();


    cout << "Starting to schedule\n";

    int pitch;
    const int min = 60, length = 24;

    for(int i = 0; i < iterations * 3 / 2; ++i)
    {
        pitch = (pitch ^ (pitch >> 3)) % length + min; 
        if (pitch % 3 == 0) pitch++;
               
        scheduler.sendLater(200 * i,       listFrom(messageFrom( 0x90, pitch, (int) round(sin(i/24.0)*35+50) )));
        scheduler.sendLater(200 * i + 800, listFrom(messageFrom( 0x90, pitch, 0 )));
    }

    for(int i = 0; i < iterations; ++i)
    {
        pitch = (++pitch ^ (pitch >> 5)) % length + min; 
        if (pitch % 17 == 0) pitch++;

        scheduler.sendLater(300 * i,       listFrom(messageFrom( 0x90, pitch, (int) round(sin(i/20.0)*35+50) )));
        scheduler.sendLater(300 * i + 800, listFrom(messageFrom( 0x90, pitch, 0 )));
    }

    cout << "Finishing scheduling\n";
    
    sleepMillis(1000);

    stream->start(); cout << "start\n";
    sleepMillis(iterations * 300 + DECAY);
    stream->stop(); cout << "stop\n";

    cout << "\n";
}





void testMidiOutputChannels()
{
    cout << "========\n";
    cout << "Testing midi channel mapping for outputs\n";

    MessageScheduler& scheduler = stream->midiScheduler();


    stream->start(); cout << "start\n";
    for (int i = 0; i < 16; ++i) {
        cout << "Sending one note on channel " << i << "\n";
    
        SendOptions opt;
        opt.channels = listFrom(i);
        scheduler.sendNow(listFrom(messageFrom(0x90, 60, 80)), opt);
        sleepMillis(200);
    }
    stream->stop(); cout << "stop\n";
    
    sleepMillis(DECAY);
    cout << "\n";

}



// ==============================



static Stream* audioStream;

void openAudioStream()
{
    cout << "========\n";
    cout << "Opening an audio device stream\n";

    AudioProcessor *processor = new instruments::Fluidsynth(SCL_TEST_SOUNDFONT_PATH);

    audioStream = Stream::openDeviceStream(NULL,
                                      NULL,
                                      AudioDevice::defaultInputDevice(),
                                      AudioDevice::defaultOutputDevice(),
                                      processor,
                                      DeviceStreamOptions());
    cout << stream << "\n";
}

void freeAudioStream()
{
    cout << "========\n";
    cout << "Freeing audio device stream\n";
    delete audioStream;
}




void testStartStopAudio()
{
    SendOptions opts;
    opts.kind = MidiMessage;

    cout << "========\n";
    cout << "Testing to start and stop an audio stream\n";

    audioStream->start();

    sleepMillis(1000);

    try {
        audioStream->messageScheduler().sendNow(listFrom(messageFrom(0x90, 60, 120)), opts);
        audioStream->messageScheduler().sendNow(listFrom(messageFrom(0x90, 62, 120)), opts);
        audioStream->messageScheduler().sendNow(listFrom(messageFrom(0x90, 63, 120)), opts);
    }
    catch (StreamError& e) {
        cerr << "StreamError: " << e.message() << "\n";
    }


    sleepMillis(5000 + DECAY);
    
    audioStream->stop();
    cout << "\n";
}



class PrintHandler : public Handler<Error>
{
    void accept(Time time, Error& error) const
    {
        cout << time << " " << error.message() << "\n";
        std::abort();
    }
};

void testErrorHandler()
{
    DeviceStreamOptions streamOpts;
    streamOpts.audioBufferSize = 48;
    PrintHandler handler;
    
    SendOptions sendOpts;
    sendOpts.kind = AudioMessage;
    
    cout << "========\n";
    cout << "Testing error handlers for streams\n";

    audioStream = Stream::openDeviceStream(
        MidiDevice::defaultInputDevice(), MidiDevice::defaultOutputDevice(),
        AudioDevice::defaultInputDevice(), AudioDevice::defaultOutputDevice(), new instruments::Fluidsynth(SCL_TEST_SOUNDFONT_PATH), 
        streamOpts);

    audioStream->setHandler(&handler);
    audioStream->start();

    for (int i = 0; i < 100; ++i)
    {
        sleepMillis(100);
        audioStream->messageScheduler().sendNow(listFrom(messageFrom(0x90, 48 + i, 120)), sendOpts);
   }

    sleepMillis(5000 + DECAY);
    
    audioStream->stop();
    audioStream->setHandler(NULL);
    cout << "\n";    
}



