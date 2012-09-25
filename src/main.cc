
#include <iostream>
#include <vector>

#include "sclaudio.h"
#include "sclaudiox.h"

using namespace doremir::scl;

#ifdef SCL_WIN
	#define SOUNDFONT_LOCATION \
	    "C:\\modus\\app\\resources\\soundfonts\\sound.sf2"
#else
	#define SOUNDFONT_LOCATION \
	    "/Users/hans/Documents/Kod/doremir/modus/app/resources/soundfonts/sound.sf2"
#endif


static AudioProcessor*  currentSynth  = NULL;
static Stream*          currentStream = NULL;
static Stream*          lastStream    = NULL;

void listDevices()
{
    std::cout << "\n" 
              << "Audio devices\n"
              << "=============\n";


    std::list<AudioHost*> hosts = AudioHost::hosts();
	std::cout << "Number of hosts: " << hosts.size();
    for (std::list<AudioHost*>::iterator host = hosts.begin(); host != hosts.end(); ++host)
    {
        std::cout << "  "
                  << "Id: " << (*host)->index()
                  << "   "
                  << "Name: " << (*host)->name() 
                  << "   " 
                  << "Mode: " << ((*host)->isExclusive() ? "Exlusive" : "Non-exlusive")
                  << "\n";

        std::list<AudioDevice*> devices = (*host)->devices();
        for (std::list<AudioDevice*>::iterator device = devices.begin(); device != devices.end(); ++device)
        {
            std::cout << "    " 
                      << "Id: " << (*device)->index()
                      << "   " 
                      << "Name: " << (*device)->name() 
                      << "\n"; 
        }
    }
}              

void useLast()
{
    Stream* temp = currentStream;
    currentStream = lastStream;
    lastStream = temp;
    std::cout << "Using previous stream\n";
}

void printStatus()
{     
    if (!currentStream)                    
        std::cout << "No current stream\n";
    else
        std::cout << "Current stream is " << (currentStream->running() ? "running" : "not running") << "\n";

    if (!lastStream)                    
        std::cout << "No last stream\n";
    else
        std::cout << "Last stream is " << (lastStream->running() ? "running" : "not running") << "\n";
}

void openStream(int host, int device)
{                                    
    AudioDevice* output = list::index(list::index(AudioHost::hosts(), host)->devices(), device);
    
    if (!currentSynth)
        currentSynth = AudioUnit::dlsMusicDevice()->createAudioUnitProcessor();
    //     currentSynth = new FluidSynth(SOUNDFONT_LOCATION);
    
    Stream* temp = currentStream;
    currentStream = DeviceStream::open(NULL, NULL, NULL, output, currentSynth);
    lastStream = temp;
    
    sleepMillis(500);
    printStatus();
}

void startStream()
{    
    if (!currentStream)                    
    {
        std::cout << "No current stream\n";
        return;
    }
    
    currentStream->start();

    sleepMillis(500);
    printStatus();
}
    
void stopStream()
{
    if (!currentStream)                    
    {
        std::cout << "No current stream\n";
        return;
    }

    currentStream->stop();    

    sleepMillis(500);
    printStatus();
}

void closeStream()
{
    if (!currentStream)                    
    {
        std::cout << "No current stream\n";
        return;
    }
    delete currentStream;
    currentStream = NULL;
}



int listOptions(std::vector<String> options)
{
    int size = options.size();
    int res;

    std::cout << "Please choose an option:" << "\n";
    for (int i = 0; i < size; ++i)
    {
        std::cout << "  (" << i << ") " << options[i] << "\n";
    }           
    std::cin >> res;

    if (res >= size) 
        return -1;
    else
        return res;
}


void handleError(Error& e)
{          
    ;
    std::cout << e.message() << "\n";
}

void interactive()
{
    printf("ScoreCleaner Audio Engine\n");

    std::vector<String> options;
    options.push_back("List devices");
    options.push_back("Use last stream");
    options.push_back("Print status");

    options.push_back("Open device stream");
    options.push_back("Start current stream");
    options.push_back("Stop current stream");
    options.push_back("Close current stream");

    options.push_back("Break");
    options.push_back("Exit");

    while (true)
    {            
		try 
		{
    		int opt = listOptions(options);
            switch (opt)
            {          
                case 0:
                    listDevices();
                    break;

                case 1:
                    useLast();
                    break;            

                case 2:
                    printStatus();
                    break;
            
                case 3: 
        			{
                    int host, device;
                    std::cout << "Enter host id:\n";
                    std::cin >> host;
                    std::cout << "Enter device id:\n";
                    std::cin >> device;
                    openStream(host, device);
					}
                    break;
            
                case 4:
                    startStream();
                    break;
                case 5:
                    stopStream();
                    break;
                case 6:
                    closeStream();
                    break;

				case 7:
					{
                    // Breakpoint here
					}
					break;
				case 8:
                    exit(0);
                    break;

                default:
                    std::cout << "Invalid option.";
            }
            getchar();
        } catch (Error& e) {
            handleError(e);
    	}                     
	}
}

void testAu()
{      
    AudioUnit* unit  = AudioUnit::dlsMusicDevice();
    std::cout << "Address: " << unit << "\n";
    
    
    try 
    {
        AudioProcessor*   synth  = unit->createProcessor();
        AudioDevice*      in     = AudioDevice::defaultInputDevice();
        AudioDevice*      out    = AudioDevice::defaultOutputDevice();
        Stream*           stream = DeviceStream::open(in, out, synth);
        MessageScheduler* sched  = stream->audioScheduler();
            
        stream->start();  

        foreach( int p, list::create(0,2,4,5,7) )
        {
            Message on  = messageFrom(0x90, 60 + p, 70);
            Message off = messageFrom(0x90, 60 + p, 0);    
            sched->sendNow(list::create(on));
            sched->sendLater(100, list::create(off));
            sleepMillis(100);
        }
        sleepMillis(500);    
    } 
    catch (Error& e)
    {
        std::cerr << "Error : " << e.message();
    }              
    catch (...)
    {
        std::cerr << "Unknown error";
    }       
    
}    

int main(int argc, char const* argv[]) 
{             
    interactive();

#ifdef _WIN32
    getchar();
#endif

    return 0;
}