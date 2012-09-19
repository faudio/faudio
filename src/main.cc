
#include <iostream>
#include <vector>

#include "sclaudio.h"
#include "sclaudiox.h"

#include "sclaudiox/util/logging.h"
#include "sclaudiox/util/concurrency.h"

using namespace doremir::scl;

/*
int times100(int x)
{
    return x * 100;
}

List<int>* range(int x, int y)
{
    List<int>* xs = new LinkedList<int>();
    for (; x < y; x++)
    {
        xs = xs->cons(x);
    }
    return xs->reverse();
}

void testLists() 
{          
    List<int>* xs = range(1000, 2000);
    List<int>* ys = range(3000, 4000);
    List<int>* xs2 = xs->append(ys);
//   List<int>* xs2 = map<int,int>(&times100, xs);

    std::cout << 

        toString(xs2)
        << "\n" <<
       
        toString(xs2->head())
        << "\n" <<

        toString(xs2->tail())
        << "\n" <<
        
        toString(xs2->last())
        << "\n" <<
        
        toString(xs2->init())

        << "\n";
}
*/
/*
static AtomicSingleReadWriteQueue<int> queue;

int sumP = 0, lastP = 0;
void producer()
{      
    for (int i = 0;; ++i)
    {             
        lastP = i;
        sumP += i;
        queue.insert(i);
        interruptionPoint();       
    }
}
    
int sumC = 0, lastC = 0;
void consumer()
{
    int x;
    for(;;)
    {
        if (queue.remove(&x))
        {
            lastC = x;
            sumC += x;
//            std::cout << x << "\n";
            interruptionPoint();       
        }
    }
}


void testAtomic()
{   
    Thread *cons, *prod;  
    cons = new Thread(consumer);
    prod = new Thread(producer);
    
    sleepMillis(1000);
    cons->interrupt();
    prod->interrupt();
    // cons->join();
    // prod->join();

    std::cout.flush();
    std::cout << "Producer: last " << lastP << ", sum " << sumP << "\n";
    std::cout << "Consumer: last " << lastC << ", sum " << sumC << "\n";
    std::cout << "Done!\n";
    std::cout.flush();
}

*/

#ifdef SCL_WIN
	#define SOUNDFONT_LOCATION \
	    "C:\\modus\\app\\resources\\soundfonts\\sound.sf2"
#else
	#define SOUNDFONT_LOCATION \
	    "/Users/hans/Documents/Kod/doremir/modus/app/resources/soundfonts/sound.sf2"
#endif

// void testAudioStream()
// {
    // 
    // AudioDevice* outDevice = AudioDevice::defaultOutputDevice();
    // std::cout << outDevice->name() << "\n";
    // 
    // AudioProcessor* synth = new Fluidsynth(SOUNDFONT_LOCATION);
    // 
    // Stream* stream = openDeviceStream(NULL, NULL, NULL, outDevice, synth);
    // 
    // SendOptions opts;
    // opts.kind = AudioMessage;
    // MessageScheduler* sched = &(stream->messageScheduler());
    // 
    // stream->start();    
    // 
    // sched->sendNow(listFrom(messageFrom(0x90, 60, 100)), opts);
    // sleepMillis(1000);
    // 
    // sched->sendNow(listFrom(messageFrom(0x90, 60, 100)), opts);
    // sleepMillis(1000);
    // 
    // sched->sendNow(listFrom(messageFrom(0x90, 60, 100)), opts);
    // sleepMillis(1000);
    // 
    // stream->stop();
    // sleepMillis(1000);
// }      
   

// class LivingThing
// { 
// public:
//     LivingThing()
//     {    
//         SCL_WRITE_LOG(this << " was born\n");
//     }
//     ~LivingThing()
//     {
//         SCL_WRITE_LOG(this << " died\n");
//     }
// };
// 
// static ThreadLocal<LivingThing> threadLocalValue;
// 
// void testThreadLocalFoo()
// {
//     threadLocalValue.get();
//     threadLocalValue.get();
//     threadLocalValue.get();
//     threadLocalValue.get();
//     sleepMillis(1000);
// }
// 
// void testThreadLocal()
// {
//     Thread *foo;
//     for (int i = 0; i < 10; ++i)
//         foo = new Thread(testThreadLocalFoo);
//     foo->join(); 
//     sleepMillis(1000);
// }



       


















// ContestedSemaphore<int> tick;
// SemaphoreIsFree<int>     odds  (&tick);
// SemaphoreIsTaken<int>    evens (&tick);
// HoldsSemaphore<int>      tick1 (&tick);


// MutableSemaphore<ThreadName> deviceOwner;
// MutableSemaphore<ThreadName> deviceUser;





// void testSemaphoreRel(void* data)
// {
//     SCL_WRITE_LOG("Released\n");
// }
// 
// void testSemaphoreOdds(int n)
// {    
//     while (true)
//     {        
//         if (odds.acquire(n, testSemaphoreRel, NULL))
//         {
//             SCL_WRITE_LOG("Got " << n << " \n");
//             sleepMillis(300);
//             odds.release(n);
//         }
//     }
// }
// 
// void testSemaphoreEvens(int n)
// {    
//     while (true)
//     {        
//         if (evens.acquire(n, testSemaphoreRel, NULL))
//         {
//             SCL_WRITE_LOG("Got " << n << " \n");
//             sleepMillis(300);
//             evens.release(n);
//         }
//     }
// }
//       
// Semaphore<int>      semX;
// SemaphoreIsFree<int> semY (&semX);
// 
// void rel1(void*)
// {
//  SCL_WRITE_LOG("1 was released\n");
// }
// void rel2(void*)
// {
//  SCL_WRITE_LOG("2 was released\n");
// }
// 
// void testSemaphore()
// {
//     // if (semY.acquire(1))
//     // {
//         // SCL_WRITE_LOG("got 1\n");
//     // }
//     // semY.release(1);
//     
//  Semaphore<int>*          dummySemaphore = new Semaphore<int>();
//  ContestedSemaphore<int>* openSemaphore;
//  Semaphore<int>* startSemaphore;
// 
//  
//  openSemaphore  = new SemaphoreIsFree<int>(NULL);
//     startSemaphore = new HoldsSemaphore<int>(openSemaphore);
//  // ((SemaphoreIsFree<int>*) openSemaphore)->semaphore = startSemaphore;
//  ((SemaphoreIsFree<int>*) openSemaphore)->semaphore = dummySemaphore;
//  
//  if (openSemaphore->acquire(1, rel1, NULL))
//  {
//      SCL_WRITE_LOG("opened 1\n");
//      if (startSemaphore->acquire(1))
//      {
//          SCL_WRITE_LOG("started 1\n");
//      }       
//  }
//  openSemaphore->release(1);
// 
//  // openSemaphore->acquire(1, rel1, NULL);
//  // openSemaphore->acquire(2, rel2, NULL);
//  // if (startSemaphore->acquire(1))
//  // {
//      // SCL_WRITE_LOG("x started 1\n");
//  // }        
//  
//  if (openSemaphore->acquire(2, rel2, NULL))
//  {
//      SCL_WRITE_LOG("opened 2\n");
//      if (startSemaphore->acquire(1))
//      {
//          SCL_WRITE_LOG("started 1\n");
//      }       
//      if (startSemaphore->acquire(2))
//      {
//          SCL_WRITE_LOG("started 2\n");
//      }       
//  }
//  openSemaphore->release(1);  
// }                     


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
    
    // if (!currentSynth)
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
    testAu();
    // interactive();

    // testAtomic();
    // testAudioStream();
    // testAudioStream();
    
   // testThreadLocal();
    //	testSemaphore();

#ifdef _WIN32
    getchar();
#endif

    return 0;
}