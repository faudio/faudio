/*
 *  core.h
 *  sclaudio
 *
 *  Created by Hans Höglund on 2011-11-03.
 *  Copyright 2011 DoReMIR http://www.doremir.com/. All rights reserved.
 *
 */
 
 
//void printIntMessages(Control& c)
//{
//    for (int i = 0; i < c.size(); ++i)
//    {
//        cout << "index " << i << " ";
//        cout << "updated: " << c.updated(i);
//
//        if (c.updated(i))
//        {
//            Message m = c.read(i);
//            cout << " value: ";
//        
//            for(Message::iterator j = m.begin(); j != m.end(); ++j)
//                cout << j->getInt() << " ";
//        }
//        cout << "\n";
//    }
//    cout << "\n";
//}

//void printStringIntMessages(Control& c)
//{
//    for (int i = 0; i < c.size(); ++i)
//    {
//        cout << "index " << i << " ";
//        cout << "updated: " << c.updated(i);
//
//        if (c.updated(i))
//        {
//            Message m = c.read(i);
//            cout << " value: ";
//
//            Message::iterator j = m.begin();
//            cout << j->getString() << " ";
//            for(++j; j != m.end(); ++j)
//                cout << j->getInt() << " ";
//        }
//        cout << "\n";
//    }
//    cout << "\n";
//}

//void printMessageTypes(Control& c)
//{
//    for (int i = 0; i < c.size(); ++i)
//    {
//        cout << "index " << i << " ";
//        cout << "updated: " << c.updated(i);
//
//        if (c.updated(i))
//        {
//            Message m = c.read(i);
//            cout << " type: ";
//            for(Message::iterator j = m.begin(); j != m.end(); ++j)
//                cout << j->getTypeName() << " ";
//        }
//        cout << "\n";
//    }
//    cout << "\n";
//}

 
 

//void testControl()
//{
//    std::cout << "========\n";
//    std::cout << "Testing control\n";
//    
//    Control x (SchedulerClient, 3);
//    Control y (ProcessorClient, 3);
//
//    x.write(0, 60, 5);
//    x.write(2, 70, 5);
//
//    x >> y;
//
//    printIntMessages(x);
//    printIntMessages(x);
//
//    printIntMessages(y);
//    printIntMessages(y);
//}
//
//
//void testControlStringInt()
//{
//    std::cout << "========\n";
//    std::cout << "Testing control with strings\n";
//    
//    Control x (SchedulerClient, 2);
//    Control y (ProcessorClient, 2);
//
//    x.write(0, "noteon ", 60, 127);
//    x.write(1, "noteoff", 60);
//
//    printStringIntMessages(x);
//    printStringIntMessages(x);
//    x >> y;
//    printStringIntMessages(y);
//    printStringIntMessages(y);
//
//    y.write(1, "noteoff", 60);
//    y >> x;
//    printStringIntMessages(x);
//    printStringIntMessages(y);
//}
//
//void testControlAccessors()
//{
//    std::cout << "========\n";
//    std::cout << "Testing control accessors\n";
//
//    Control x (SchedulerClient, 3);
//    Control y (ProcessorClient, 3);
//    
//    x.write(0, 1);
//    x.write(1, "hans");
//    x.write(2, 31, "55");
//    
//    x >> y;
//
//    int a, b;
//    string s, t;
//
//    y.select(0, a);
//    y.select(1, s);
//    y.select(2, b, t);
//    cout << a << " " << b << "\n";
//    cout << s << " " << t << "\n";
//
//}
//
//
//
//void testControlSubrange()
//{
//    std::cout << "========\n";
//    std::cout << "Testing control subranges\n";
//
//    const int N = 100;
//
//    Control x (SchedulerClient, N);
//    for (int i = 0; i < x.size(); ++i)
//        x.write(i, i);
//    
//    Control y = x.subrange(90, 10);
//
//    for (int i = 0; i < y.size(); ++i)
//        cout << y.read(i).front().getInt() << " ";
//
//    cout << "\n";
//
//}
//
//
//
//static boost::mutex xmutex;
//static boost::mutex ymutex;
//static Control x (SchedulerClient, 3);
//static Control y (ProcessorClient, 3);
//
//void runControls1()
//{
//    xmutex.lock();
//    x.write(0, 100);
//    x.write(1, 200);
//    x.write(2, 300);
//    xmutex.unlock();
//
//    sleepMillis(1);
//    
//    xmutex.lock();
//    x.write(2, 300);
//    x.write(2, 300);
//    x.write(2, 300);
//    xmutex.unlock();
//    
//    xmutex.lock();
//    cout << "x:" << x.read(0).front().getInt() << "\n";
//    cout << "x:" << x.read(1).front().getInt() << "\n";
//    cout << "x:" << x.read(2).front().getInt() << "\n";
//    xmutex.unlock();
//}
//void runControls2()
//{
//    ymutex.lock();
//    y.write(0, 11);
//    sleepMillis(0.2),
//    y.write(1, 22);
//    ymutex.unlock();
//
//    xmutex.lock();
//    y >> x;
//    xmutex.unlock();
//
//    ymutex.lock();
//    sleepMillis(1),
//    y.write(2, 33);
//    ymutex.unlock();
//    
//    xmutex.lock();
//    x << y;
//    xmutex.unlock();
//    
//    xmutex.lock();
//    cout << "y:" << y.read(0).front().getInt() << "\n";
//    cout << "y:" << y.read(1).front().getInt() << "\n";
//    cout << "y:" << y.read(2).front().getInt() << "\n";
//    xmutex.unlock();
//}
//
//void testControlThreads()
//{
//    std::cout << "========\n";
//    std::cout << "Testing control in threads\n";
//    boost::thread s(runControls1);
//    boost::thread t(runControls2);
//    s.join();
//    t.join();
//    
//    cout << "\n";
//}






