/*
 *  core.h
 *  sclaudio
 *
 *  Created by Hans Höglund on 2011-11-03.
 *  Copyright 2011 DoReMIR http://www.doremir.com/. All rights reserved.
 *
 */

#include "math.h"


int add1(int x)
{
    return x + 1;
}

int add10(int x)
{
    return x + 10;
}


void fillWithChars(Signal s)
{
    SampleIterator i = s.begin(); int j = 0;
    for(; i != s.end(); ++i, ++j)
        *i = (j  % 26) + 97;
}
void fillWithFloats(Signal s)
{
    SampleIterator i = s.begin(); int j = 0;
    for(; i != s.end(); ++i, ++j)
        *i = cos(1/44100);
}

void fill(Signal s)
{
    fillWithChars(s);
}


void printSamples(Signal s)
{
    SampleIterator k = s.begin();
    for(; k != s.end(); ++k)
        cout << *k << " ";
    cout << "\n";
}


void testSignalPerformance()
{
    std::cout << "========\n";
    std::cout << "Testing signal performance\n";

    double start, end; 
#define START start = (float)clock()/CLOCKS_PER_SEC
#define STOP end = (float)clock()/CLOCKS_PER_SEC
#define PRINT cout << end - start << " seconds\n"
#define ITER 100

    Signal s (SampleDimensions(2, 1, 44100));
    Signal t (SampleDimensions(2, 1, 44100));
    

    cout << "Iterating over stereo buffer " << ITER << " times\n";    
    START;
    for (int i = 0; i < ITER; ++i)
        for(SampleIterator samp = s.begin(); samp != s.end(); ++samp);
    STOP;
    PRINT;

    cout << "Copying stereo buffer " << ITER << " times\n";    
    START;
    for (int i = 0; i < ITER; ++i)
        stl::copy(s.begin(), s.end(), t.begin());
    STOP;
    PRINT;

//    cout << "Rotating stereo buffer\n";    
//    START;
//    for (int i = 0; i < 100; ++i)
//        rotate(s.begin(), s.end(), s.end());
//    STOP;
//    PRINT;


}



void testSignalMap()
{
    std::cout << "========\n";
    std::cout << "Testing signal mapping\n";
    
    Signal s (SampleDimensions(2, 2, 10));
    Signal t (SampleDimensions(2, 2, 10));
    
    fill(s);

//    copy_(s.begin(), s.end(), t.begin());
    transform(s.begin(), s.end(), t.begin(), add1);
  
    printSamples(s);
    printSamples(t);

}

void testSignalOperations()
{
    Signal s (SampleDimensions(10, 10, 10));
    Signal t (SampleDimensions(10, 10, 10));
    fill(s);


}


void testSignal()
{
    std::cout << "========\n";
    std::cout << "Testing signals\n";


    Signal s (SampleDimensions(2, 2, 4));

    Signal t = s;

    fill(s);
    printSamples(s);
    printSamples(t);
    
}