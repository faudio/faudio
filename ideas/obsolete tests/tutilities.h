/*
 *  core.h
 *  sclaudio
 *
 *  Created by Hans Höglund on 2011-11-03.
 *  Copyright 2011 DoReMIR http://www.doremir.com/. All rights reserved.
 *
 */


void testString()
{
    std::cout << "========\n";
    std::cout << "Testing string utils\n";

    std::cout << "hello " << 22 << "\n";
}

void testList()
{
    std::cout << "========\n";
    std::cout << "Testing list utils\n";

    list<int> lst = listFrom(1, 2, 3, 4);
}