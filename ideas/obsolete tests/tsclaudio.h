/*
 *  tsclaudio.h
 *  sclaudio
 *
 *  Created by Hans Höglund on 2011-11-13.
 *  Copyright 2011 DoReMIR http://www.doremir.com/. All rights reserved.
 *
 */





void testListReturn()
{
//    cout << "========\n";
//    cout << "Testing list return\n";
//
//    int len;
//
//    int *ints = scl_test_return_list_int(&len);
//    for (int i = 0; i < len; ++i)
//        cout << ints[i] << "\n";
//    cout << "\n";
//    
//    SclString *strs = scl_test_return_list_string(&len);
//    for (int i = 0; i < len; ++i)
//        cout << strs[i] << "\n";
//    cout << "\n";
}

void testRecursiveListReturn()
{
//    cout << "========\n";
//    cout << "Testing recursive list return\n";
//
//    int len2;
//    int *len1;
//    int **ints = scl_test_return_list_list_int(&len1, &len2);
//    
//    cout << "size : " << len2 << "\n";
//    
//    for (int i = 0; i < len2; ++i)
//    {
//        cout << "  size : " << len1[i] << "\n";
//        for (int j = 0; j < len1[i]; ++j)
//        {
//            cout << ints[i][j] << " ";
//        }
//        cout << "\n";
//    }
//    cout << "\n";
}


void testListPass()
{
//    cout << "========\n";
//    cout << "Testing list pass\n";
//    
//    
//    int len2 = 3;
//    int *len1;
//    int **ints;
//    for(int i = 0; i < len2; ++i)
//    {
//        *ints = new int[len1[i]];
//        for(int j = 0; j < len1[i]; ++j)
//        {
//            ints[i][j] = i * 10 + j;
//        }
//    }
//    scl_test_pass_list_list_int(ints, len1, len2);
}

void testRecursiveListPass()
{
    cout << "========\n";
    cout << "Testing recursive list passing\n";
}