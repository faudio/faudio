/*
 *  core.h
 *  sclaudio
 *
 *  Created by Hans Höglund on 2011-11-03.
 *  Copyright 2011 DoReMIR http://www.doremir.com/. All rights reserved.
 *
 */



void testErrors()
{

    void **err1 = new SclPortaudioError[1];
    void **err2 = new SclPortaudioError[1];
    void **err3 = new SclPortaudioError[1];
    
    *err1 = NULL;
    *err2 = NULL;
    *err3 = NULL;
    

    scl_open_device_stream(NULL, NULL, NULL, NULL, NULL, NULL, err1, err2, err3);

    string message;
//    if (*err1 != NULL) message = reinterpret_cast<PortaudioError*> (*err1)->message();
//    if (*err2 != NULL) message = reinterpret_cast<PortmidiError*> (*err2)->message();
//    if (*err3 != NULL) message = reinterpret_cast<DspError*> (*err3)->message();
//
    int code;
    if (*err1 != NULL) code = reinterpret_cast<PortaudioError*> (*err1)->errorCode();
    if (*err2 != NULL) code = reinterpret_cast<PortmidiError*>  (*err2)->errorCode();


}