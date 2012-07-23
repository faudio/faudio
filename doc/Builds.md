

    #!/bin/bash
    git submodule init external_libraries/gtest;
    git submodule update external_libraries/gtest;

    pushd external_libraries/gtest;

    mkdir -p result \
    	&& pushd result \
    	&& cmake -G "MSYS Makefiles" -Dgtest_disable_pthreads=True .. \
    	&& make \
    	&& popd \
    	&& mkdir -p result/include \
    	&& mkdir -p result/lib \
    	&& mv result/libgtest.a result/lib \
    	&& cp -R include/ result/include;

    popd;

    #!/bin/bash

    pushd external_libraries/gtest;
    rm -rf result;
    popd;
    #!/bin/bash
    git submodule init external_libraries/gtest/source;
    git submodule update external_libraries/gtest/source;

    pushd external_libraries/gtest/source;

    mkdir -p build \
    	&& pushd build \
    	&& cmake .. \
    	&& make \
    	&& popd \
    	&& mkdir -p ../result/include \
    	&& mkdir -p ../result/lib \
    	&& mv build/libgtest.a ../result/lib \
    	&& cp -R include/      ../result/include;
    echo "Gtest result: $?";
    popd;

    #!/bin/bash

    pushd external_libraries/gtest;
    rm -rf result;
    popd;
    #!/bin/bash
    git submodule init   external_libraries/portaudio/source;
    git submodule update external_libraries/portaudio/source;

    pushd external_libraries/portaudio/source;

    ./configure \
    	  --with-winapi=wasapi,asio \
    	  --with-asiodir=../../asio/ASIOSDK2/ \
    	  --prefix=`pwd`/../result \
    	  && make \
    		&& make install \
    		&& cp -R include/ ../result/

    popd;                                                 
    #!/bin/bash

    pushd external_libraries/portaudio;
    rm -rf result;
    popd;
    #!/bin/bash
    git submodule init   external_libraries/portaudio/source;
    git submodule update external_libraries/portaudio/source;

    pushd external_libraries/portaudio/source;

    ./configure \
        --prefix=`pwd`/../result \
    	  && make \
    		&& make install \
    		&& cp -R include/ ../result/include/

    popd;
    #!/bin/bash

    pushd external_libraries/portaudio;
    rm -rf result;
    popd;
    #!/bin/bash
    git submodule init   external_libraries/portmidi/source;
    git submodule update external_libraries/portmidi/source;

    pushd external_libraries/portmidi/source;

    mkdir -p build \
      	&& pushd build \
      	&& cmake .. -G "MSYS Makefiles" \
      	    -DCMAKE_BUILD_TYPE=Release \
      	    -DCMAKE_ARCHIVE_OUTPUT_DIRECTORY="lib" \
      	    -DCMAKE_LIBRARY_OUTPUT_DIRECTORY="lib" \
      	    -DCMAKE_RUNTIME_OUTPUT_DIRECTORY="lib" \
      	    -DCMAKE_OSX_ARCHITECTURES="i386;x86_64" \
      	&& make \
      	&& popd \
      	&& mkdir -p ../result/include \
      	&& mkdir -p ../result/lib \
      	&& mv build/libportmidi.dll 	  ../result/lib \
      	&& mv build/libportmidi.dll.a   ../result/lib \
      	&& mv build/libportmidi_s.a  	  ../result/lib \
      	&& cp pm_common/portmidi.h      ../result/include/portmidi.h;

    popd;#!/bin/bash

    pushd external_libraries/portmidi;
    rm -rf result;
    popd;
    #!/bin/bash
    git submodule init   external_libraries/portmidi/source;
    git submodule update external_libraries/portmidi/source;

    pushd external_libraries/portmidi/source;

    mkdir -p build \
      	&& pushd build \
      	&& cmake .. \
      	    -DCMAKE_BUILD_TYPE=Release \
      	    -DCMAKE_ARCHIVE_OUTPUT_DIRECTORY="" \
      	    -DCMAKE_LIBRARY_OUTPUT_DIRECTORY="" \
      	    -DCMAKE_RUNTIME_OUTPUT_DIRECTORY="" \
      	    -DCMAKE_OSX_ARCHITECTURES="i386;x86_64" \
      	&& make \
      	&& popd \
      	&& mkdir -p ../result/include \
      	&& mkdir -p ../result/lib \
      	&& mv build/libportmidi.dylib 	../result/lib \
      	&& mv build/libportmidi_s.a  	  ../result/lib \
      	&& cp pm_common/portmidi.h      ../result/include/portmidi.h;

    popd;
    #!/bin/bash

    pushd external_libraries/portmidi;
    rm -rf result;
    popd;
    #!/bin/bash
    git submodule init   external_libraries/sndfile/source;
    git submodule update external_libraries/sndfile/source;

    pushd external_libraries/sndfile/source;

    CFLAGS="-arch i386 -I /Developer/SDKs/MacOSX10.7.sdk/Developer/Headers/FlatCarbon/" \
    CXXFLAGS="-arch i386" \
    LDFLAGS="-arch i386" \
    ./configure \
        --prefix=`pwd`/../result \
    		&& make install \
    		&& cp -R include/ result/include/

    popd;#!/bin/bash

    pushd external_libraries/sndfile;
    rm -rf result;
    popd;
