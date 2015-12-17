install_name_tool -change /Volumes/DoReMIR/faudio/external/liblo/source/../result/lib/liblo.7.dylib @executable_path/../Frameworks/liblo.7.dylib build/Frameworks/Faudio.framework/Versions/2.16.0/Faudio
install_name_tool -change /Volumes/DoReMIR/faudio/external/liblo/source/../result/lib/liblo.7.dylib @executable_path/../Frameworks/liblo.7.dylib build/bin/faudio-server
install_name_tool -change /Volumes/DoReMIR/faudio/build/Frameworks/Faudio.framework/Versions/2.16.0/Faudio @executable_path/../Frameworks/Faudio.framework/Versions/2.16.0/Faudio build/bin/faudio-server
