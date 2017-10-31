# External library references in Faudio.framework
install_name_tool -change /Volumes/DoReMIR/faudio/external/liblo/source/../result/lib/liblo.7.dylib @executable_path/../Frameworks/liblo.7.dylib Frameworks/Faudio.framework/Faudio
install_name_tool -change /Volumes/DoReMIR/faudio/external/curl/source/../result/lib/libcurl.4.dylib @executable_path/../Frameworks/libcurl.4.dylib Frameworks/Faudio.framework/Faudio
install_name_tool -change /Volumes/DoReMIR/faudio/external/mpg123/source/../result/lib/libmpg123.0.dylib @executable_path/../Frameworks/libmpg123.0.dylib Frameworks/Faudio.framework/Faudio
install_name_tool -change /Volumes/DoReMIR/faudio/external/lame/source/../result/lib/libmp3lame.0.dylib @executable_path/../Frameworks/libmp3lame.0.dylib Frameworks/Faudio.framework/Faudio

# External library references in faudio-server
install_name_tool -change /Volumes/DoReMIR/faudio/external/liblo/source/../result/lib/liblo.7.dylib @executable_path/../Frameworks/liblo.7.dylib bin/faudio-server
install_name_tool -change /Volumes/DoReMIR/faudio/external/curl/source/../result/lib/libcurl.4.dylib @executable_path/../Frameworks/libcurl.4.dylib bin/faudio-server
install_name_tool -change /Volumes/DoReMIR/faudio/external/mpg123/source/../result/lib/libmpg123.0.dylib @executable_path/../Frameworks/libmpg123.0.dylib bin/faudio-server
install_name_tool -change /Volumes/DoReMIR/faudio/external/lame/source/../result/lib/libmp3lame.0.dylib @executable_path/../Frameworks/libmp3lame.0.dylib bin/faudio-server

# Reference to Faudio.framework in faudio-server
install_name_tool -change /Volumes/DoReMIR/faudio/build/Frameworks/Faudio.framework/Versions/2.23.2/Faudio @executable_path/../Frameworks/Faudio.framework/Versions/2.23.2/Faudio bin/faudio-server


# Other
install_name_tool -change /Volumes/DoReMIR/faudio/external/liblo/source/../result/lib/liblo.7.dylib @executable_path/../lib/liblo.7.dylib bin/faudio-io_simple_filter
install_name_tool -change /Volumes/DoReMIR/faudio/build/Frameworks/Faudio.framework/Versions/2.23.2/Faudio @executable_path/../lib/Faudio.framework/Versions/2.23.2/Faudio bin/faudio-io_simple_filter
