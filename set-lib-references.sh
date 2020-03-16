# External library references in Faudio.framework
install_name_tool -change /Volumes/DoReMIR/faudio/external/liblo/source/../result/lib/liblo.7.dylib @executable_path/../Frameworks/liblo.7.dylib Frameworks/Faudio.framework/Faudio
install_name_tool -change /Volumes/DoReMIR/faudio/external/curl/source/../result/lib/libcurl.4.dylib @executable_path/../Frameworks/libcurl.4.dylib Frameworks/Faudio.framework/Faudio
install_name_tool -change /Volumes/source/curl-7.64.0/dist/lib/libcurl.4.dylib @executable_path/../Frameworks/libcurl.4.dylib Frameworks/Faudio.framework/Faudio
install_name_tool -change /Volumes/DoReMIR/faudio/external/mpg123/source/../result/lib/libmpg123.0.dylib @executable_path/../Frameworks/libmpg123.0.dylib Frameworks/Faudio.framework/Faudio
install_name_tool -change /Volumes/DoReMIR/faudio/external/lame/source/../result/lib/libmp3lame.0.dylib @executable_path/../Frameworks/libmp3lame.0.dylib Frameworks/Faudio.framework/Faudio
install_name_tool -change /Volumes/DoReMIR/faudio/external/fluidsynth/result/lib64/libfluidsynth.1.dylib @executable_path/../Frameworks/libfluidsynth.1.dylib Frameworks/Faudio.framework/Faudio
install_name_tool -change /usr/local/opt/glib/lib/libglib-2.0.0.dylib @executable_path/../Frameworks/libglib-2.0.0.dylib Frameworks/Faudio.framework/Faudio
install_name_tool -change /usr/local/opt/glib/lib/libgthread-2.0.0.dylib @executable_path/../Frameworks/libgthread-2.0.0.dylib Frameworks/Faudio.framework/Faudio
install_name_tool -change /usr/local/opt/gettext/lib/libintl.8.dylib @executable_path/../Frameworks/libintl.8.dylib Frameworks/Faudio.framework/Faudio
install_name_tool -change /usr/local/opt/pcre/lib/libpcre.1.dylib @executable_path/../Frameworks/libpcre.1.dylib Frameworks/Faudio.framework/Faudio

# Fluidsynth
install_name_tool -change /usr/local/opt/glib/lib/libglib-2.0.0.dylib @executable_path/../Frameworks/libglib-2.0.0.dylib Frameworks/libfluidsynth.1.dylib
install_name_tool -change /usr/local/opt/glib/lib/libgthread-2.0.0.dylib @executable_path/../Frameworks/libgthread-2.0.0.dylib Frameworks/libfluidsynth.1.dylib
install_name_tool -change /usr/local/opt/gettext/lib/libintl.8.dylib @executable_path/../Frameworks/libintl.8.dylib Frameworks/libfluidsynth.1.dylib


# External library references in faudio-server
install_name_tool -change /Volumes/DoReMIR/faudio/external/liblo/source/../result/lib/liblo.7.dylib @executable_path/../Frameworks/liblo.7.dylib bin/faudio-server
install_name_tool -change /Volumes/DoReMIR/faudio/external/curl/source/../result/lib/libcurl.4.dylib @executable_path/../Frameworks/libcurl.4.dylib bin/faudio-server
install_name_tool -change /Volumes/source/curl-7.64.0/dist/lib/libcurl.4.dylib @executable_path/../Frameworks/libcurl.4.dylib bin/faudio-server
install_name_tool -change /Volumes/DoReMIR/faudio/external/mpg123/source/../result/lib/libmpg123.0.dylib @executable_path/../Frameworks/libmpg123.0.dylib bin/faudio-server
install_name_tool -change /Volumes/DoReMIR/faudio/external/lame/source/../result/lib/libmp3lame.0.dylib @executable_path/../Frameworks/libmp3lame.0.dylib bin/faudio-server
install_name_tool -change /Volumes/DoReMIR/faudio/external/fluidsynth/result/lib64/libfluidsynth.1.dylib @executable_path/../Frameworks/libfluidsynth.1.dylib bin/faudio-server
install_name_tool -change /usr/local/opt/glib/lib/libglib-2.0.0.dylib @executable_path/../Frameworks/libglib-2.0.0.dylib bin/faudio-server
install_name_tool -change /usr/local/opt/glib/lib/libgthread-2.0.0.dylib @executable_path/../Frameworks/libgthread-2.0.0.dylib bin/faudio-server
install_name_tool -change /usr/local/opt/gettext/lib/libintl.8.dylib @executable_path/../Frameworks/libintl.8.dylib bin/faudio-server
install_name_tool -change /usr/local/opt/pcre/lib/libpcre.1.dylib @executable_path/../Frameworks/libpcre.1.dylib bin/faudio-server

# Reference to Faudio.framework in faudio-server
install_name_tool -change /Volumes/DoReMIR/faudio/build/Frameworks/Faudio.framework/Versions/2.25.5/Faudio @executable_path/../Frameworks/Faudio.framework/Versions/2.25.5/Faudio bin/faudio-server


# Other
#install_name_tool -change /Volumes/DoReMIR/faudio/external/liblo/source/../result/lib/liblo.7.dylib @executable_path/../lib/liblo.7.dylib bin/faudio-io_simple_filter
#install_name_tool -change /Volumes/DoReMIR/faudio/build/Frameworks/Faudio.framework/Versions/2.25.5/Faudio @executable_path/../lib/Faudio.framework/Versions/2.25.5/Faudio bin/faudio-io_simple_filter
