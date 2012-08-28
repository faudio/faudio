
#include "scl/string.h"
#include "scl/time.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef SclAudioDeviceGroup_handle* SclAudioDeviceGroup;
typedef SclAudioDevice_handle*       SclAudioDevice;

SCL_API SclString scl_audio_device_group_name(SclAudioDeviceGroup obj);
SCL_API int scl_audio_device_group_number_of_devices(SclAudioDeviceGroup obj);
SCL_API SclAudioDevice* scl_audio_device_group_devices(SclAudioDeviceGroup obj, int* len);
SCL_API SclAudioDeviceGroup* scl_audio_device_groups(int* len, SclPortaudioError* err);
SCL_API SclAudioDeviceGroup scl_portaudio_device_groups(SclPortaudioError* err);

SCL_API SclString scl_audio_device_name(SclAudioDevice obj);
SCL_API SclAudioDeviceGroup scl_audio_device_host(SclAudioDevice obj);
SCL_API int scl_audio_device_max_input_channels(SclAudioDevice obj);
SCL_API int scl_audio_device_max_output_channels(SclAudioDevice obj);
SCL_API int scl_audio_device_default_low_input_latency(SclAudioDevice obj);
SCL_API int scl_audio_device_default_high_input_latency(SclAudioDevice obj);
SCL_API int scl_audio_device_default_low_output_latency(SclAudioDevice obj);
SCL_API int scl_audio_device_default_high_output_latency(SclAudioDevice obj);
SCL_API int scl_audio_device_default_sample_rate(SclAudioDevice obj);


SCL_API SclAudioDevice  scl_default_audio_input_device(SclPortaudioError* err);
SCL_API SclAudioDevice  scl_default_audio_output_device(SclPortaudioError* err);

#ifdef __cplusplus
}
#endif

#endif
