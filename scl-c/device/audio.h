

#ifdef __cplusplus
extern "C" {
#endif

typedef scl_audio_device_group_handle* scl_audio_device_group;
typedef scl_audio_device_handle*       scl_audio_device;

SCL_API scl_string scl_audio_host_name(scl_audio_device_group obj);
SCL_API int scl_audio_host_number_of_devices(scl_audio_device_group obj);
SCL_API scl_audio_device* scl_audio_host_devices(scl_audio_device_group obj, int* len);
SCL_API scl_audio_device_group* scl_audio_hosts(int* len, SclPortaudioError* err);
SCL_API scl_audio_device_group scl_default_audio_host(SclPortaudioError* err);

SCL_API scl_string scl_audio_device_name(scl_audio_device obj);
SCL_API scl_audio_device_group scl_audio_device_host(scl_audio_device obj);
SCL_API int scl_audio_device_max_input_channels(scl_audio_device obj);
SCL_API int scl_audio_device_max_output_channels(scl_audio_device obj);
SCL_API int scl_audio_device_default_low_input_latency(scl_audio_device obj);
SCL_API int scl_audio_device_default_high_input_latency(scl_audio_device obj);
SCL_API int scl_audio_device_default_low_output_latency(scl_audio_device obj);
SCL_API int scl_audio_device_default_high_output_latency(scl_audio_device obj);
SCL_API int scl_audio_device_default_sample_rate(scl_audio_device obj);

SCL_API scl_audio_device  scl_default_audio_input_device(SclPortaudioError* err);
SCL_API scl_audio_device  scl_default_audio_output_device(SclPortaudioError* err);


#ifdef __cplusplus
}
#endif

#endif
