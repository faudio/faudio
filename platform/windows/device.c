#include <fa/audio.h>
#include <fa/midi.h>
#include <fa/string.h>
#include <fa/thread.h>
#include <fa/util.h>

#define _WIN32_WINNT 0x0500
#define _WIN32_WINDOWS 0x0500
#define WINVER 0x0500
#include <windows.h>
#include <dbt.h>
#include <MMSystem.h>
#include <Ks.h>
#include <Ksmedia.h>

#define WM_PARAMSG WM_USER + 1

typedef fa_audio_status_callback_t audio_status_callback_t;
typedef fa_midi_status_callback_t  midi_status_callback_t;
typedef WCHAR HASH;

typedef struct _thread_params {
	HWND* 		 phwnd;
	int			 cb_type;
	fa_nullary_t function;
	fa_ptr_t	 data;
} thread_params, *pthread_params;

enum callback_type {
	MIDI_STATUS_CALLBACK,
	AUDIO_STATUS_CALLBACK,
	NULL_STATUS_CALLBACK
};

static const char* WND_CLASS_NAME 	= "dummyWindow";
static const GUID GUID_AUDIO_DEVIFACE = {0x6994AD04L,0x93EF,0x11D0, {0xA3,0xCC,0x00,0xA0,0xC9,0x22,0x31,0x96}};
/*static const GUID GUID_USB_DEVIFACE = {0xA5DCBF10L, 0x6530, 0x11D2, {0x90, 0x1F, 0x00, 0xC0, 0x4F, 0xB9, 0x51, 0xED}};*/

int mINumDevs;
int mONumDevs;
int wINumDevs;
int wONumDevs;

HASH	audio_hash;
HASH	midi_hash;
WCHAR rand_table[WCHAR_MAX];

void InitTable() {
	static bool init = false;

	if(!init) {
		for(int i=0;i<WCHAR_MAX;i++)
			rand_table[i] = ((rand()&0xFFFF)|(rand()&0xFFFF)<<16) & WCHAR_MAX-1;
		init = true;
	}
}

HASH BuildAudioHash() {
	HASH h = 0;
	WAVEINCAPS	wic;
	WAVEOUTCAPS woc;

	for(int j=0;j<waveInGetNumDevs();j++) {
		waveInGetDevCaps(j, &wic, sizeof(WAVEINCAPS));
		for(int i=0;i<strlen(wic.szPname);i++)
			h = rand_table[h ^ wic.szPname[i]];
	}

	for(int j=0;j<waveOutGetNumDevs();j++) {
		waveOutGetDevCaps(j, &woc, sizeof(WAVEOUTCAPS));
		for(int i=0;i<strlen(woc.szPname);i++)
			h = rand_table[h ^ woc.szPname[i]];
	}
	return h;
}

HASH BuildMidiHash() {
	HASH h = 0;
	MIDIINCAPS	mic;
	MIDIOUTCAPS moc;

	for(int j=0;j<midiInGetNumDevs();j++) {
		midiInGetDevCaps(j, &mic, sizeof(MIDIINCAPS));
		for(int i=0;i<strlen(mic.szPname);i++)
			h = rand_table[h ^ mic.szPname[i]];
	}

	for(int j=0;j<midiOutGetNumDevs();j++) {
		midiOutGetDevCaps(j, &moc, sizeof(MIDIOUTCAPS));
		for(int i=0;i<strlen(moc.szPname);i++)
			h = rand_table[h ^ moc.szPname[i]];
	}
	return h;
}

bool CheckAudioHash() {
	printf("%p == %p\n",audio_hash, BuildAudioHash());
	return(audio_hash == BuildAudioHash());
}

bool CheckMidiHash() {
	return(midi_hash == BuildMidiHash());
}

bool RegisterDeviceInterfaceToHwnd(HWND hwnd, HDEVNOTIFY *hDeviceNotify) {
	printf("LINE: %d\n",__LINE__);

	DEV_BROADCAST_DEVICEINTERFACE NotificationFilter;

	ZeroMemory(&NotificationFilter, sizeof(NotificationFilter));

	NotificationFilter.dbcc_size = sizeof(DEV_BROADCAST_DEVICEINTERFACE);
	NotificationFilter.dbcc_devicetype = DBT_DEVTYP_DEVICEINTERFACE;
	NotificationFilter.dbcc_classguid = GUID_AUDIO_DEVIFACE;

	HDEVNOTIFY hDevNotify = RegisterDeviceNotification(hwnd,
		&NotificationFilter, DEVICE_NOTIFY_WINDOW_HANDLE);

	if(!hDevNotify) {
	    return false;
	}
	return true;
}

DWORD WINAPI check_thread_midi(LPVOID params) {
	printf("LINE: %d\n",__LINE__);
	pthread_params tp = params;

	Sleep(500);

	if(mINumDevs!=midiInGetNumDevs() || mONumDevs!=midiOutGetNumDevs()) {
		tp->function(tp->data);
		mINumDevs = midiInGetNumDevs();
		mONumDevs = midiOutGetNumDevs();
		midi_hash = BuildMidiHash();
	} else if(!CheckMidiHash()) {
		tp->function(tp->data);
		midi_hash = BuildMidiHash();
	}

	return 0;
}

DWORD WINAPI check_thread_audio(LPVOID params) {
	pthread_params tp = params;

	Sleep(500);

	if(wINumDevs!=waveInGetNumDevs() || wONumDevs!=waveOutGetNumDevs()) {
		tp->function(tp->data);
		wINumDevs  = waveInGetNumDevs();
		wONumDevs  = waveOutGetNumDevs();
		audio_hash = BuildAudioHash();
	} else if(!CheckAudioHash()) {
		tp->function(tp->data);
		audio_hash = BuildAudioHash();
	}

	return 0;
}

void ScheduleMidiCheck(pthread_params ptp) {
	printf("LINE: %d\n",__LINE__);
	CloseHandle(CreateThread(NULL,0,check_thread_midi,ptp,0,0));
}

void ScheduleAudioCheck(pthread_params ptp) {
	CloseHandle(CreateThread(NULL,0,check_thread_audio,ptp,0,0));
}

INT_PTR WINAPI hardware_status_callback(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {
	LRESULT lRet = 0;
	PDEV_BROADCAST_HDR pbdi;
	PDEV_BROADCAST_DEVICEINTERFACE pdi;
	pthread_params ptparams = malloc(sizeof(thread_params));
	static HDEVNOTIFY hDeviceNotify;

	if(hwnd == NULL) {
        printf("\n\nERROR HWND == NULL\n\n");
	}

	if(msg == WM_PARAMSG) {
		printf("LINE: %d\n",__LINE__);
		ptparams = (pthread_params) lParam;
	}

	printf("LINE: %d tp: %p\n",__LINE__,ptparams);

	switch(msg) {
	case WM_CREATE:
		printf("LINE: %d\n",__LINE__);
		if (!RegisterDeviceInterfaceToHwnd(hwnd, &hDeviceNotify)) {
            assert(false && "failed to register device interface");
        }
		break;
	case WM_CLOSE:
		printf("LINE: %d\n",__LINE__);
		if (!UnregisterDeviceNotification(hDeviceNotify)) {
           assert(false && "failed to unregister device interface");
        }
		DestroyWindow(hwnd);
		break;
	case WM_DESTROY:
		printf("LINE: %d\n",__LINE__);
		PostQuitMessage(0);
		break;
	case WM_KEYDOWN:	// FIXME
		printf("LINE: %d\n",__LINE__);
		if(wParam == VK_ESCAPE) {
			PostQuitMessage(0);
		}
		break;
	case WM_DEVICECHANGE:
	{
		printf("LINE: %d\n",__LINE__);
		switch(wParam) {
		case DBT_DEVICEARRIVAL:
			if(ptparams == NULL)
				break;

			pbdi = (PDEV_BROADCAST_HDR)lParam;

			if(pbdi->dbch_devicetype != DBT_DEVTYP_DEVICEINTERFACE)
				break;

			pdi = (PDEV_BROADCAST_DEVICEINTERFACE) pbdi;

			if(!IsEqualGUID(&pdi->dbcc_classguid, &GUID_AUDIO_DEVIFACE))
				break;

			switch(ptparams->cb_type) {
			case AUDIO_STATUS_CALLBACK:
				ScheduleAudioCheck(ptparams);
				break;
			case MIDI_STATUS_CALLBACK:
				ScheduleMidiCheck(ptparams);
				break;
			}
			break;
		case DBT_DEVICEREMOVECOMPLETE:
			if(ptparams==NULL)
				break;

			pbdi = (PDEV_BROADCAST_HDR)lParam;

			if(pbdi->dbch_devicetype != DBT_DEVTYP_DEVICEINTERFACE)
				break;

			pdi = (PDEV_BROADCAST_DEVICEINTERFACE) pbdi;

			if(!IsEqualGUID(&pdi->dbcc_classguid, &GUID_AUDIO_DEVIFACE))
				break;

			switch(ptparams->cb_type) {
			case AUDIO_STATUS_CALLBACK:
				ScheduleAudioCheck(ptparams);
				break;
			case MIDI_STATUS_CALLBACK:
				ScheduleMidiCheck(ptparams);
				break;
			}
			break;
		}
		break;
	}
	default:
		printf("LINE: %d %p %p %p\n",__LINE__,msg,wParam,lParam);
		if(ptparams) free(ptparams);
        return DefWindowProc(hwnd, msg, wParam, lParam);
	}
	if(ptparams)
        free(ptparams);
	return lRet;
}

DWORD WINAPI window_thread(LPVOID params) {
	/*
	WM_DEVICECHANGE messages are sent to windows and services.
	A dummy receiving window is therefor created in hidden mode.
	*/

	printf("LINE: %d\n",__LINE__);

	pthread_params ptparams = params;

	WNDCLASSEX wndClass = {};
	wndClass.cbSize = sizeof(WNDCLASSEX);
	wndClass.hInstance = (HINSTANCE) GetModuleHandle(NULL);
	wndClass.lpfnWndProc = (WNDPROC) hardware_status_callback;
	wndClass.lpszClassName = WND_CLASS_NAME;

	//assert(RegisterClassEx(&wndClass) && "error registering dummy window");

    if(!RegisterClassEx(&wndClass)) {
        printf("ERROR REGISTER CLASSEX");
        exit(1);
    }

	HINSTANCE hInstance = GetModuleHandle(NULL);

	*(ptparams->phwnd) = CreateWindow(WND_CLASS_NAME, "new window", WS_ICONIC, 0, 0, CW_USEDEFAULT, 0, NULL, NULL, hInstance, NULL);
	printf("LINE: %d\n",__LINE__);
	//assert((*(ptparams->phwnd) != NULL) && "failed to create window");
	if((*(ptparams->phwnd))==NULL) {
        printf("ERROR CREATING WINDOW");
        exit(1);
    }
	printf("LINE: %d\n",__LINE__);
	ShowWindow(*(ptparams->phwnd),SW_HIDE);

	SendMessage(*(ptparams->phwnd), WM_PARAMSG, (WPARAM) 0, (LPARAM) ptparams);

	MSG msg;
	while(GetMessage(&msg,NULL,0,0)!=0) {
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}

	return 0;
}

void add_audio_status_listener(audio_status_callback_t function, ptr_t data) {
	wINumDevs = waveInGetNumDevs();
	wONumDevs = waveOutGetNumDevs();

	InitTable();
	audio_hash	= BuildAudioHash();

	pthread_params params = malloc(sizeof(thread_params));
	params->cb_type	 = AUDIO_STATUS_CALLBACK;
	params->function = function;
	params->data	 = data;
	HANDLE wt = CreateThread(NULL,0,window_thread,params,0,0);
	CloseHandle(wt);
}

void add_midi_status_listener(midi_status_callback_t function, ptr_t data) {
    printf("LINE: %d\n",__LINE__);
    mINumDevs = midiInGetNumDevs();
	mONumDevs = midiOutGetNumDevs();

	InitTable();
	midi_hash = BuildMidiHash();

	pthread_params params = malloc(sizeof(thread_params));
	params->cb_type	 = MIDI_STATUS_CALLBACK;
	params->function = function;
	params->data	 = data;
	HANDLE wt = CreateThread(NULL,0,window_thread,params,0,0);
	CloseHandle(wt);
}
