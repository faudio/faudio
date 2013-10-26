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

#define WM_PARAMSG WM_USER + 1

typedef fa_audio_status_callback_t audio_status_callback_t;
typedef fa_midi_status_callback_t  midi_status_callback_t;

static const char* WND_CLASS_NAME 	= "dummyWindow";
static const GUID GUID_USB_DEVIFACE = {0xA5DCBF10L, 0x6530, 0x11D2, {0x90, 0x1F, 0x00, 0xC0, 0x4F, 0xB9, 0x51, 0xED}};

typedef struct _thread_params {
	HWND* 		 phwnd;
	fa_nullary_t function;
	fa_ptr_t	 data;
} thread_params, *pthread_params;

BOOL RegisterDeviceInterfaceToHwnd(IN GUID InterfaceClassGuid, IN HWND hwnd, OUT HDEVNOTIFY *hDeviceNotify) {
	DEV_BROADCAST_DEVICEINTERFACE NotificationFilter;
	ZeroMemory( &NotificationFilter, sizeof(NotificationFilter) );
	NotificationFilter.dbcc_size = sizeof(DEV_BROADCAST_DEVICEINTERFACE);
	NotificationFilter.dbcc_devicetype = DBT_DEVTYP_DEVICEINTERFACE;

	NotificationFilter.dbcc_classguid = GUID_USB_DEVIFACE;
	HDEVNOTIFY hDevNotify = RegisterDeviceNotification(hwnd, &NotificationFilter, DEVICE_NOTIFY_WINDOW_HANDLE);
	if(!hDevNotify) {
	    return FALSE;
	}
	return TRUE;
}

INT_PTR WINAPI hardware_callback(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {
	LRESULT ret = 1;
	static HDEVNOTIFY hDeviceNotify;
	static pthread_params tparams = NULL;
	
	if(msg == WM_PARAMSG)
		tparams = (pthread_params) lParam;

	switch(msg) {
	case WM_CREATE:
		// REGISTER FOR HARDWARE NOTIFICATIONS
		if (!RegisterDeviceInterfaceToHwnd(GUID_USB_DEVIFACE, hwnd, &hDeviceNotify)) {
            assert(false && "failed to register device interface");
        }
		break;
	case WM_CLOSE:
		if (!UnregisterDeviceNotification(hDeviceNotify)) {
           assert(false && "failed to unregister device interface");
        }
		DestroyWindow(hwnd);
		break;
	case WM_DESTROY:
		PostQuitMessage(0);
		break;
	case WM_DEVICECHANGE: 
	{
		PDEV_BROADCAST_HDR pbdi = (PDEV_BROADCAST_HDR)lParam;
		switch(wParam) {
		case DBT_DEVICEARRIVAL:
			/*
				This code runs for all hardware changes but can and should be filtered
				using the pbdi->dbch_devicetype member.
				http://msdn.microsoft.com/en-us/library/windows/desktop/aa363246(v=vs.85).aspx
			*/
			if(tparams != NULL) {
				tparams->function(tparams->data);
			}
			break;
		case DBT_DEVICEREMOVECOMPLETE:
			if(tparams != NULL) {
				tparams->function(tparams->data);
			}
			break;
		}
		break;
	}
	default:
		ret = DefWindowProc(hwnd, msg, wParam, lParam);
        break;
	}
	return ret;
}

DWORD WINAPI window_thread(LPVOID params) {
	// WM_DEVICECHANGE messages are sent to windows and services.
	// A dummy receiving window is therefor created in hidden mode.
	
	pthread_params ptparams = params; 
	
	WNDCLASSEX wndClass = {0};
	wndClass.cbSize = sizeof(WNDCLASSEX);
	wndClass.hInstance = (HINSTANCE) GetModuleHandle(NULL);
	wndClass.lpfnWndProc = (WNDPROC) hardware_callback;
	wndClass.lpszClassName = WND_CLASS_NAME;

	assert(RegisterClassEx(&wndClass) && "error registering dummy window");
	/*
	if(!RegisterClassEx(&wndClass)) {
		errormsg("failed to register class");
		return 1;
	}
	*/

	HINSTANCE hInstance = GetModuleHandle(NULL);

	*(ptparams->phwnd) = CreateWindow(WND_CLASS_NAME, "new window", WS_ICONIC, 0, 0,
        CW_USEDEFAULT, 0, NULL, NULL, hInstance, NULL);

	assert((*(ptparams->phwnd) != NULL) && "failed to create window");
	/*
	if(*(ptparams->phwnd) == NULL) {
		errormsg("failed to create window");
		return 1;
	}
	*/

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
	pthread_params params = malloc(sizeof(thread_params));
	params->function = function;
	params->data	 = data;
	HANDLE wt = CreateThread(NULL,0,window_thread,params->phwnd,0,0);
	CloseHandle(wt);
}

void add_midi_status_listener(midi_status_callback_t function, ptr_t data) {
    assert(false && "implementation missing");
}
