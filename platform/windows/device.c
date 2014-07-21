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

typedef fa_audio_status_callback_t audio_status_callback_t;
typedef fa_midi_status_callback_t  midi_status_callback_t;
typedef WCHAR HASH;

struct nullary_closure {
    fa_nullary_t   function;
    fa_ptr_t       data;
};
typedef struct nullary_closure *closure_t;

static
int     gInitializedOnce = 0;

static fa_thread_mutex_t   windows_device_mutex;

closure_t gMidiCallbackTable[1000];
fa_pair_t gAudioCallbackTable[1000];
long    gMidiCallbackTableCount;
long    gAudioCallbackTableCount;
int     mINumDevs;
int     mONumDevs;
int     wINumDevs;
int     wONumDevs;
HASH    audio_hash;
HASH    midi_hash;
WCHAR   rand_table[WCHAR_MAX];
HWND     hDummies[2];

static const char *WND_CLASS_MIDI_NAME  = "midiDummyWindow";
static const char *WND_CLASS_AUDIO_NAME = "audioDummyWindow";

#define kAudioDeviceType ((void*) 0)
#define kMidiDeviceType ((void*) 1)

/* This is the same as KSCATEGORY_AUDIO */
static const GUID GUID_AUDIO_DEVIFACE = {0x6994AD04L, 0x93EF, 0x11D0, {0xA3, 0xCC, 0x00, 0xA0, 0xC9, 0x22, 0x31, 0x96}};

// --------------------------------------------------------------------------------
void fa_device_initialize();
void fa_device_terminate();
void InitTable();
HASH BuildAudioHash();
HASH BuildMidiHash();
bool CheckAudioHash();
bool CheckMidiHash();
bool RegisterDeviceInterfaceToHwnd(HWND hwnd, HDEVNOTIFY *hDeviceNotify);
DWORD WINAPI window_thread(LPVOID params);
DWORD WINAPI check_thread_midi(LPVOID params);
DWORD WINAPI check_thread_audio(LPVOID params);
void ScheduleMidiCheck();
void ScheduleAudioCheck();
INT_PTR WINAPI midi_hardware_status_callback(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam);
INT_PTR WINAPI audio_hardware_status_callback(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam);
void add_audio_status_listener(fa_pair_t closure);
void remove_audio_status_listener(fa_pair_t closure);
void add_midi_status_listener(midi_status_callback_t function, fa_ptr_t data);

void fa_device_initialize2();

void fa_device_initialize()
{
    if (gInitializedOnce == 0) {
        gInitializedOnce++;
        fa_device_initialize2();
    }
}

void fa_device_initialize2()
{
    InitTable();

    wINumDevs = waveInGetNumDevs();
    wONumDevs = waveOutGetNumDevs();

    audio_hash  = BuildAudioHash();

    mINumDevs = midiInGetNumDevs();
    mONumDevs = midiOutGetNumDevs();

    midi_hash = BuildMidiHash();

    gMidiCallbackTableCount  = 0;
    gAudioCallbackTableCount = 0;

    {
        HANDLE wt = CreateThread(NULL, 0, window_thread, kAudioDeviceType, 0, 0);
        CloseHandle(wt);
    }

    {
        HANDLE wt = CreateThread(NULL, 0, window_thread, kMidiDeviceType, 0, 0);
        CloseHandle(wt);
    }

    windows_device_mutex = fa_thread_create_mutex();
}

void fa_device_terminate()
{
    // TODO unregister and stop threads
    // fa_thread_destroy_mutex(windows_device_mutex);
}

// --------------------------------------------------------------------------------


void InitTable()
{
    static bool init = false;

    if (!init) {
        for (int i = 0; i < WCHAR_MAX; i++) {
            rand_table[i] = ((rand() & 0xFFFF) | (rand() & 0xFFFF) << 16) & WCHAR_MAX - 1;
        }

        init = true;
    }
}

HASH BuildAudioHash()
{
    /*
    Fast hashing algorithm:
    http://cs.mwsu.edu/~griffin/courses/2133/downloads/Spring11/p677-pearson.pdf
    */
    HASH h = 0;
    WAVEINCAPS  wic;
    WAVEOUTCAPS woc;

    for (int j = 0; j < waveInGetNumDevs(); j++) {
        waveInGetDevCaps(j, &wic, sizeof(WAVEINCAPS));

        for (int i = 0; i < strlen(wic.szPname); i++) {
            h = rand_table[h ^ wic.szPname[i]];
        }
    }

    for (int j = 0; j < waveOutGetNumDevs(); j++) {
        waveOutGetDevCaps(j, &woc, sizeof(WAVEOUTCAPS));

        for (int i = 0; i < strlen(woc.szPname); i++) {
            h = rand_table[h ^ woc.szPname[i]];
        }
    }

    return h;
}

HASH BuildMidiHash()
{
    /*
    Fast hashing algorithm:
    http://cs.mwsu.edu/~griffin/courses/2133/downloads/Spring11/p677-pearson.pdf
    */
    HASH h = 0;
    MIDIINCAPS  mic;
    MIDIOUTCAPS moc;

    for (int j = 0; j < midiInGetNumDevs(); j++) {
        midiInGetDevCaps(j, &mic, sizeof(MIDIINCAPS));

        for (int i = 0; i < strlen(mic.szPname); i++) {
            h = rand_table[h ^ mic.szPname[i]];
        }
    }

    for (int j = 0; j < midiOutGetNumDevs(); j++) {
        midiOutGetDevCaps(j, &moc, sizeof(MIDIOUTCAPS));

        for (int i = 0; i < strlen(moc.szPname); i++) {
            h = rand_table[h ^ moc.szPname[i]];
        }
    }

    return h;
}

bool CheckAudioHash()
{
    return (audio_hash == BuildAudioHash());
}

bool CheckMidiHash()
{
    return (midi_hash == BuildMidiHash());
}

bool RegisterDeviceInterfaceToHwnd(HWND hwnd, HDEVNOTIFY *hDeviceNotify)
{

    DEV_BROADCAST_DEVICEINTERFACE NotificationFilter;

    ZeroMemory(&NotificationFilter, sizeof(NotificationFilter));

    NotificationFilter.dbcc_size = sizeof(DEV_BROADCAST_DEVICEINTERFACE);
    NotificationFilter.dbcc_devicetype = DBT_DEVTYP_DEVICEINTERFACE;
    NotificationFilter.dbcc_classguid = GUID_AUDIO_DEVIFACE;

    HDEVNOTIFY hDevNotify = RegisterDeviceNotification(hwnd, &NotificationFilter, DEVICE_NOTIFY_WINDOW_HANDLE);

    if (!hDevNotify) {
        return false;
    }

    return true;
}

DWORD WINAPI check_thread_midi(LPVOID _)
{
    /*
    midiXXXGetNumDevs() / midiXXXGetDevCaps() does not update until after the
    WM_DEVICECHANGE message has been dispatched.

    This thread launches, waits a short while for device list to be updated
    and then checks for changes.

    It works but it's not pretty.
    */

    Sleep(500);

    if (mINumDevs != midiInGetNumDevs() || mONumDevs != midiOutGetNumDevs()) {
        fa_with_lock(windows_device_mutex) {
            for (int i = 0; i < gMidiCallbackTableCount; ++i) {
                closure_t tp = gMidiCallbackTable[i];

                if (tp) {
                    tp->function(tp->data);
                }
            }
        }

        mINumDevs = midiInGetNumDevs();
        mONumDevs = midiOutGetNumDevs();
        midi_hash = BuildMidiHash();
    } else if (!CheckMidiHash()) {
        fa_with_lock(windows_device_mutex) {
            for (int i = 0; i < gMidiCallbackTableCount; ++i) {
                closure_t tp = gMidiCallbackTable[i];

                if (tp) {
                    tp->function(tp->data);
                }
            }
        }

        midi_hash = BuildMidiHash();
    }

    return 0;
}

DWORD WINAPI check_thread_audio(LPVOID _)
{
    /*
    waveXXXGetNumDevs() / waveXXXGetDevCaps() does not update until after the
    WM_DEVICECHANGE message has been dispatched.

    This thread launches, waits a short while for device list to be updated
    and then checks for changes.
    */
    Sleep(500);
    inform(string("  check_thread_audio"));

    if (wINumDevs != waveInGetNumDevs() || wONumDevs != waveOutGetNumDevs()) {
        inform(string("  Change (number)"));

        fa_with_lock(windows_device_mutex) {
            for (int i = 0; i < gAudioCallbackTableCount; ++i) {
                fa_pair_t tp = gAudioCallbackTable[i];

                if (tp) {
                    fa_unpair(tp, function, data) {
                        fa_nullary_t function2 = function;
                        function2(data);
                    }
                }
            }
        }

        wINumDevs  = waveInGetNumDevs();
        wONumDevs  = waveOutGetNumDevs();
        audio_hash = BuildAudioHash();
    } else if (!CheckAudioHash()) {
        inform(string("  Change (hash)"));

        fa_with_lock(windows_device_mutex) {
            for (int i = 0; i < gAudioCallbackTableCount; ++i) {
                fa_pair_t tp = gAudioCallbackTable[i];

                if (tp) {
                    fa_unpair(tp, function, data) {
                        fa_nullary_t function2 = function;
                        function2(data);
                    }
                }
            }
        }

        audio_hash = BuildAudioHash();
    }

    return 0;
}

void ScheduleMidiCheck()
{
    CloseHandle(CreateThread(NULL, 0, check_thread_midi, (LPVOID) NULL, 0, 0));
}

void ScheduleAudioCheck()
{
    inform(string("ScheduleAudioCheck"));
    CloseHandle(CreateThread(NULL, 0, check_thread_audio, (LPVOID) NULL, 0, 0));
}

INT_PTR WINAPI audio_hardware_status_callback(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
    LRESULT lRet = 0;
    PDEV_BROADCAST_HDR pbdi;
    PDEV_BROADCAST_DEVICEINTERFACE pdi;
    static HDEVNOTIFY hDeviceNotify;

    inform(string("Call to status callback"));

    switch (msg) {
    case WM_CREATE:
        if (!RegisterDeviceInterfaceToHwnd(hwnd, &hDeviceNotify)) {
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

    case WM_DEVICECHANGE: {
        switch (wParam) {
        case DBT_DEVICEARRIVAL:
            pbdi = (PDEV_BROADCAST_HDR)lParam;

            if (pbdi->dbch_devicetype != DBT_DEVTYP_DEVICEINTERFACE) {
                break;
            }

            pdi = (PDEV_BROADCAST_DEVICEINTERFACE) pbdi;

            if (!IsEqualGUID(&pdi->dbcc_classguid, &GUID_AUDIO_DEVIFACE)) {
                break;
            }

            /*
            Device check needs to be scheduled
            See comment in check_thread_audio/midi
            */
            ScheduleAudioCheck();

            break;

        case DBT_DEVICEREMOVECOMPLETE:
            pbdi = (PDEV_BROADCAST_HDR)lParam;

            if (pbdi->dbch_devicetype != DBT_DEVTYP_DEVICEINTERFACE) {
                break;
            }

            pdi = (PDEV_BROADCAST_DEVICEINTERFACE) pbdi;

            if (!IsEqualGUID(&pdi->dbcc_classguid, &GUID_AUDIO_DEVIFACE)) {
                break;
            }

            /*
            Device check needs to be scheduled
            See comment in check_thread_audio/midi
            */
            ScheduleAudioCheck();

            break;
        }

        break;
    }

    default:
        return DefWindowProc(hwnd, msg, wParam, lParam);
    }

    return lRet;
}

INT_PTR WINAPI midi_hardware_status_callback(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
    LRESULT lRet = 0;
    PDEV_BROADCAST_HDR pbdi;
    PDEV_BROADCAST_DEVICEINTERFACE pdi;
    static HDEVNOTIFY hDeviceNotify;

    switch (msg) {
    case WM_CREATE:
        if (!RegisterDeviceInterfaceToHwnd(hwnd, &hDeviceNotify)) {
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

    case WM_DEVICECHANGE: {
        switch (wParam) {
        case DBT_DEVICEARRIVAL:

            pbdi = (PDEV_BROADCAST_HDR)lParam;

            if (pbdi->dbch_devicetype != DBT_DEVTYP_DEVICEINTERFACE) {
                break;
            }

            pdi = (PDEV_BROADCAST_DEVICEINTERFACE) pbdi;

            if (!IsEqualGUID(&pdi->dbcc_classguid, &GUID_AUDIO_DEVIFACE)) {
                break;
            }

            /*
            Device check needs to be scheduled
            See comment in check_thread_audio/midi
            */
            ScheduleMidiCheck();

            break;

        case DBT_DEVICEREMOVECOMPLETE:
            pbdi = (PDEV_BROADCAST_HDR)lParam;

            if (pbdi->dbch_devicetype != DBT_DEVTYP_DEVICEINTERFACE) {
                break;
            }

            pdi = (PDEV_BROADCAST_DEVICEINTERFACE) pbdi;

            if (!IsEqualGUID(&pdi->dbcc_classguid, &GUID_AUDIO_DEVIFACE)) {
                break;
            }

            ScheduleMidiCheck();

            break;
        }

        break;
    }

    default:
        return DefWindowProc(hwnd, msg, wParam, lParam);
    }

    return lRet;
}

DWORD WINAPI window_thread(LPVOID params)
{
    /*
    WM_DEVICECHANGE messages are only sent to windows and services.
    A dummy receiving window is therefore created in hidden mode.
    */

    WNDCLASSEX wndClass = {0};
    wndClass.cbSize = sizeof(WNDCLASSEX);
    wndClass.hInstance = (HINSTANCE) GetModuleHandle(NULL);

    if (params == kAudioDeviceType) {
        wndClass.lpfnWndProc = (WNDPROC) midi_hardware_status_callback;
        wndClass.lpszClassName = WND_CLASS_MIDI_NAME;
        assert(RegisterClassEx(&wndClass) && "error registering dummy window");
        hDummies[0] = CreateWindow(WND_CLASS_MIDI_NAME, "midi window", WS_ICONIC,
                                   0, 0, CW_USEDEFAULT, 0, NULL, NULL, wndClass.hInstance, NULL);
        assert((hDummies[0] != NULL) && "failed to create window");
        ShowWindow(hDummies[0], SW_HIDE);
    } else if (params == kMidiDeviceType) {
        wndClass.lpfnWndProc = (WNDPROC) audio_hardware_status_callback;
        wndClass.lpszClassName = WND_CLASS_AUDIO_NAME;
        assert(RegisterClassEx(&wndClass) && "error registering dummy window");
        hDummies[1] = CreateWindow(WND_CLASS_AUDIO_NAME, "audio window", WS_ICONIC,
                                   0, 0, CW_USEDEFAULT, 0, NULL, NULL, wndClass.hInstance, NULL);
        assert((hDummies[1] != NULL) && "failed to create window");
        ShowWindow(hDummies[1], SW_HIDE);
    }

    MSG msg;

    while (GetMessage(&msg, NULL, 0, 0) != 0) {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }

    return 0;
}



void add_audio_status_listener(fa_pair_t closure)
{
    // Save params in global array
    fa_with_lock(windows_device_mutex) {
        gAudioCallbackTable[gAudioCallbackTableCount++] = closure;
    }
}

void add_midi_status_listener(midi_status_callback_t function, fa_ptr_t data)
{
    closure_t closure = malloc(sizeof(struct nullary_closure));
    closure->function = function;
    closure->data     = data;

    fa_with_lock(windows_device_mutex) {
        // Save params in global array
        gMidiCallbackTable[gMidiCallbackTableCount++] = closure;
    }
}

// TODO remove user callbacks
void remove_audio_status_listener(fa_pair_t closure)
{
    fa_with_lock(windows_device_mutex) {
        for (int i = 0; i < gAudioCallbackTableCount; i++) {
            if (gAudioCallbackTable[i] == closure) {
                gAudioCallbackTable[i] = NULL;
            }
        }
    }
}

void remove_midi_status_listener(midi_status_callback_t function)
{
    fa_with_lock(windows_device_mutex) {
        for (int i = 0; i < gMidiCallbackTableCount; i++) {
            if (gMidiCallbackTable[i] &&
                    gMidiCallbackTable[i]->function == function) {
                free(gMidiCallbackTable[i]);
            }
        }
    }
}

