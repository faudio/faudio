
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/string.h>
#include <fa/util.h>

#include <windows.h>
#include <shlobj.h>

fa_string_t fa_system_directory_home()
{
    /*
	Might have to be tested on Windows XP
    http://msdn.microsoft.com/en-us/library/windows/desktop/bb762181(v=vs.85).aspx
    */
    char path[MAX_PATH];
	if(FAILED(SHGetFolderPath(
		NULL,
		CSIDL_PROFILE,
		NULL,
		0,
		path)))
	{
		assert(FALSE && "Error get home directory");
	}
    return string(path);
}

fa_string_t fa_system_directory_current()
{
    char path[MAX_PATH];
	if(FAILED(GetCurrentDirectory(MAX_PATH, path))) {
		assert(FALSE && "Error get current directory")
	}
	return string(path);
}

void fa_system_directory_create(string_t path)
{
	/*
		Creates a directory with default permissions.
		For special permissions see:
		http://msdn.microsoft.com/en-us/library/windows/desktop/aa446595(v=vs.85).aspx
	*/
    if(FALSE == CreateDirectory(unstring(path),NULL)) 
	{
		id(ERROR_PATH_NOT_FOUND==GetLastError()) {
			assert(FALSE && "Intermediate directory not found");
		}
	}
}

fa_string_t fa_system_directory_read_file(fa_string_t path)
{
    DWORD buf_size = 1000000, bytes_read;
	char buf[buf_size+1];
	
	HANDLE file = CreateFile(
		unstring(path),
		GENERIC_READ,
		FILE_SHARE_READ,
		NULL,
		OPEN_EXISTING,
		FILE_ATTRIBUTE_NORMAL,
		NULL);
		
	if(file == INVALID_HANDLE_VALUE) {
		assert(FALSE && "Error opening file");
	}
	
	if(FALSE == ReadFile(file, buf, buf_size, &bytes_read, NULL)) {
		assert(FALSE && "Error reading file");
	}
	
	buf[bytes_read+1] = '\0';
	
	CloseHandle(file);
	
	return string(buf);
}


void fa_system_directory_write_file(fa_string_t path,
                                    fa_string_t string)
{
	/*
	http://msdn.microsoft.com/en-us/library/windows/desktop/aa365747(v=vs.85).aspx
	"lpNumberOfBytesWritten [out, optional]
	...
	This parameter can be NULL only when the lpOverlapped parameter is not NULL."
	*/
	
	DWORD bytes_written;
	
	HANDLE file = CreateFile(
		unstring(path),
		GENERIC_WRITE,
		FILE_SHARE_READ,
		NULL,
		CREATE_ALWAYS,
		FILE_ATTRIBUTE_NORMAL,
		NULL);
	
	if(file == INVALID_HANDLE_VALUE) {
		assert(FALSE && "Error opening file");
	}
	
	/*
	// CreateFile() will overwrite if the file at path already exists.
	// To prevent this use the CREATE_NEW flag and catch overwrites:
	if(ERROR_ALREADY_EXISTS == GetLastError()) {...}
	*/
	
	if(FALSE == WriteFile(
		file, 
		unstring(string), 
		strlen(unstring(string)),
		&bytes_written, // This is necessary: see comment at top.
		NULL))
	{
		assert(FALSE && "Error writing to file");
	}
	
	// FIXME error if srtlen(unstring(string)) != bytes_written ?

	CloseHandle(file);
}

void fa_system_directory_append_file(fa_string_t path,
                                     fa_string_t string)
{
	/*
	http://msdn.microsoft.com/en-us/library/windows/desktop/aa365747(v=vs.85).aspx
	"lpNumberOfBytesWritten [out, optional]
	...
	This parameter can be NULL only when the lpOverlapped parameter is not NULL."
	*/
	
	DWORD bytes_written;
	
	HANDLE file = CreateFile(
		unstring(path),
		FILE_APPEND_DATA,
		FILE_SHARE_READ,
		NULL,
		OPEN_ALWAYS,
		FILE_ATTRIBUTE_NORMAL,
		NULL);
	
	if(file == INVALID_HANDLE_VALUE) {
		assert(FALSE && "Error opening file");
	}
	
	if(FALSE == WriteFile(
		file,
		unstring(string),
		strlen(unstring(string)),
		&bytes_written, // This is necessary: see comment at top.
		NULL))
	{
		assert(FALSE && "Error appending to file");
	}
	
	// FIXME error if srtlen(unstring(string)) != bytes_written ?

	CloseHandle(file);
}


