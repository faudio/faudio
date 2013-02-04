
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/string.h>
#include <doremir/util.h>

#include <unistd.h>
#include <sys/types.h>
#include <pwd.h>

doremir_string_file_path_t doremir_directory_home()
{
    struct passwd *pw = getpwuid(getuid());
    return string(pw->pw_dir);
}

doremir_string_file_path_t doremir_directory_current()
{
    char cwd[1024];
    getcwd(cwd, sizeof(cwd));
    return string(cwd);
}