
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/string.h>
#include <doremir/util.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>

doremir_string_file_path_t doremir_system_directory_home()
{
    struct passwd *pw = getpwuid(getuid());
    return string(pw->pw_dir);
}

doremir_string_file_path_t doremir_system_directory_current()
{
    char cwd[1024];
    getcwd(cwd, sizeof(cwd));
    return string(cwd);
}

void doremir_system_directory_create(string_t path)
{
    // Assure system_directory
    mkdir(unstring(path), (S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH));
}

doremir_string_t doremir_system_directory_read_file(doremir_string_file_path_t path)
{
    size_t buf_size = 1000000;
    char buf[buf_size + 1];

    FILE *file = fopen(unstring(path), "r");

    if (!file) {
        assert(false && "Error reading file");
    }

    size_t read_size = fread(buf, sizeof(char), buf_size, file);
    fclose(file);

    if (read_size == 0) {
        assert(false && "Error reading file");
    } else {
        buf[read_size + 1] = '\0';
    }

    return string(buf);
}


void doremir_system_directory_write_file(doremir_string_file_path_t path,
                                         doremir_string_t string)
{
    FILE *f = fopen(unstring(path), "w+");
    fprintf(f, "%s", unstring(string));
    fclose(f);
}

void doremir_system_directory_append_file(doremir_string_file_path_t path,
                                          doremir_string_t string)
{
    FILE *f = fopen(unstring(path), "a+");
    fprintf(f, "%s", unstring(string));
    fclose(f);
}


