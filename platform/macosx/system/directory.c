
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/string.h>
#include <fa/util.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>

fa_string_t fa_system_directory_home()
{
    struct passwd *pw = getpwuid(getuid());
    return fa_string_from_utf8(pw->pw_dir);
}

fa_string_t fa_system_directory_current()
{
    char cwd[1024];
    getcwd(cwd, sizeof(cwd));
    return fa_string_from_utf8(cwd);
}

void fa_system_directory_create(fa_string_t path)
{
    // Assure system_directory
    mkdir(fa_unstring(path), (S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH));
}

fa_string_t fa_system_directory_read_file(fa_string_t path)
{
    size_t buf_size = 1000000;
    char buf[buf_size + 1];

    FILE *file = fopen(fa_unstring(path), "r");

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

    return fa_string_from_utf8(buf);
}


void fa_system_directory_write_file(fa_string_t path,
                                    fa_string_t string)
{
    FILE *f = fopen(fa_unstring(path), "w+");
    fprintf(f, "%s", fa_unstring(string));
    fclose(f);
}

void fa_system_directory_append_file(fa_string_t path,
                                     fa_string_t string)
{
    FILE *f = fopen(fa_unstring(path), "a+");
    fprintf(f, "%s", fa_unstring(string));
    fclose(f);
}


