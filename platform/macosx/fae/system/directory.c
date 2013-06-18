
/*
    FAE
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <fae/string.h>
#include <fae/util.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>

fae_string_file_path_t fae_system_directory_home()
{
    struct passwd *pw = getpwuid(getuid());
    return string(pw->pw_dir);
}

fae_string_file_path_t fae_system_directory_current()
{
    char cwd[1024];
    getcwd(cwd, sizeof(cwd));
    return string(cwd);
}

void fae_system_directory_create(string_t path)
{
    // Assure system_directory
    mkdir(unstring(path), (S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH));
}

fae_string_t fae_system_directory_read_file(fae_string_file_path_t path)
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


void fae_system_directory_write_file(fae_string_file_path_t path,
                                     fae_string_t string)
{
    FILE *f = fopen(unstring(path), "w+");
    fprintf(f, "%s", unstring(string));
    fclose(f);
}

void fae_system_directory_append_file(fae_string_file_path_t path,
                                      fae_string_t string)
{
    FILE *f = fopen(unstring(path), "a+");
    fprintf(f, "%s", unstring(string));
    fclose(f);
}


