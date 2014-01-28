
/*
    faudio

    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.

 */

#include <fa/io.h>
#include <fa/util.h>

struct filter_base {
    impl_t impl;
    ptr_t data1, data2, data3;
};


fa_io_filter_t fa_io_ref_filter(ptr_t r);

void fa_io_pull(fa_io_source_t source,
                fa_io_callback_t bufferCallback,
                fa_ptr_t ptr)
{
    fa_io_pull_through((fa_io_filter_t) source, NULL, bufferCallback, ptr);
}


void fa_io_push(fa_io_sink_t sink, fa_buffer_t buffer)
{
    fa_io_push_through((fa_io_filter_t) sink, NULL, buffer);
}

void fa_io_pull_through(fa_io_filter_t filter,
                        fa_io_source_t upstream,
                        fa_io_callback_t bufferCallback,
                        fa_ptr_t ptr)
{
    ((fa_io_filter_interface_t *) fa_interface(fa_io_filter_interface_i, filter))->pull(filter, upstream, bufferCallback, ptr);
}


void fa_io_push_through(fa_io_filter_t filter, fa_io_sink_t downstream, fa_buffer_t buffer)
{
    ((fa_io_filter_interface_t *) fa_interface(fa_io_filter_interface_i, filter))->push(filter, downstream, buffer);
}



#define FILTER_IMPLEMENTATION(T) \
    fa_ptr_t T##_impl(fa_id_t interface)                            \
    {                                                               \
        static fa_string_show_t T##_show_impl = { T##_show };       \
        static fa_io_filter_interface_t T##_io_filter_impl          \
            = { T##_pull, T##_push };                               \
                                                                    \
        switch (interface) {                                        \
        case fa_string_show_i: return &T##_show_impl;               \
        case fa_io_filter_interface_i: return &T##_io_filter_impl;  \
        default: return NULL;                                       \
        }                                                           \
    }


// ------------------------------------------------------------------------------------------

string_t  split_filter_show(ptr_t x)
{
    return string("<SplitFilter>");
}

static inline void _split_pull(fa_ptr_t x, fa_buffer_t buffer)
{
    fa_unpair(x, sink2, closure) {
        if (buffer) {
            fa_io_push(sink2, fa_copy(buffer));
            fa_unpair(closure, callback, data) {
                ((fa_io_callback_t) callback)(data, buffer);
            }
        } else {
            fa_io_push(sink2, NULL);
            fa_unpair(closure, callback, data) {
                ((fa_io_callback_t) callback)(data, NULL);
            }
        }
    }
}
void split_filter_pull(fa_ptr_t x, fa_io_source_t source, fa_io_callback_t callback, ptr_t data)
{
    fa_io_sink_t sink2 = ((struct filter_base *) x)->data1;
    fa_io_pull(source, _split_pull, pair(sink2, pair(callback, data)));
}
void split_filter_push(fa_ptr_t x, fa_io_sink_t sink, fa_buffer_t buffer)
{
    fa_io_sink_t sink2 = ((struct filter_base *) x)->data1;
    fa_io_push(sink2, buffer);
    fa_io_push(sink, buffer);
}
FILTER_IMPLEMENTATION(split_filter);


// ------------------------------------------------------------------------------------------

string_t  stdin_filter_show(ptr_t x)
{
    return string("<StdInSource>");
}

inline static
char *read_line(char *in)
{
    char *cptr;
    if ((cptr = fgets(in, 80, stdin))) {
        while (*cptr == ' ' || *cptr == '\t') {
            cptr++;
        }
        return cptr;
    } else {
        return 0;
    }
}

void stdin_filter_pull(fa_ptr_t _, fa_io_source_t upstream, fa_io_callback_t callback, ptr_t data)
{
    // int c = getchar();
    // if (c == EOF) {
    //     callback(data, NULL);
    // } else {
    //     callback(data, fa_buffer_wrap(&c, 1, NULL, NULL));
    // }
    char in[80];
    read_line(in);
    callback(data, fa_buffer_wrap(in, strlen(in), NULL, NULL));
}
void stdin_filter_push(fa_ptr_t _, fa_io_sink_t downstream, fa_buffer_t buffer)
{
}
FILTER_IMPLEMENTATION(stdin_filter);


// ------------------------------------------------------------------------------------------

string_t  standardout_filter_show(ptr_t x)
{
    return string("<StdOutSink>");
}

void standardout_filter_pull(fa_ptr_t _, fa_io_source_t upstream, fa_io_callback_t callback, ptr_t data)
{
}
void standardout_filter_push(fa_ptr_t _, fa_io_sink_t downstream, fa_buffer_t buffer)
{
    if (buffer) {
        // printf("\x1b[32m");

        for (int i = 0; i < fa_buffer_size(buffer); ++i) {
            int c = fa_buffer_get(buffer, i);
            putchar(c);
        }

        // printf("\x1b[0m");
    } else {
        // Can not close standardout
    }
}
FILTER_IMPLEMENTATION(standardout_filter);


// ------------------------------------------------------------------------------------------

string_t  write_filter_show(ptr_t x)
{
    return string("<StdOutSink>");
}

void write_filter_pull(fa_ptr_t _, fa_io_source_t upstream, fa_io_callback_t callback, ptr_t data)
{
}
void write_filter_push(fa_ptr_t x, fa_io_sink_t downstream, fa_buffer_t buffer)
{
    if (buffer) {
        string_t path = ((struct filter_base *) x)->data1;
        FILE *fp = fopen(unstring(path), "a");
        fwrite(fa_buffer_unsafe_address(buffer), fa_buffer_size(buffer), 1, fp);
        fclose(fp);
    } else {
        // TODO close
    }
}
FILTER_IMPLEMENTATION(write_filter);


// ------------------------------------------------------------------------------------------

string_t  read_filter_show(ptr_t x)
{
    return string("<StdOutSink>");
}

void read_filter_pull(fa_ptr_t x, fa_io_source_t upstream, fa_io_callback_t callback, ptr_t data)
{
    string_t path = ((struct filter_base *) x)->data1;
    FILE *fp = fopen(unstring(path), "r");

    char raw[1024*16];
    size_t read;

    while (!ferror(fp) && !feof(fp)) {
        read = fread(raw, 1, sizeof(raw), fp);
        callback(data, fa_copy(fa_buffer_wrap(raw, read, NULL, NULL)));
    }

    callback(data, NULL);

    fclose(fp);
}

void read_filter_push(fa_ptr_t _, fa_io_sink_t downstream, fa_buffer_t buffer)
{
}
FILTER_IMPLEMENTATION(read_filter);


// ------------------------------------------------------------------------------------------

string_t  ref_filter_show(ptr_t x)
{
    return string("<Ref>");
}

void ref_filter_pull(fa_ptr_t x, fa_io_source_t _, fa_io_callback_t callback, ptr_t data)
{
    buffer_t *r = ((struct filter_base *) x)->data1;
    callback(data, *r);
}
void ref_filter_push(fa_ptr_t x, fa_io_sink_t _, fa_buffer_t buffer)
{
    buffer_t *r = ((struct filter_base *) x)->data1;
    *r = buffer;
}
FILTER_IMPLEMENTATION(ref_filter);


// ------------------------------------------------------------------------------------------

string_t identity_show(ptr_t x)
{
    return string("<Ref>");
}

void identity_pull(fa_ptr_t x, fa_io_source_t source, fa_io_callback_t callback, ptr_t data)
{
    fa_io_pull(source, callback, data);
}
void identity_push(fa_ptr_t x, fa_io_sink_t downstream, fa_buffer_t buffer)
{
    fa_io_push(downstream, buffer);
}
FILTER_IMPLEMENTATION(identity);



// ------------------------------------------------------------------------------------------

string_t  composed_filter_show(ptr_t x)
{
    return string("<Composed>");
}

static inline void _composed_pull2(fa_ptr_t x, fa_buffer_t buffer)
{
    fa_unpair(x, callback, data) {
        ((fa_io_callback_t) callback)(data, buffer);
    }
}
static inline void _composed_pull(fa_ptr_t x, fa_buffer_t buffer)
{
    fa_unpair(x, f2, closure) {
        fa_io_pull_through(f2, (fa_io_source_t) fa_io_ref_filter(&buffer), _composed_pull2, closure);
    }
}

void composed_filter_pull(fa_ptr_t x, fa_io_source_t upstream, fa_io_callback_t callback, ptr_t data)
{
    fa_io_filter_t f1 = ((struct filter_base *) x)->data1;
    fa_io_filter_t f2 = ((struct filter_base *) x)->data2;

    fa_io_pull_through(f1, upstream, _composed_pull, pair(f2, pair(callback, data)));
}
void composed_filter_push(fa_ptr_t x, fa_io_sink_t downstream, fa_buffer_t buffer)
{
    fa_io_filter_t f1 = ((struct filter_base *) x)->data1;
    fa_io_filter_t f2 = ((struct filter_base *) x)->data2;

    buffer_t buffer2;
    fa_io_push_through(f1, (fa_io_sink_t) fa_io_ref_filter(&buffer2), buffer);
    fa_io_push_through(f2, downstream, buffer2);
}
FILTER_IMPLEMENTATION(composed_filter);


// ------------------------------------------------------------------------------------------

string_t simple_filter_show(ptr_t x)
{
    return string("<Ref>");
}

void _simple_filter_pull1(ptr_t y, buffer_t buffer) {
    fa_unpair(y, x, closure) {
        fa_io_callback_t      push = ((struct filter_base *) x)->data1;
        fa_io_read_callback_t pull = ((struct filter_base *) x)->data2;
        ptr_t                 cbData = ((struct filter_base *) x)->data3;
        
        push(cbData, buffer);
        fa_unpair(closure, callback, data) {
            pull(cbData, callback, data);
        }
    }
}

// pull from upstream, push to x, pull from x
void simple_filter_pull(fa_ptr_t x, fa_io_source_t upstream, fa_io_callback_t callback, ptr_t data)
{
    fa_io_pull(upstream, _simple_filter_pull1, pair(x, pair(callback, data)));
}                             

void _simple_push1(ptr_t downstream, buffer_t buffer) {
    fa_io_push(downstream, buffer);
}

// push to x, pull from x, push downstream
void simple_filter_push(fa_ptr_t x, fa_io_sink_t downstream, fa_buffer_t buffer)
{
    fa_io_callback_t      push = ((struct filter_base *) x)->data1;
    fa_io_read_callback_t pull = ((struct filter_base *) x)->data2;
    ptr_t                 cbData = ((struct filter_base *) x)->data3;

    push(cbData, buffer);
    pull(cbData, _simple_push1, downstream);
}
FILTER_IMPLEMENTATION(simple_filter);


// ------------------------------------------------------------------------------------------

fa_io_filter_t fa_io_split(fa_io_sink_t sink)
{
    struct filter_base *x = fa_new_struct(filter_base);
    x->impl = &split_filter_impl;
    x->data1 = sink;
    return (fa_io_filter_t) x;
}

fa_io_filter_t fa_io_compose(fa_io_filter_t f1, fa_io_filter_t f2)
{
    struct filter_base *x = fa_new_struct(filter_base);
    x->impl = &composed_filter_impl;
    x->data1 = f1;
    x->data2 = f2;
    return (fa_io_filter_t) x;
}

fa_io_source_t fa_io_map(fa_io_source_t f1, fa_io_filter_t f2)
{
    return (fa_io_source_t) fa_io_compose((fa_io_filter_t) f1, f2);
}

fa_io_sink_t fa_io_contramap(fa_io_filter_t f1, fa_io_sink_t f2)
{
    return (fa_io_sink_t) fa_io_compose(f1, (fa_io_filter_t) f2);
}


fa_io_filter_t fa_io_ref_filter(ptr_t r)
{
    struct filter_base *x = fa_new_struct(filter_base);
    x->impl = &ref_filter_impl;
    x->data1 = r;
    return (fa_io_filter_t) x;
}

fa_io_filter_t fa_io_identity()
{
    struct filter_base *x = fa_new_struct(filter_base);
    x->impl = &identity_impl;
    return (fa_io_filter_t) x;
}

fa_io_source_t fa_io_standard_in()
{
    struct filter_base *x = fa_new_struct(filter_base);
    x->impl = &stdin_filter_impl;
    return (fa_io_source_t) x;
}

fa_io_sink_t fa_io_standard_out()
{
    struct filter_base *x = fa_new_struct(filter_base);
    x->impl = &standardout_filter_impl;
    return (fa_io_sink_t) x;
}

fa_io_sink_t fa_io_write_file(string_t path)
{
    struct filter_base *x = fa_new_struct(filter_base);
    x->impl = &write_filter_impl;
    x->data1 = path;
    return (fa_io_sink_t) x;
}

fa_io_source_t fa_io_read_file(string_t path)
{
    struct filter_base *x = fa_new_struct(filter_base);
    x->impl = &read_filter_impl;
    x->data1 = path;
    return (fa_io_source_t) x;
}


fa_io_filter_t fa_io_create_simple_filter(fa_io_callback_t callback,
                                          fa_io_read_callback_t readCallback,
                                          ptr_t data)
{
    struct filter_base *x = fa_new_struct(filter_base);
    x->impl = &simple_filter_impl;
    x->data1 = callback;
    x->data2 = readCallback;
    x->data3 = data;
    return (fa_io_filter_t) x;
}






static inline void _run(fa_ptr_t x, fa_buffer_t buffer)
{
    // fprintf(stderr, "%s\n", unstring(fa_string_show(buffer)));
    // fflush(stderr);

    fa_unpair(x, sink, ok) {
        if (!buffer) *((bool*) ok) = false;
        else
        fa_io_push(sink, buffer);
    }
}
void fa_io_run(fa_io_source_t source, fa_io_sink_t sink)
{            
    bool ok = true;
    while (ok) { // TODO
        fa_io_pull(source, _run, pair(sink, &ok));
    }
}