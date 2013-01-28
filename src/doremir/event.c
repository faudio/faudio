
/*
    DoReMIR Audio Engine
    Copyright (c) DoReMIR Music Research 2012-2013
    All rights reserved.
 */

#include <doremir/event.h>
#include <doremir/set.h>
#include <doremir/string.h>
#include <doremir/util.h>

/*
    Notes:
        * We implement Map as a Set of entries
        * An edge is exactly like a pair, but compares on the first element only
        * Performance, memory etc depend entirely on Set implementation
 */
struct _doremir_event_t {
  impl_t      impl;

  enum {
    never_event,      // E a
    now_event,        // a -> E a
    delay_event,      // t -> E a -> E a
    either_event,     // E a -> E a -> E a
    switch_event     // E a -> E b -> E b -> E b
  }                       tag;

  union {
    struct {
    }                     never;
    struct {
      ptr_t value;
    }                     now;
    struct {
      ptr_t               event;
      time_t              time;
    }                     delay;
    struct {
      ptr_t               left;
      ptr_t               right;
    }                     either;
    struct {
      ptr_t               pred;
      ptr_t               left;
      ptr_t               right;
    }                     switch_;
  }                       fields;
};

// --------------------------------------------------------------------------------


doremir_event_t new_event(int tag)
{
  event_t e = doremir_new(event);
  doremir_ptr_t event_impl(doremir_id_t interface);

  e->impl = &event_impl;
  e->tag  = tag;

  return e;
}

void delete_event(doremir_event_t event)
{
  doremir_delete(event);
}

#define now_get(v,f)      v->fields.now.f
#define delay_get(v,f)    v->fields.delay.f
#define either_get(v,f)   v->fields.either.f
#define switch_get(v,f)   v->fields.switch_.f

// --------------------------------------------------------------------------------

doremir_event_t doremir_event_never()
{
  event_t e = new_event(never_event);
  return e;
}

doremir_event_t doremir_event_now(doremir_ptr_t value)
{
  event_t e = new_event(now_event);
  now_get(e, value) = value;
  return e;
}

doremir_event_t doremir_event_delay(doremir_time_t time,
                                    doremir_event_t event)
{
  event_t e = new_event(delay_event);
  delay_get(e, time)  = time;
  delay_get(e, event) = event;
  return e;
}

doremir_event_t doremir_event_either(doremir_event_t event1,
                                     doremir_event_t event2)
{
  event_t e = new_event(either_event);
  either_get(e, left)   = event1;
  either_get(e, right)  = event2;
  return e;
}

doremir_event_t doremir_event_switch(doremir_event_t pred,
                                     doremir_event_t event1,
                                     doremir_event_t event2)
{
  event_t e = new_event(switch_event);
  switch_get(e, pred)   = pred;
  switch_get(e, left)   = event1;
  switch_get(e, right)  = event2;
  return e;
}

void doremir_event_destroy(doremir_event_t event)
{                        
  // TODO manage components
  delete_event(event);
}

doremir_time_t doremir_event_delta(doremir_event_t event)
{
  switch(event->tag)
  {
  case never_event:
    break;
  case now_event:
    break;
  case delay_event:
    break;
  case either_event:
    break;
  case switch_event:
    break;
  }
}

doremir_time_t doremir_event_live(doremir_event_t event, doremir_time_t time)
{
  switch(event->tag)
  {
  case never_event:
    break;
  case now_event:
    break;
  case delay_event:
    break;
  case either_event:
    break;
  case switch_event:
    break;
  }
}

doremir_event_t doremir_event_external()
{
}

void doremir_event_trig(doremir_event_t event)
{
}


doremir_ptr_t event_impl(doremir_id_t interface)
{
  
}
