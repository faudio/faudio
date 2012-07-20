
cmake_minimum_required(VERSION 2.8.8)

# Logic

# (macro (negate-inline x)
#     (if (deref x)
#         (set x False)
#         (set x True)))

macro(negate_inline x)
    if(${${x}})
        set(${x} False)
    else()
        set(${x} True)
    endif()    
endmacro()

function(negate k x)
    set(a ${x})
    negate_inline(a)
    set(${k} ${a} PARENT_SCOPE) 
endfunction()

macro(bool_inline x)
    if(${${x}})
        set(${x} True)
    else()
        set(${x} False)
    endif()    
endmacro()

function(bool k x)
    set(a ${x})
    bool_inline(a)
    set(${k} ${a} PARENT_SCOPE) 
endfunction()

function(assert x m)
    if(NOT assert)
        message(FATAL_ERROR ${m})
    endif()
endfunction()






# Unary functions

function(func_succ k x)
    math(EXPR j "${x} + 1")
    set(${k} ${j} PARENT_SCOPE)
endfunction()

function(func_pred k x)
    math(EXPR j "${x} - 1")
    set(${k} ${j} PARENT_SCOPE)
endfunction()

function(apply_unary k f x)
    string(COMPARE EQUAL ${f} succ is_succ)
    string(COMPARE EQUAL ${f} pred is_pred)
    if(FALSE)
    elseif(is_succ)
        func_succ(j ${x})
        set(${k} ${j} PARENT_SCOPE)
    elseif(is_pred)
        func_pred(j ${x})
        set(${k} ${j} PARENT_SCOPE)
    else()
        message(FATAL_ERROR "apply_unary: Unknown function ${f}")
    endif()
endfunction()


# Binary functions

function(func_add k x y)
    math(j "${x} + ${y}")
    set(${k} ${j} PARENT_SCOPE)
endfunction()

function(func_subtract k x y)
    math(j "${x} - ${y}")
    set(${k} ${j} PARENT_SCOPE)
endfunction()

function(func_multiply k x y)
    math(j "${x} * ${y}")
    set(${k} ${j} PARENT_SCOPE)
endfunction()

function(func_divide k x y)
    math(j "${x} / ${y}")
    set(${k} ${j} PARENT_SCOPE)
endfunction()



# Combinators

function(constant k x)
endfunction()

function(identity k)
endfunction()

function(compose k f g)
endfunction()

function(bind k f x)
endfunction()



# Lists

function(head k xs)
    list(GET list 0 temp)
    set(${k} ${xs} PARENT_SCOPE)
endfunction()

# TODO has to use variadic definition here...
function(map k f xs)
    # set(ys "")
    foreach(x ${xs})
        message(">>>${x}")
        # apply_unary(j ${f} ${x})
        # set(ys "${ys};${j}")
    endforeach()
    # set(${k} ${ys} PARENT_SCOPE)
endfunction()





# -----------–-----------–-----------–-----------–-----------–-----------–
message(STATUS "negate")

set(x True)
negate(y ${x})
negate(z ${y})
message(${x})
message(${y})
message(${z})

# -----------–-----------–-----------–-----------–-----------–-----------–
message(STATUS "head")

set(xs 1 2 3)
head(k ${xs})
message(${k})


# -----------–-----------–-----------–-----------–-----------–-----------–
message(STATUS "apply_unary")

set(x 1)          
apply_unary(a succ ${x})
message(${a})


# -----------–-----------–-----------–-----------–-----------–-----------–
message(STATUS "map")

set(as 1 2 3 4 5)          
map(bs succ ${as})
message(${bs})
