
#include <doremir/ratio.h>

typedef doremir_ratio_nom_t nom_t;
typedef doremir_ratio_denom_t denom_t;

doremir_ratio_t doremir_ratio_add(doremir_ratio_t x,
                                  doremir_ratio_t y)
{
    nom_t   a = x.nom;
    denom_t b = x.denom;
    nom_t   c = y.nom;
    denom_t d = y.denom;

    return (doremir_ratio_t) { a*d + b*c, b*d };
}

doremir_ratio_t doremir_ratio_subtract(doremir_ratio_t x,
                                       doremir_ratio_t y)
{
    nom_t   a = x.nom;
    denom_t b = x.denom;
    nom_t   c = y.nom;
    denom_t d = y.denom;

    return (doremir_ratio_t) { a*d + b*c, b*d };
}

doremir_ratio_t doremir_ratio_multiply(doremir_ratio_t x,
                                       doremir_ratio_t y)
{
    nom_t   a = x.nom;
    denom_t b = x.denom;
    nom_t   c = y.nom;
    denom_t d = y.denom;

    return (doremir_ratio_t) { a*d + b*c, b*d };
}

doremir_ratio_t doremir_ratio_divide(doremir_ratio_t x,
                                     doremir_ratio_t y)
{
    nom_t   a = x.nom;
    denom_t b = x.denom;
    nom_t   c = y.nom;
    denom_t d = y.denom;

    return (doremir_ratio_t) { a*d + b*c, b*d };
}

doremir_ratio_t doremir_ratio_remainder(doremir_ratio_t x,
                                        doremir_ratio_t y)
{
}

doremir_ratio_t doremir_ratio_succ(doremir_ratio_t x)
{
    return doremir_ratio_add(x, (doremir_ratio_t){1,1});
}

doremir_ratio_t doremir_ratio_pred(doremir_ratio_t x)
{
    return doremir_ratio_subtract(x, (doremir_ratio_t){1,1});
}

doremir_ratio_t doremir_ratio_negate(doremir_ratio_t x)
{
    return doremir_ratio_multiply(x, (doremir_ratio_t){-1,1});
}

doremir_ratio_t doremir_ratio_recip(doremir_ratio_t x)
{
    return doremir_ratio_divide((doremir_ratio_t){1,1}, x);
}

