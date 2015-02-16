#!/usr/bin/python2
# -*- coding: utf-8 -*-

import itertools
import re
import sys
import types

###

from pyparsing import delimitedList, Forward, Group, Keyword, nums, Suppress, Word

any_type = Forward()

LPAR, RPAR = map(Suppress, '()')
BT, IT, ST, LT, TT = TYPES = 'BISLT'
BKW, IKW, SKW, LKW, TKW = map(Keyword, TYPES)

bool_type = Group(BKW)
int_type = Group(IKW)
str_type = Group(SKW)
comp_type = Group(Word(nums))
atom_types = comp_type | bool_type | int_type | str_type

ATOM_TYPES = [BT, IT, ST]

list_type = Group(LKW + LPAR + Group(any_type) + RPAR)
tuple_type = Group(TKW + LPAR + Group(delimitedList(any_type)) + RPAR)

any_type << (atom_types | list_type | tuple_type)

###

mode = ''
huge = False

def ifHuge(l):
    global huge

    return (l if huge else [])

def print_usage():
    print >>(sys.stderr), ("\npreprocess.py: Adds features.\n\n"
                           "Usage: preprocess.py source.ml\n\n")

def stringify(obj, top = True):
    if isinstance(obj, types.StringType):
        return obj
    elif isinstance(obj, types.TupleType):
        return "(%s)" % ', '.join(stringify(i, False) for i in obj)
    elif isinstance(obj, types.ListType):
        return "[%s%s%s]" % ('\n    ' if top else '',
                             (';%s' % (' ' if not top else '\n    ')).join(stringify(i, False) for i in obj),
                             '\n' if top else '')
    else:
        raise TypeError('Bad argument type to stringify: ' + type(obj))

def getProperties(fromTyp, toTyp):
    try:
        return {
            ST: {IT: [("String.length", "len")]},
            LT: {IT: [("List.length", "len")]}
        }[fromTyp][toTyp]
    except KeyError:
        return []

def getConvergedPreds(typ1, var1, prop1, typ2, var2, prop2):
    if prop1 is not None and prop2 is not None:
        return []

    allPreds = {}
    if typ1 == typ2:
        allPreds.update({typ1[0][0]: []})

    if prop2 is not None and typ1 != typ2:
        props = getProperties(typ1[0][0], typ2[0][0])
        allPreds = allPreds.update({typ2[0][0]: sum((getBinaryPreds(typ2, var1, prop, typ2, var2, prop2) for prop in props), [])})

    if prop1 is not None and typ1 != typ2:
        props = getProperties(typ2[0][0], typ1[0][0])
        allPreds = allPreds.update({typ1[0][0]: sum((getBinaryPreds(typ1, var1, prop1, typ1, var2, prop) for prop in props), [])})

    if prop1 is None and prop2 is None:
        for T in TYPES:
            if T not in allPreds:
                if typ1[0][0] == T:
                    props = getProperties(typ2[0][0], T)
                    allPreds.update({T: sum((getBinaryPreds([[T]], var1, None, [[T]], var2, p2) for p2 in props), [])})
                elif typ2[0][0] == T:
                    props = getProperties(typ1[0][0], T)
                    allPreds.update({T: sum((getBinaryPreds([[T]], var1, p1, [[T]], var2, None) for p1 in props), [])})
                else:
                    props1 = getProperties(typ1[0][0], T)
                    props2 = getProperties(typ2[0][0], T)
                    allPreds.update({T: sum((getBinaryPreds([[T]], var1, p1, [[T]], var2, p2) for (p1,p2) in zip(props1,props2)), []) })

    return sum([allPreds[k] for k in allPreds], [])

def getBinaryPreds(typ1, var1, prop1, typ2, var2, prop2):
    global mode

    nvar1 = [var1 if prop1 is None else '%s(%s)' % (prop1[0],var1), var1 if prop1 is None else '%s(%s)' % (prop1[1], var1)]
    nvar2 = [var2 if prop2 is None else '%s(%s)' % (prop2[0],var2), var2 if prop2 is None else '%s(%s)' % (prop2[1], var2)]

    preds = []
    if typ1[0][0] == BT and typ2[0][0] == BT:
        preds.extend(map(lambda (f,n): (f % (var1, '', nvar1[0], ''), n % (nvar1[1], 'true')) if var2 is None else (
                                        f % (var1, var2+' ', nvar1[0], ' = ' + nvar2[0]), n % (nvar1[1], nvar2[1])),
                         [('(fun %s %s-> %s%s)', '"%s = %s"')]
                         + ifHuge([('(fun %s %s-> %s%s)', '"%s != %s"')])))

    elif typ1[0][0] == IT and typ2[0][0] == IT:
        preds.extend(map(lambda (f,n): (f % (var1, '', nvar1[0], '0'), n % (nvar1[1], '0')) if var2 is None else (
                                        f % (var1, var2+' ', nvar1[0], nvar2[0]), n % (nvar1[1], nvar2[1])),
                         [('(fun %s %s-> %s > %s)', '"%s > %s"'),
                          ('(fun %s %s-> %s = %s)', '"%s = %s"')]
                         + ifHuge([('(fun %s %s-> %s < %s)', '"%s < %s"')])))

    elif typ1[0][0] == ST and typ2[0][0] == ST and var2 is not None:
        preds.extend(map(lambda (f,n): (f % (var1, var2+' ', nvar1[0], nvar2[0]), n % (nvar1[1], nvar2[1])),
                         [('(fun %s %s-> %s > %s)', '"%s > %s"'),
                          ('(fun %s %s-> %s = %s)', '"%s = %s"')]
                         + ifHuge([('(fun %s %s-> %s == %s)', '"%s == %s"'),
                                   ('(fun %s %s-> %s <> %s)', '"%s <> %s"'),
                                   ('(fun %s %s-> %s != %s)', '"%s != %s"'),
                                   ('(fun %s %s-> %s < %s)', '"%s < %s"')])))

    if var2 is not None:
        if typ1 == typ2 and typ1[0][0] not in ATOM_TYPES:
            preds.extend(map(lambda (f,n): (f % (var1, var2+' ', nvar1[0], nvar2[0]), n % (nvar1[1], nvar2[1])),
                            [('(fun %s %s-> %s = %s)', '"%s = %s"')]
                            + ifHuge([('(fun %s %s-> %s == %s)', '"%s == %s"'),
                                    ('(fun %s %s-> %s <> %s)', '"%s <> %s"'),
                                    ('(fun %s %s-> %s != %s)', '"%s != %s"')])))

        if mode == 'F':
            if typ1[0][0] == LT:
                preds.extend(('(fun %s %s -> List.for_all (fun %se -> %s %se %s) %s)' % (var1, var2, var1, f[0], var1, var2, var1),
                            '"∀ %se in %s -> %s"' % (var1, var1, f[1][1:-1])) for f
                            in getBinaryPreds(typ1[0][1], '%se' % var1, None, typ2, var2, None))
                preds.extend(('(fun %s %s -> List.exists (fun %se -> %s %se %s) %s)' % (var1, var2, var1, f[0], var1, var2, var1),
                            '"∃ %se in %s -> %s"' % (var1, var1, f[1][1:-1])) for f
                            in getBinaryPreds(typ1[0][1], '%se' % var1, None, typ2, var2, None))

            elif typ2[0][0] == LT:
                preds.extend(('(fun %s %s -> List.for_all (fun %se -> %s %s %se) %s)' % (var1, var2, var2, f[0], var1, var2, var2),
                            '"∀ %se in %s -> %s"' % (var2, var2, f[1][1:-1])) for f
                            in getBinaryPreds(typ1, var1, None, typ2[0][1], '%se' % var2, None))
                preds.extend(('(fun %s %s -> List.exists (fun %se -> %s %s %se) %s)' % (var1, var2, var2, f[0], var1, var2, var2),
                            '"∃ %se in %s -> %s"' % (var2, var2, f[1][1:-1])) for f
                            in getBinaryPreds(typ1, var1, None, typ2[0][1], '%se' % var2, None))

        if typ1[0][0] == TT:
            tps = [([t], var1+str(i)) for (i,t) in enumerate(typ1[0][1])]
            head = '(fun (%s) %s -> ' % (','.join(t[1] for t in tps), var2)
            preds.extend(sum(([(head + '(%s %s %s))' % (f, v, var2), a) for (f,a)
                            in getBinaryPreds(t, v, None, typ2, var2, None)] for (t,v)
                            in tps), []))

        if typ2[0][0] == TT:
            tps = [([t], var2+str(i)) for (i,t) in enumerate(typ2[0][1])]
            head = '(fun %s (%s) -> ' % (var1, ','.join(t[1] for t in tps))
            preds.extend(sum(([(head + '(%s %s %s))' % (f, var1, v), a) for (f,a)
                            in getBinaryPreds(typ1, var1, None, t, v, None)] for (t,v)
                            in tps), []))

    return preds + getConvergedPreds(typ1, var1, prop1, typ2, var2, prop2)

def explodeUnaryPredsInTuple(allTypsNVars):
    subFeatures = ((getFeatures([t], v) or (), v) for (t,v) in allTypsNVars)
    head = '(fun (%s) -> ' % (','.join(t[1] for t in allTypsNVars))
    return sum(([(head + ('(%s %s))' % (fa[0], v)), fa[1]) for fa in fas] for (fas,v)
                in subFeatures if fas != ()),
               [])

def explodeBinaryPredsInTuple(allTypsNVars):
    head = '(fun (%s) -> ' % (','.join([t[1] for t in allTypsNVars]))
    return sum(([(head + '(%s %s %s))' % (f, v1, v2), a) for (f,a)
                 in getBinaryPreds([t1], v1, None, [t2], v2, None)] for ((t1,v1),(t2,v2))
                in itertools.combinations(allTypsNVars, 2)),
               [])

def getFeatures(typ, var):
    features = []

    if typ[0][0] == LT:
        features.extend(('(fun %s -> List.for_all %s %s)' % (var, f[0], var),
                         '"∀ %se in %s -> %s"' % (var, var, f[1][1:-1])) for f
                        in getFeatures(typ[0][1], '%se' % var))
        features.extend(('(fun %s -> List.exists %s %s)' % (var, f[0], var),
                         '"∃ %se in %s -> %s"' % (var, var, f[1][1:-1])) for f
                        in getFeatures(typ[0][1], '%se' % var))

    elif typ[0][0] == TT:
        tps = zip(typ[0][1], map(lambda x: var+str(x), range(len(typ[0][1]))))
        features.extend(explodeUnaryPredsInTuple(tps))
        features.extend(explodeBinaryPredsInTuple(tps))

    features.extend(getBinaryPreds(typ, var, None, typ, None, None))
    return features

def Fanalyze(pat):
    global mode

    mode = "F"
    var, typ = pat.split('|')
    return stringify(getFeatures(any_type.parseString(typ).asList(), var))

def Panalyze(pat):
    global mode

    mode = "P"
    var, ityp, otyp = pat.split('|')
    ityp = any_type.parseString(ityp).asList()
    otyp = any_type.parseString(otyp).asList()
    ofeatures = [('(fun z r -> match r with Bad _ -> false | Ok ' + f[5:], a)
                 for (f,a) in getFeatures(otyp, 'res')]
    iofeatures = [('(fun ' + f[5:].replace(' ', ' r -> match r with Bad _ -> false | Ok ', 1), a)
                  for (f,a) in getBinaryPreds(ityp, var, None, otyp, 'res', None)]
    return stringify([('(fun z r -> match r with Bad _ -> true | _ -> false)', '"exception thrown"'),
                      ('(fun z r -> match r with Ok _ -> true | _ -> false)', '"terminates normally"')] + ofeatures + iofeatures)

if len(sys.argv) < 2:
    print >>(sys.stderr), 'Missing argument: OCaml file to be pre-processed.'
    print_usage()
    sys.exit(1)
elif len(sys.argv) > 2 and sys.argv[2] == 'ALL':
    huge = True

if __name__ == "__main__":
    Fpat = "\(\*PYF:(%s)\*\)"
    Fmat = re.compile(Fpat % '[^\*]+')
    Ppat = "\(\*PYP:(%s)\*\)"
    Pmat = re.compile(Ppat % '[^\*]+')
    for l in open(sys.argv[1]).readlines():
        Fres = Fmat.search(l)
        Frep = l if Fres is None else Fmat.sub(Fanalyze(Fres.group(1)), l)
        Pres = Pmat.search(Frep)
        sys.stdout.write(Frep if Pres is None else Pmat.sub(Panalyze(Pres.group(1)), Frep))
