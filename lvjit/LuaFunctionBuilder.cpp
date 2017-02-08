/*******************************************************************************
 *
 * (c) Copyright IBM Corp. 2016, 2017
 *
 *  This program and the accompanying materials are made available
 *  under the terms of the Eclipse Public License v1.0 and
 *  Apache License v2.0 which accompanies this distribution.
 *
 *      The Eclipse Public License is available at
 *      http://www.eclipse.org/legal/epl-v10.html
 *
 *      The Apache License v2.0 is available at
 *      http://www.opensource.org/licenses/apache2.0.php
 *
 * Contributors:
 *    Multiple authors (IBM Corp.) - initial implementation and documentation
 ******************************************************************************/

// Lua Vermelha headers
#include "LuaFunctionBuilder.hpp"

// JitBuilder headers
#include "ilgen/VirtualMachineState.hpp"

// c libraries
#include <math.h>
#include <stdio.h>

#define DEBUG 0


//~ convenience functions to be called from JITed code ~~~~~~~~~~~~~~~~~~~~~~~~~

static void printAddr(void* addr) {
   printf("\t%p\n", addr);
}

static void printInt(int n) {
   printf("\t%d\n", n);
}

static void printFloat(float n) {
   printf("\t%f\n", n);
}

static void printDouble(double n) {
   printf("\t%f\n", n);
}


//~ Lua VM functions and macros ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

/*
The following functions and macros are taken from `lvm.c`. These local copies
are used as helpers either by JITed code of IL generation.
*/

static int forlimit (const TValue *obj, lua_Integer *p, lua_Integer step,
                     int *stopnow) {
  *stopnow = 0;  /* usually, let loops run */
  if (!luaV_tointeger(obj, p, (step < 0 ? 2 : 1))) {  /* not fit in integer? */
    lua_Number n;  /* try to convert to float */
    if (!tonumber(obj, &n)) /* cannot convert to float? */
      return 0;  /* not a number */
    if (luai_numlt(0, n)) {  /* if true, float is larger than max integer */
      *p = LUA_MAXINTEGER;
      if (step < 0) *stopnow = 1;
    }
    else {  /* float is smaller than min integer */
      *p = LUA_MININTEGER;
      if (step >= 0) *stopnow = 1;
    }
  }
  return 1;
}

static LClosure *getcached (Proto *p, UpVal **encup, StkId base) {
  LClosure *c = p->cache;
  if (c != NULL) {  /* is there a cached closure? */
    int nup = p->sizeupvalues;
    Upvaldesc *uv = p->upvalues;
    int i;
    for (i = 0; i < nup; i++) {  /* check whether it has right upvalues */
      TValue *v = uv[i].instack ? base + uv[i].idx : encup[uv[i].idx]->v;
      if (c->upvals[i]->v != v)
        return NULL;  /* wrong upvalue; cannot reuse closure */
    }
  }
  return c;  /* return cached closure (or NULL if no cached closure) */
}

static void pushclosure (lua_State *L, Proto *p, UpVal **encup, StkId base,
                         StkId ra) {
  int nup = p->sizeupvalues;
  Upvaldesc *uv = p->upvalues;
  int i;
  LClosure *ncl = luaF_newLclosure(L, nup);
  ncl->p = p;
  setclLvalue(L, ra, ncl);  /* anchor new closure in stack */
  for (i = 0; i < nup; i++) {  /* fill in its upvalues */
    if (uv[i].instack)  /* upvalue refers to local variable? */
      ncl->upvals[i] = luaF_findupval(L, base + uv[i].idx);
    else  /* get upvalue from enclosing function */
      ncl->upvals[i] = encup[uv[i].idx];
    ncl->upvals[i]->refcount++;
    /* new closure is white, so we do not need a barrier here */
  }
  if (!isblack(p))  /* cache will not break GC invariant? */
    p->cache = ncl;  /* save it on cache for reuse */
}

#define Protect(x)	{ {x;}; base = ci->u.l.base; }

#define RA(i)	(base+GETARG_A(i))
#define RB(i)	check_exp(getBMode(GET_OPCODE(i)) == OpArgR, base+GETARG_B(i))
#define RC(i)	check_exp(getCMode(GET_OPCODE(i)) == OpArgR, base+GETARG_C(i))
#define RKB(i)	check_exp(getBMode(GET_OPCODE(i)) == OpArgK, \
	ISK(GETARG_B(i)) ? k+INDEXK(GETARG_B(i)) : base+GETARG_B(i))
#define RKC(i)	check_exp(getCMode(GET_OPCODE(i)) == OpArgK, \
	ISK(GETARG_C(i)) ? k+INDEXK(GETARG_C(i)) : base+GETARG_C(i))


#define Protect(x)	{ {x;}; base = ci->u.l.base; }

#define checkGC(L,c)  \
   { luaC_condGC(L, L->top = (c),  /* limit of live values */ \
                         Protect(L->top = ci->top));  /* restore top */ \
           luai_threadyield(L); }

/*
** copy of 'luaV_gettable', but protecting the call to potential
** metamethod (which can reallocate the stack)
*/
#define gettableProtected(L,t,k,v)  { const TValue *slot; \
  if (luaV_fastget(L,t,k,slot,luaH_get)) { setobj2s(L, v, slot); } \
  else Protect(luaV_finishget(L,t,k,v,slot)); }


/* same for 'luaV_settable' */
#define settableProtected(L,t,k,v) { const TValue *slot; \
  if (!luaV_fastset(L,t,k,slot,luaH_get,v)) \
    Protect(luaV_finishset(L,t,k,v,slot)); }


//~ Lua VM macro wrappers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

/*
The following are helper functions that wrap Lua VM macros so they can be called
from JITed code.
*/

void jit_setbvalue(TValue* obj, int x) {
   setbvalue(obj,x);
}

StkId jit_gettableProtected(lua_State* L, TValue* t, TValue* k, TValue* v) {
   auto ci = L->ci;
   StkId base = ci->u.l.base;
   gettableProtected(L, t, k, v);
   return base;
}

StkId jit_settableProtected(lua_State* L, TValue* t, TValue* k, TValue* v) {
   auto ci = L->ci;
   StkId base = ci->u.l.base;
   settableProtected(L, t, k, v);
   return base;
}

void jit_checkstack(lua_State* L, int n) {
   luaD_checkstack(L, n);
}

void jit_luaC_upvalbarrier(lua_State *L, UpVal *uv) {
   luaC_upvalbarrier(L, uv);
}

//~ Lua VM interpreter helpers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

/*
The following functions are helpers for handling specific opcodes. Each
implements a simplified version of the code in the Lua VM interpreter for
handling the coresponding opcode. Instead of generating IL for handling these
opcodes, JITed code can simply call these function, which essentially copy the
behavior of the the VM interpreter. Although this is not the most optimal
approach, it is simple and a good way of starting out. As the project evolves,
calls to these functions can be replaced by "hand-generated" IL.
*/

StkId vm_gettable(lua_State* L, Instruction i) {
   // prologue
   CallInfo *ci = L->ci;
   LClosure *cl = clLvalue(ci->func);
   TValue *k = cl->p->k;
   StkId base = ci->u.l.base;
   StkId ra = RA(i);

   // main body
   StkId rb = RB(i);
   TValue *rc = RKC(i);
   gettableProtected(L, rb, rc, ra);

   // epilogue
   return base;
}

StkId vm_settable(lua_State* L, Instruction i) {
   // prologue
   CallInfo *ci = L->ci;
   LClosure *cl = clLvalue(ci->func);
   TValue *k = cl->p->k;
   StkId base = ci->u.l.base;
   StkId ra = RA(i);

   // main body
   TValue *rb = RKB(i);
   TValue *rc = RKC(i);
   settableProtected(L, ra, rb, rc);

   // epilogue
   return base;
}

StkId vm_newtable(lua_State* L, Instruction i) {
   // prologue
   CallInfo *ci = L->ci;
   StkId base = ci->u.l.base;
   StkId ra = RA(i);

   // main body
   int b = GETARG_B(i);
   int c = GETARG_C(i);
   Table *t = luaH_new(L);
   sethvalue(L, ra, t);
   if (b != 0 || c != 0)
     luaH_resize(L, t, luaO_fb2int(b), luaO_fb2int(c));
   checkGC(L, ra + 1);

   // epilogue
   return base;
}

StkId vm_self(lua_State* L, Instruction i) {
   // prologue
   CallInfo *ci = L->ci;
   LClosure *cl = clLvalue(ci->func);
   TValue *k = cl->p->k;
   StkId base = ci->u.l.base;
   StkId ra = RA(i);

   // main body
   const TValue *aux;
   StkId rb = RB(i);
   TValue *rc = RKC(i);
   TString *key = tsvalue(rc);  /* key must be a string */
   setobjs2s(L, ra + 1, rb);
   if (luaV_fastget(L, rb, key, aux, luaH_getstr)) {
     setobj2s(L, ra, aux);
   }
   else Protect(luaV_finishget(L, rb, rc, ra, aux));

   // epilogue
   return base;
}

StkId vm_mod(lua_State* L, Instruction i) {
   // prologue
   CallInfo *ci = L->ci;
   LClosure *cl = clLvalue(ci->func);
   TValue *k = cl->p->k;
   StkId base = ci->u.l.base;
   StkId ra = RA(i);

   // main body
    TValue *rb = RKB(i);
   TValue *rc = RKC(i);
   lua_Number nb; lua_Number nc;
   if (ttisinteger(rb) && ttisinteger(rc)) {
     lua_Integer ib = ivalue(rb); lua_Integer ic = ivalue(rc);
     setivalue(ra, luaV_mod(L, ib, ic));
   }
   else if (tonumber(rb, &nb) && tonumber(rc, &nc)) {
     lua_Number m;
     luai_nummod(L, nb, nc, m);
     setfltvalue(ra, m);
   }
   else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_MOD)); }

   // epilogue
   return base;
}

StkId vm_pow(lua_State* L, Instruction i) {
   // prologue
   CallInfo *ci = L->ci;
   LClosure *cl = clLvalue(ci->func);
   TValue *k = cl->p->k;
   StkId base = ci->u.l.base;
   StkId ra = RA(i);

   // main body
   TValue *rb = RKB(i);
   TValue *rc = RKC(i);
   lua_Number nb; lua_Number nc;
   if (tonumber(rb, &nb) && tonumber(rc, &nc)) {
     setfltvalue(ra, luai_numpow(L, nb, nc));
   }
   else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_POW)); }

   // epilogue
   return base;
}

StkId vm_idiv(lua_State* L, Instruction i) {
   // prologue
   CallInfo *ci = L->ci;
   LClosure *cl = clLvalue(ci->func);
   TValue *k = cl->p->k;
   StkId base = ci->u.l.base;
   StkId ra = RA(i);

   // main body
   TValue *rb = RKB(i);
   TValue *rc = RKC(i);
   lua_Number nb; lua_Number nc;
   if (ttisinteger(rb) && ttisinteger(rc)) {
     lua_Integer ib = ivalue(rb); lua_Integer ic = ivalue(rc);
     setivalue(ra, luaV_div(L, ib, ic));
   }
   else if (tonumber(rb, &nb) && tonumber(rc, &nc)) {
     setfltvalue(ra, luai_numidiv(L, nb, nc));
   }
   else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_IDIV)); }

   // epilogue
   return base;
}

StkId vm_band(lua_State* L, Instruction i) {
   // prologue
   CallInfo *ci = L->ci;
   LClosure *cl = clLvalue(ci->func);
   TValue *k = cl->p->k;
   StkId base = ci->u.l.base;
   StkId ra = RA(i);

   // main body
   TValue *rb = RKB(i);
   TValue *rc = RKC(i);
   lua_Integer ib; lua_Integer ic;
   if (tointeger(rb, &ib) && tointeger(rc, &ic)) {
     setivalue(ra, intop(&, ib, ic));
   }
   else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_BAND)); }

   // epilogue
   return base;
}

StkId vm_bor(lua_State* L, Instruction i) {
   // prologue
   CallInfo *ci = L->ci;
   LClosure *cl = clLvalue(ci->func);
   TValue *k = cl->p->k;
   StkId base = ci->u.l.base;
   StkId ra = RA(i);

   // main body
   TValue *rb = RKB(i);
   TValue *rc = RKC(i);
   lua_Integer ib; lua_Integer ic;
   if (tointeger(rb, &ib) && tointeger(rc, &ic)) {
     setivalue(ra, intop(|, ib, ic));
   }
   else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_BOR)); }

   // epilogue
   return base;
}

StkId vm_bxor(lua_State* L, Instruction i) {
   // prologue
   CallInfo *ci = L->ci;
   LClosure *cl = clLvalue(ci->func);
   TValue *k = cl->p->k;
   StkId base = ci->u.l.base;
   StkId ra = RA(i);

   // main body
   TValue *rb = RKB(i);
   TValue *rc = RKC(i);
   lua_Integer ib; lua_Integer ic;
   if (tointeger(rb, &ib) && tointeger(rc, &ic)) {
     setivalue(ra, intop(^, ib, ic));
   }
   else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_BXOR)); }

   // epilogue
   return base;
}

StkId vm_shl(lua_State* L, Instruction i) {
   // prologue
   CallInfo *ci = L->ci;
   LClosure *cl = clLvalue(ci->func);
   TValue *k = cl->p->k;
   StkId base = ci->u.l.base;
   StkId ra = RA(i);

   // main body
   TValue *rb = RKB(i);
   TValue *rc = RKC(i);
   lua_Integer ib; lua_Integer ic;
   if (tointeger(rb, &ib) && tointeger(rc, &ic)) {
     setivalue(ra, luaV_shiftl(ib, ic));
   }
   else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_SHL)); }

   // epilogue
   return base;
}

StkId vm_shr(lua_State* L, Instruction i) {
   // prologue
   CallInfo *ci = L->ci;
   LClosure *cl = clLvalue(ci->func);
   TValue *k = cl->p->k;
   StkId base = ci->u.l.base;
   StkId ra = RA(i);

   // main body
   TValue *rb = RKB(i);
   TValue *rc = RKC(i);
   lua_Integer ib; lua_Integer ic;
   if (tointeger(rb, &ib) && tointeger(rc, &ic)) {
     setivalue(ra, luaV_shiftl(ib, -ic));
   }
   else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_SHR)); }

   // epilogue
   return base;
}

StkId vm_unm(lua_State* L, Instruction i) {
   // prologue
   CallInfo *ci = L->ci;
   StkId base = ci->u.l.base;
   StkId ra = RA(i);

   // main body
   TValue *rb = RB(i);
   lua_Number nb;
   if (ttisinteger(rb)) {
     lua_Integer ib = ivalue(rb);
     setivalue(ra, intop(-, 0, ib));
   }
   else if (tonumber(rb, &nb)) {
     setfltvalue(ra, luai_numunm(L, nb));
   }
   else {
     Protect(luaT_trybinTM(L, rb, rb, ra, TM_UNM));
   }

   // epilogue
   return base;
}

StkId vm_bnot(lua_State* L, Instruction i) {
   // prologue
   CallInfo *ci = L->ci;
   StkId base = ci->u.l.base;
   StkId ra = RA(i);

   // main body
   TValue *rb = RB(i);
   lua_Integer ib;
   if (tointeger(rb, &ib)) {
     setivalue(ra, intop(^, ~l_castS2U(0), ib));
   }
   else {
     Protect(luaT_trybinTM(L, rb, rb, ra, TM_BNOT));
   }

   // epilogue
   return base;
}

StkId vm_not(lua_State* L, Instruction i) {
   // prologue
   CallInfo *ci = L->ci;
   StkId base = ci->u.l.base;
   StkId ra = RA(i);

   // main body
   TValue *rb = RB(i);
   int res = l_isfalse(rb);  /* next assignment may change this value */
   setbvalue(ra, res);

   // epilogue
   return base;
}

StkId vm_len(lua_State* L, Instruction i) {
   // prologue
   CallInfo *ci = L->ci;
   StkId base = ci->u.l.base;
   StkId ra = RA(i);

   // main body
   Protect(luaV_objlen(L, ra, RB(i)));

   // epilogue
   return base;
}

StkId vm_concat(lua_State* L, Instruction i) {
   // prologue
   CallInfo *ci = L->ci;
   StkId base = ci->u.l.base;
   int b = GETARG_B(i);
   int c = GETARG_C(i);
   StkId ra;
   StkId rb;
   L->top = base + c + 1;  /* mark the end of concat operands */
   Protect(luaV_concat(L, c - b + 1));
   ra = RA(i);  /* 'luaV_concat' may invoke TMs and move the stack */
   rb = base + b;
   setobjs2s(L, ra, rb);
   checkGC(L, (ra >= rb ? ra + 1 : rb));
   L->top = ci->top;  /* restore top */

   return base;
}

int32_t vm_test(lua_State* L, Instruction i) {
   // prologue
   CallInfo *ci = L->ci;
   StkId base = ci->u.l.base;
   StkId ra = RA(i);
   int32_t skipnext = 0;

   // main body
   if (GETARG_C(i) ? l_isfalse(ra) : !l_isfalse(ra)) {
      ci->u.l.savedpc++;
      skipnext = 1;
   }
   else
      skipnext = 0;

   // epilogue
   return skipnext;
}

int32_t vm_testset(lua_State* L, Instruction i) {
   // prologue
   CallInfo *ci = L->ci;
   StkId base = ci->u.l.base;
   StkId ra = RA(i);
   int32_t skipnext = 0;

   // main body
   TValue *rb = RB(i);
   if (GETARG_C(i) ? l_isfalse(rb) : !l_isfalse(rb)) {
      ci->u.l.savedpc++;
      skipnext = 1;
   }
   else {
      setobjs2s(L, ra, rb);
      skipnext = 0;
   }

   // epilogue
   return skipnext;
}

int32_t vm_forloop(lua_State* L, Instruction i) {
   // prologue
   int32_t continueLoop = 0;
   CallInfo *ci = L->ci;
   StkId base = ci->u.l.base;
   StkId ra = RA(i);

   // main body
   if (ttisinteger(ra)) {  /* integer loop? */
      lua_Integer step = ivalue(ra + 2);
      lua_Integer idx = intop(+, ivalue(ra), step); /* increment index */
      lua_Integer limit = ivalue(ra + 1);
      if ((0 < step) ? (idx <= limit) : (limit <= idx)) {
        ci->u.l.savedpc += GETARG_sBx(i);  /* jump back */
        continueLoop = 1;
        chgivalue(ra, idx);  /* update internal index... */
        setivalue(ra + 3, idx);  /* ...and external index */
      }
   }
   else {  /* floating loop */
      lua_Number step = fltvalue(ra + 2);
      lua_Number idx = luai_numadd(L, fltvalue(ra), step); /* inc. index */
      lua_Number limit = fltvalue(ra + 1);
      if (luai_numlt(0, step) ? luai_numle(idx, limit)
                              : luai_numle(limit, idx)) {
        ci->u.l.savedpc += GETARG_sBx(i);  /* jump back */
        continueLoop = 1;
        chgfltvalue(ra, idx);  /* update internal index... */
        setfltvalue(ra + 3, idx);  /* ...and external index */
      }
   }

   // epilogue
   return continueLoop;
}

void vm_forprep(lua_State* L, Instruction i) {
   // prologue
   CallInfo *ci = L->ci;
   StkId base = ci->u.l.base;
   StkId ra = RA(i);

   // main body
   TValue *init = ra;
   TValue *plimit = ra + 1;
   TValue *pstep = ra + 2;
   lua_Integer ilimit;
   int stopnow;
   if (ttisinteger(init) && ttisinteger(pstep) &&
       forlimit(plimit, &ilimit, ivalue(pstep), &stopnow)) {
     /* all values are integer */
     lua_Integer initv = (stopnow ? 0 : ivalue(init));
     setivalue(plimit, ilimit);
     setivalue(init, intop(-, initv, ivalue(pstep)));
   }
   else {  /* try making all values floats */
     lua_Number ninit; lua_Number nlimit; lua_Number nstep;
     if (!tonumber(plimit, &nlimit))
       luaG_runerror(L, "'for' limit must be a number");
     setfltvalue(plimit, nlimit);
     if (!tonumber(pstep, &nstep))
       luaG_runerror(L, "'for' step must be a number");
     setfltvalue(pstep, nstep);
     if (!tonumber(init, &ninit))
       luaG_runerror(L, "'for' initial value must be a number");
     setfltvalue(init, luai_numsub(L, ninit, nstep));
   }
   ci->u.l.savedpc += GETARG_sBx(i);

   // epilogue
}

void vm_setlist(lua_State* L, Instruction i) {
   // prologue
   CallInfo *ci = L->ci;
   //LClosure *cl = clLvalue(ci->func);
   StkId base = ci->u.l.base;
   StkId ra = RA(i);

   // main body
   int n = GETARG_B(i);
   int c = GETARG_C(i);
   unsigned int last;
   Table *h;
   if (n == 0) n = cast_int(L->top - ra) - 1;
   if (c == 0) {
     lua_assert(GET_OPCODE(*ci->u.l.savedpc) == OP_EXTRAARG);
     c = GETARG_Ax(*ci->u.l.savedpc++);
   }
   h = hvalue(ra);
   last = ((c-1)*LFIELDS_PER_FLUSH) + n;
   if (last > h->sizearray)  /* needs more space? */
     luaH_resizearray(L, h, last);  /* preallocate it at once */
   for (; n > 0; n--) {
     TValue *val = ra+n;
     luaH_setint(L, h, last--, val);
     luaC_barrierback(L, h, val);
   }
   L->top = ci->top;  /* correct top (in case of previous open call) */
}

StkId vm_closure(lua_State* L, Instruction i) {
   // prologue
   CallInfo *ci = L->ci;
   LClosure *cl = clLvalue(ci->func);
   StkId base = ci->u.l.base;
   StkId ra = RA(i);

   // main body
   Proto *p = cl->p->p[GETARG_Bx(i)];
   LClosure *ncl = getcached(p, cl->upvals, base);  /* cached closure */
   if (ncl == NULL)  /* no match? */
     pushclosure(L, p, cl->upvals, base, ra);  /* create a new one */
   else
     setclLvalue(L, ra, ncl);  /* push cashed closure */
   checkGC(L, ra + 1);

   // epilogue
   return base;
}


//~ general IL generation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Lua::FunctionBuilder::FunctionBuilder(Proto* p, Lua::TypeDictionary* types)
   : TR::MethodBuilder(types), prototype(p), luaTypes(types->getLuaTypes()) {

   char file_name_buff[1024];
   snprintf(file_name_buff, 1024, "%s", getstr(prototype->source));

   char line_num_buff[1024];
   snprintf(line_num_buff, 1024, "%d", prototype->linedefined);

   // since Lua functions do not have actual names, use the format
   // `<filename>:<first line of definition>_<last line of definition>`
   // as the "unique" function name
   char func_name_buff[1024];
   snprintf(func_name_buff, 1024, "%s:%s_%d", file_name_buff, line_num_buff, prototype->lastlinedefined);

   DefineLine(line_num_buff);
   DefineFile(file_name_buff);

   DefineName(func_name_buff);
   DefineParameter("L", types->PointerTo("lua_State"));
   DefineReturnType(NoType);
   setUseBytecodeBuilders();

   auto pTValue = types->PointerTo(luaTypes.TValue);
   auto plua_State = types->PointerTo(luaTypes.lua_State);

   // convenience functions to be called from JITed code

   DefineFunction("printAddr", __FILE__, "0", (void*)printAddr,
                  NoType, 1,
                  types->toIlType<void*>());

   DefineFunction("printInt", __FILE__, "0", (void*)printInt,
                  NoType, 1,
                  types->toIlType<int>());

   DefineFunction("printFloat", __FILE__, "0", (void*)printFloat,
                  NoType, 1,
                  types->toIlType<float>());

   DefineFunction("printDouble", __FILE__, "0", (void*)printDouble,
                  NoType, 1,
                  types->toIlType<double>());

   DefineFunction("compiledbody", "0", "0", nullptr,
                  NoType, 1,
                  plua_State);

   // Lua VM functions to be called from JITed code

   DefineFunction("luaD_precall", "ldo.c", "344", (void*)luaD_precall,
                  Int32, 3,
                  plua_State,
                  luaTypes.StkId,
                  Int32);

   DefineFunction("luaD_poscall", "ldo.c", "453", (void*)luaD_poscall,
                  Int32, 4,
                  types->PointerTo(luaTypes.lua_State),
                  types->PointerTo(luaTypes.CallInfo),
                  luaTypes.StkId,
                  Int32);

   DefineFunction("luaD_call", "ldo.c", "492", (void*)luaD_call,
                  NoType, 3,
                  plua_State,
                  luaTypes.StkId,
                  types->toIlType<int>());

   DefineFunction("luaV_execute", "lvm.c", "790", (void*)luaV_execute,
                  NoType, 1,
                  plua_State);

   DefineFunction("luaJ_compile", "lvjit.cpp", "53", (void*)luaJ_compile,
                  Int32, 1,
                  types->PointerTo(luaTypes.Proto));

   DefineFunction("luaF_close", "lfunc.c", "83", (void*)luaF_close,
                  NoType, 2,
                  plua_State,
                  luaTypes.StkId);

   DefineFunction("luaT_trybinTM", "ltm.c", "135", (void*)luaT_trybinTM,
                  NoType, 5,
                  plua_State,
                  pTValue,
                  pTValue,
                  luaTypes.StkId,
                  luaTypes.TMS);

   DefineFunction("luaV_equalobj", "lvm.c", "409", (void*)luaV_equalobj,
                  types->toIlType<int>(), 3,
                  plua_State,
                  pTValue,
                  pTValue);

   DefineFunction("luaV_lessthan", "lvm.c", "366", (void*)luaV_lessthan,
                  types->toIlType<int>(), 3,
                  plua_State,
                  pTValue,
                  pTValue);

   DefineFunction("luaV_lessequal", "lvm.c", "386", (void*)luaV_lessequal,
                  types->toIlType<int>(), 3,
                  plua_State,
                  pTValue,
                  pTValue);

   // Lua VM macro wrappers to be called from JITed code

   DefineFunction("jit_setbvalue", __FILE__, "0", (void*)jit_setbvalue,
                  NoType, 2,
                  pTValue,
                  types->toIlType<int>());

   DefineFunction("jit_gettableProtected", __FILE__, "0", (void*)jit_gettableProtected,
                  luaTypes.StkId, 4,
                  plua_State,
                  pTValue,
                  pTValue,
                  pTValue);

   DefineFunction("jit_settableProtected", __FILE__, "0", (void*)jit_settableProtected,
                  luaTypes.StkId, 4,
                  plua_State,
                  pTValue,
                  pTValue,
                  pTValue);

   DefineFunction("jit_checkstack", __FILE__, "0", (void*)jit_checkstack,
                  NoType, 2,
                  plua_State,
                  types->toIlType<int>());

   DefineFunction("jit_luaC_upvalbarrier", __FILE__, "0", (void*)jit_luaC_upvalbarrier,
                  NoType, 2,
                  plua_State,
                  types->PointerTo(luaTypes.UpVal));

   // Lua VM interpreter helpers

   DefineFunction("vm_gettable", __FILE__, "0", (void*)vm_gettable,
                  luaTypes.StkId, 2,
                  plua_State,
                  luaTypes.Instruction);

   DefineFunction("vm_settable", __FILE__, "0", (void*)vm_settable,
                  luaTypes.StkId, 2,
                  plua_State,
                  luaTypes.Instruction);

   DefineFunction("vm_newtable", __FILE__, "0", (void*)vm_newtable,
                  luaTypes.StkId, 2,
                  plua_State,
                  luaTypes.Instruction);

   DefineFunction("vm_self", __FILE__, "0", (void*)vm_self,
                  luaTypes.StkId, 2,
                  plua_State,
                  luaTypes.Instruction);

   DefineFunction("vm_mod", __FILE__, "0", (void*)vm_mod,
                  luaTypes.StkId, 2,
                  plua_State,
                  luaTypes.Instruction);

   DefineFunction("vm_pow", __FILE__, "0", (void*)vm_pow,
                  luaTypes.StkId, 2,
                  plua_State,
                  luaTypes.Instruction);

   DefineFunction("vm_idiv", __FILE__, "0", (void*)vm_idiv,
                  luaTypes.StkId, 2,
                  plua_State,
                  luaTypes.Instruction);

   DefineFunction("vm_band", __FILE__, "0", (void*)vm_band,
                  luaTypes.StkId, 2,
                  plua_State,
                  luaTypes.Instruction);

   DefineFunction("vm_bor", __FILE__, "0", (void*)vm_bor,
                  luaTypes.StkId, 2,
                  plua_State,
                  luaTypes.Instruction);

   DefineFunction("vm_bxor", __FILE__, "0", (void*)vm_bxor,
                  luaTypes.StkId, 2,
                  plua_State,
                  luaTypes.Instruction);

   DefineFunction("vm_shl", __FILE__, "0", (void*)vm_shl,
                  luaTypes.StkId, 2,
                  plua_State,
                  luaTypes.Instruction);

   DefineFunction("vm_shr", __FILE__, "0", (void*)vm_shr,
                  luaTypes.StkId, 2,
                  plua_State,
                  luaTypes.Instruction);

   DefineFunction("vm_unm", __FILE__, "0", (void*)vm_unm,
                  luaTypes.StkId, 2,
                  plua_State,
                  luaTypes.Instruction);

   DefineFunction("vm_bnot", __FILE__, "0", (void*)vm_bnot,
                  luaTypes.StkId, 2,
                  plua_State,
                  luaTypes.Instruction);

   DefineFunction("vm_not", __FILE__, "0", (void*)vm_not,
                  luaTypes.StkId, 2,
                  plua_State,
                  luaTypes.Instruction);

   DefineFunction("vm_len", __FILE__, "0", (void*)vm_len,
                  luaTypes.StkId, 2,
                  plua_State,
                  luaTypes.Instruction);

   DefineFunction("vm_concat", __FILE__, "0", (void*)vm_concat,
                  luaTypes.StkId, 2,
                  plua_State,
                  luaTypes.Instruction);

   DefineFunction("vm_test", __FILE__, "0", (void*)vm_test,
                  Int32, 2,
                  plua_State,
                  luaTypes.Instruction);

   DefineFunction("vm_testset", __FILE__, "0", (void*)vm_testset,
                  Int32, 2,
                  plua_State,
                  luaTypes.Instruction);

   DefineFunction("vm_forloop", __FILE__, "0", (void*)vm_forloop,
                  Int32, 2,
                  plua_State,
                  luaTypes.Instruction);

   DefineFunction("vm_forprep", __FILE__, "0", (void*)vm_forprep,
                  NoType, 2,
                  plua_State,
                  luaTypes.Instruction);

   DefineFunction("vm_setlist", __FILE__, "0", (void*)vm_setlist,
                  NoType, 2,
                  plua_State,
                  luaTypes.Instruction);

   DefineFunction("vm_closure", __FILE__, "0", (void*)vm_closure,
                  luaTypes.StkId, 2,
                  plua_State,
                  luaTypes.Instruction);
}

bool Lua::FunctionBuilder::buildIL() {
   auto instructionCount = prototype->sizecode;
   auto instructions = prototype->code;
   bool result = true;

   datatypes = alloc_data_flow_array(instructionCount, prototype->maxstacksize);
   if (datatypes == nullptr) {
      return false;
   }

   complete_data_flow_analysis(prototype, 0, instructionCount, datatypes, true);

   TR::BytecodeBuilder** bytecodeBuilders = new TR::BytecodeBuilder*[instructionCount];
   for (auto i = 0; i < instructionCount; ++i) {
      auto opcode = GET_OPCODE(instructions[i]);
      bytecodeBuilders[i] = OrphanBytecodeBuilder(i, (char*)luaP_opnames[opcode]);
   }

   Store("ci", LoadIndirect("lua_State", "ci", Load("L")));         // ci = L->ci
   Store("base", LoadIndirect("CallInfo", "u.l.base", Load("ci"))); // base = ci->u.l.base

   Store("cl",jit_clLvalue(this,
                                LoadIndirect("CallInfo", "func",
                                             Load("ci"))));

   AllLocalsHaveBeenDefined();

   // Check that all parameters match their profiled data types.  If they do not bail to the interpreter
   TR::IlBuilder *mismatch = nullptr;
   if (prototype->numparams > 0) {
      bool didParamCheck = false;
      for (auto i = 0; i < prototype->numparams; i++) {
         if ((prototype->paramtypes[i] > LUA_TNONE) && (prototype->paramtypes[i] < LUA_NUMTAGS)) {
            didParamCheck = true;
            auto param = IndexAt(luaTypes.StkId,
                                 Load("base"),
                                 ConstInt32(i));
            auto param_tt = LoadIndirect("TValue", "tt_", param);
            auto prototpye_tt = Const(prototype->paramtypes[i]);
            IfThen(&mismatch,
                   NotEqualTo(param_tt, prototpye_tt));
         }
      }
      if (didParamCheck) {
         mismatch->Return();
      }
   }

   intType = ConstInt32(LUA_TNUMINT);
   fltType = ConstInt32(LUA_TNUMFLT);
   numType = ConstInt32(LUA_TNUMBER);

   setVMState(new OMR::VirtualMachineState{});
   AppendBuilder(bytecodeBuilders[0]);

   auto instructionIndex = GetNextBytecodeFromWorklist();
   while (result && (-1 != instructionIndex)) {
      auto instruction = instructions[instructionIndex];
      auto builder = bytecodeBuilders[instructionIndex];
      auto nextBuilder = (instructionIndex < instructionCount - 1) ? bytecodeBuilders[instructionIndex + 1] : nullptr;

#if DEBUG
      fprintf(stderr, "\t%s at %d ra %d ", (char*)luaP_opnames[GET_OPCODE(instructions[instructionIndex])], instructionIndex, GETARG_A(instruction));
#endif
      if (TraceEnabled_log()) {
         TraceIL_log("BC%d %s [MB %p]", builder->bcIndex(), (char*)luaP_opnames[GET_OPCODE(instruction)], this);
      }

      // ra = base + GETARG_A(i)
      auto arg_a = GETARG_A(instruction);
      builder->Store("ra",
      builder->      IndexAt(luaTypes.StkId,
      builder->              Load("base"),
      builder->              ConstInt32(arg_a)));

      switch (GET_OPCODE(instruction)) {
      case OP_MOVE:
         do_move(builder, instruction);
         break;
      case OP_LOADK:
         do_loadk(builder, instruction);
         break;
      case OP_LOADBOOL:
         do_loadbool(builder, bytecodeBuilders[instructionIndex + 2], instruction);
         break;
      case OP_LOADNIL:
         do_loadnil(builder, instruction);
         break;
      case OP_GETUPVAL:
         do_getupval(builder, instruction);
         break;
      case OP_GETTABUP:
         do_gettabup(builder, instruction);
         break;
      case OP_GETTABLE:
         do_gettable(builder, instruction);
         break;
      case OP_SETTABUP:
         do_settabup(builder, instruction);
         break;
      case OP_SETUPVAL:
         do_setupval(builder, instruction);
         break;
      case OP_SETTABLE:
         do_settable(builder, instruction);
         break;
      case OP_NEWTABLE:
         do_newtable(builder, instruction);
         break;
      case OP_SELF:
         do_self(builder, instruction);
         break;
      case OP_ADD:
         do_math(builder, instruction, instructionIndex);
         break;
      case OP_SUB:
         do_math(builder, instruction, instructionIndex);
         break;
      case OP_MUL:
         do_math(builder, instruction, instructionIndex);
         break;
      case OP_MOD:
         do_mod(builder, instruction);
         break;
      case OP_POW:
         do_pow(builder, instruction);
         break;
      case OP_DIV:
         do_div(builder, instruction, instructionIndex);
         break;
      case OP_BAND:
         do_band(builder, instruction);
         break;
      case OP_BOR:
         do_bor(builder, instruction);
         break;
      case OP_BXOR:
         do_bxor(builder, instruction);
         break;
      case OP_IDIV:
         do_idiv(builder, instruction);
         break;
      case OP_SHL:
         do_shl(builder, instruction);
         break;
      case OP_SHR:
         do_shr(builder, instruction);
         break;
      case OP_UNM:
         do_unm(builder, instruction);
         break;
      case OP_BNOT:
         do_bnot(builder, instruction);
         break;
      case OP_NOT:
         do_not(builder, instruction);
         break;
      case OP_LEN:
         do_len(builder, instruction);
         break;
      case OP_CONCAT:
         do_concat(builder, instruction);
         break;
      case OP_JMP:
         do_jmp(builder, instruction);
         builder->AddFallThroughBuilder(bytecodeBuilders[instructionIndex + 1 + GETARG_sBx(instruction)]);
         nextBuilder = nullptr;
         break;
      case OP_EQ:
         do_eq(builder, bytecodeBuilders[instructionIndex + 2], instruction, instructionIndex);
         break;
      case OP_LT:
         do_cmp("luaV_lessthan", builder, bytecodeBuilders[instructionIndex + 2], instruction, instructionIndex);
         break;
      case OP_LE:
         do_cmp("luaV_lessequal", builder, bytecodeBuilders[instructionIndex + 2], instruction, instructionIndex);
         break;
      case OP_TEST:
         do_test(builder, bytecodeBuilders[instructionIndex + 2], instruction);
         break;
      case OP_TESTSET:
         do_testset(builder, bytecodeBuilders[instructionIndex + 2], instruction);
         break;
      case OP_CALL:
         do_call(builder, instruction, instructionIndex);
         break;
      case OP_RETURN:
         do_return(builder, instruction);
         nextBuilder = nullptr;   // prevent addition of a fallthrough path
         break;
      case OP_FORLOOP:
         do_forloop(builder, bytecodeBuilders[instructionIndex + 1 + GETARG_sBx(instruction)], instruction, instructionIndex);
         break;
      case OP_FORPREP:
         do_forprep(builder, instruction, instructionIndex);
         builder->AddFallThroughBuilder(bytecodeBuilders[instructionIndex + 1 + GETARG_sBx(instruction)]);
         nextBuilder = nullptr;   // prevent addition of a fallthrough path
         break;
      case OP_TFORCALL:
         do_tforcall(builder, instruction);
         if (GET_OPCODE(instructions[instructionIndex + 1]) != OP_TFORLOOP) return false;
         break;
      case OP_TFORLOOP:
         bytecodeBuilders[instructionIndex + 1 + GETARG_sBx(instruction)]->setVMState(new OMR::VirtualMachineState{});
         addBytecodeBuilderToWorklist(bytecodeBuilders[instructionIndex + 1 + GETARG_sBx(instruction)]);
         do_tforloop(builder, bytecodeBuilders[instructionIndex + 1 + GETARG_sBx(instruction)]);
         break;
      case OP_SETLIST:
         do_setlist(builder, instruction);
         break;
      case OP_CLOSURE:
         do_closure(builder, instruction);
         break;
      case OP_VARARG:
         do_vararg(builder, instruction);
         break;
      default:
         result = false;
         break;
      }

      if (nextBuilder) {
         builder->AddFallThroughBuilder(nextBuilder);
      }
#if DEBUG
      fprintf(stderr, "\n");
#endif
      instructionIndex = GetNextBytecodeFromWorklist();
   }

   free_data_flow_array(datatypes, instructionCount);

   return result;
}


//~ opcode specific IL generation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bool Lua::FunctionBuilder::do_move(TR::BytecodeBuilder* builder, Instruction instruction) {
   // setobjs2s(L, ra, RB(i));
   jit_setobj(builder,
   builder->  Load("ra"),
              jit_R(builder, GETARG_B(instruction)));

#if DEBUG
   fprintf(stderr, " rb %d ", GETARG_B(instruction));
#endif

   return true;
}

bool Lua::FunctionBuilder::do_loadk(TR::BytecodeBuilder* builder, Instruction instruction) {
   // setobj2s(L, ra, RBx(i));
   jit_setobj(builder,
              builder->Load("ra"),
              jit_K(builder, GETARG_Bx(instruction)));

#if DEBUG
   fprintf(stderr, " rb %d", RKASK(GETARG_Bx(instruction)));
#endif

   return true;
}

bool Lua::FunctionBuilder::do_loadbool(TR::BytecodeBuilder* builder, TR::BytecodeBuilder* dest, Instruction instruction) {
   // setbvalue(ra, GETARG_B(i));
   builder->Call("jit_setbvalue", 2,
   builder->     Load("ra"),
   builder->     ConstInt32(GETARG_B(instruction)));

   // if (GETARG_C(i)) ci->u.l.savedpc++;
   builder->IfCmpNotEqualZero(&dest,
   builder->               ConstInt32(GETARG_C(instruction)));

   return true;
}

bool Lua::FunctionBuilder::do_loadnil(TR::BytecodeBuilder* builder, Instruction instruction) {
   auto setnils = OrphanBuilder();
   builder->ForLoopDown("b", &setnils,
   builder->            ConstInt32(GETARG_B(instruction) +1),
   builder->            ConstInt32(0),
   builder->            ConstInt32(1));

   setnils->StoreIndirect("TValue", "tt_",
   setnils->              Load("ra"),
   setnils->              ConstInt32(LUA_TNIL));
   setnils->Store("ra",
   setnils->      IndexAt(typeDictionary()->PointerTo(luaTypes.TValue),
   setnils->              Load("ra"),
   setnils->              ConstInt32(1)));

   return true;
}

bool Lua::FunctionBuilder::do_getupval(TR::BytecodeBuilder* builder, Instruction instruction) {
   //setobj2s(L, ra, cl->upvals[b]->v);
   auto pUpVal = typeDictionary()->PointerTo(luaTypes.UpVal);
   auto ppUpVal = typeDictionary()->PointerTo(pUpVal);
   builder->Store("upval",
   builder->      LoadIndirect("UpVal", "v",
   builder->                   LoadAt(pUpVal,
   builder->                          IndexAt(ppUpVal,
   builder->                                  Add(
   builder->                                      Load("cl"),
   builder->                                      ConstInt64(typeDictionary()->OffsetOf("LClosure", "upvals"))),
   builder->                                  ConstInt64(GETARG_B(instruction))))));

   jit_setobj(builder,
              builder->Load("ra"),
              builder->Load("upval"));
   return true;
}

bool Lua::FunctionBuilder::do_gettabup(TR::BytecodeBuilder* builder, Instruction instruction) {
   // upval = cl->upvals[GETARG_B(instruction)]->v
   auto pUpVal = typeDictionary()->PointerTo(luaTypes.UpVal);
   auto ppUpVal = typeDictionary()->PointerTo(pUpVal);
   builder->Store("upval",
   builder->      LoadIndirect("UpVal", "v",
   builder->                   LoadAt(pUpVal,
   builder->                          IndexAt(ppUpVal,
   builder->                                  Add(
   builder->                                      Load("cl"),
   builder->                                      ConstInt64(typeDictionary()->OffsetOf("LClosure", "upvals"))),
   builder->                                  ConstInt64(GETARG_B(instruction))))));

   auto rc = jit_RK(builder, GETARG_C(instruction));

   builder->Store("base",
   builder->      Call("jit_gettableProtected", 4,
   builder->           Load("L"),
   builder->           Load("upval"),
                       rc,
   builder->           Load("ra")));

   return true;
}

bool Lua::FunctionBuilder::do_gettable(TR::BytecodeBuilder* builder, Instruction instruction) {
   builder->Store("base",
   builder->      Call("vm_gettable", 2,
   builder->           Load("L"),
   builder->           ConstInt32(instruction)));

   return true;
}

bool Lua::FunctionBuilder::do_settabup(TR::BytecodeBuilder* builder, Instruction instruction) {
   // upval = cl->upvals[GETARG_A(instruction)]->v
   auto pUpVal = typeDictionary()->PointerTo(luaTypes.UpVal);
   auto ppUpVal = typeDictionary()->PointerTo(pUpVal);
   builder->Store("upval",
   builder->      LoadIndirect("UpVal", "v",
   builder->                   LoadAt(pUpVal,
   builder->                          IndexAt(ppUpVal,
   builder->                                  Add(
   builder->                                      Load("cl"),
   builder->                                      ConstInt64(typeDictionary()->OffsetOf("LClosure", "upvals"))),
   builder->                                  ConstInt64(GETARG_A(instruction))))));

   auto rb = jit_RK(builder, GETARG_B(instruction));
   auto rc = jit_RK(builder, GETARG_C(instruction));
   
   builder->Store("base",
   builder->      Call("jit_settableProtected", 4,
   builder->           Load("L"),
   builder->           Load("upval"),
                       rb,
                       rc));

   return true;
}

bool Lua::FunctionBuilder::do_setupval(TR::BytecodeBuilder* builder, Instruction instruction) {
   auto pUpVal = typeDictionary()->PointerTo(luaTypes.UpVal);
   auto ppUpVal = typeDictionary()->PointerTo(pUpVal);

   //UpVal *uv = cl->upvals[GETARG_B(i)];
   builder->Store("uv",
   builder->      LoadAt(pUpVal,
   builder->             IndexAt(ppUpVal,
   builder->                     Add(
   builder->                         Load("cl"),
   builder->                         ConstInt64(typeDictionary()->OffsetOf("LClosure", "upvals"))),
   builder->                     ConstInt64(GETARG_B(instruction)))));

   builder->Store("upval",
   builder->      LoadIndirect("UpVal", "v",
   builder->                   Load("uv")));
   //setobj(L, uv->v, ra);
   jit_setobj(builder,
              builder->Load("upval"),
              builder->Load("ra"));

   //luaC_upvalbarrier(L, uv);
   builder->Call("jit_luaC_upvalbarrier", 2,
   builder->     Load("L"),
   builder->     Load("uv"));

   return true;
}

bool Lua::FunctionBuilder::do_settable(TR::BytecodeBuilder* builder, Instruction instruction) {
   builder->Store("base",
   builder->      Call("vm_settable", 2,
   builder->           Load("L"),
   builder->           ConstInt32(instruction)));

   return true;
}

bool Lua::FunctionBuilder::do_newtable(TR::BytecodeBuilder* builder, Instruction instruction) {
   builder->Store("base",
   builder->      Call("vm_newtable", 2,
   builder->           Load("L"),
   builder->           ConstInt32(instruction)));

   return true;
}

bool  Lua::FunctionBuilder::do_self(TR::BytecodeBuilder* builder, Instruction instruction) {
   builder->Store("base",
   builder->      Call("vm_self", 2,
   builder->           Load("L"),
   builder->           ConstInt32(instruction)));

   return true;
}

void Lua::FunctionBuilder::jit_integer_math(TR::IlBuilder *builder, Instruction instruction, int result_arg, int left_arg, int right_arg) {
   TR::IlValue *result_reg = jit_R(builder, result_arg);
   TR::IlValue *leftint = jit_get_intvalue(builder, left_arg);
   TR::IlValue *rightint = jit_get_intvalue(builder, right_arg);
   TR::IlValue *result = jit_performmath(builder, instruction, leftint, rightint);

   jit_set_intvalue(builder, result_reg, result);
   jit_set_type(builder, result_reg, intType);
}

void Lua::FunctionBuilder::jit_integer_cmp(TR::IlBuilder *builder, Instruction instruction, int left_arg, int right_arg) {
   TR::IlValue *leftint = jit_get_intvalue(builder, left_arg);
   TR::IlValue *rightint = jit_get_intvalue(builder, right_arg);
   jit_performcmp(builder, instruction, leftint, rightint);
}

void Lua::FunctionBuilder::jit_number_math(TR::IlBuilder *builder, Instruction instruction, int result_arg, int left_arg, int left_type, int right_arg, int right_type) {
   TR::IlValue *result_reg = jit_R(builder, result_arg);
   TR::IlValue *leftvalue = jit_tonumber(builder, left_arg, left_type);
   TR::IlValue *rightvalue = jit_tonumber(builder, right_arg, right_type);
   TR::IlValue *result = jit_performmath(builder, instruction, leftvalue, rightvalue);

   jit_set_fltvalue(builder, result_reg, result);
   jit_set_type(builder, result_reg, fltType);
}

void Lua::FunctionBuilder::jit_number_cmp(TR::IlBuilder *builder, Instruction instruction, int left_arg, int left_type, int right_arg, int right_type) {
   TR::IlValue *leftvalue = jit_tonumber(builder, left_arg, left_type);
   TR::IlValue *rightvalue = jit_tonumber(builder, right_arg, right_type);
   jit_performcmp(builder, instruction, leftvalue, rightvalue);
}

bool Lua::FunctionBuilder::do_math(TR::BytecodeBuilder* builder, Instruction instruction, int instructionIndex) {
   // rb = RKB(i);
   // rc = RKC(i);
   int arg_a = GETARG_A(instruction);
   int arg_b = GETARG_B(instruction);
   int arg_c = GETARG_C(instruction);
   TR::IlValue *ra = jit_R(builder, arg_a);
   TR::IlValue *rb = jit_RK(builder, arg_b);
   TR::IlValue *rc = jit_RK(builder, arg_c);

#if DEBUG
   fprintf(stderr, " rb %d (%d) rc %d  (%d)", arg_b, get_data_type(datatypes, instructionIndex, arg_b), arg_c, get_data_type(datatypes, instructionIndex, arg_c));
#endif

   int b_type = get_data_type(datatypes, instructionIndex, arg_b);
   int c_type = get_data_type(datatypes, instructionIndex, arg_c);
   if (is_data_type_int(b_type) && is_data_type_int(c_type)) {
      jit_integer_math(builder, instruction, arg_a, arg_b, arg_c);
   } else if (is_data_type_number(b_type) && is_data_type_number(c_type)) {
      jit_number_math(builder, instruction, arg_a, arg_b, b_type, arg_c, c_type);
   } else {
      // if (ttisinteger(rb) && ttisinteger(rc))
      auto rbtype = jit_get_type(builder, arg_b);
      auto isrbint = jit_isinteger(builder, rbtype);
      auto rctype = jit_get_type(builder, arg_c);
      auto isrcint = jit_isinteger(builder, rctype);

      auto ints = builder->OrphanBuilder();
      auto notints = builder->OrphanBuilder();
      builder->IfThenElse(&ints, &notints,
      builder->           And(isrbint, isrcint));

      // lua_Integer ib = ivalue(rb); lua_Integer ic = ivalue(rc);
      // setivalue(ra, intop([+,-,*], ib, ic));
      TR::IlValue *rbint = jit_get_intvalue(ints, arg_b);
      TR::IlValue *rcint = jit_get_intvalue(ints, arg_c);
      TR::IlValue *intresult = jit_performmath(ints, instruction, rbint, rcint);

      jit_set_intvalue(ints, ra, intresult);
      jit_set_type(ints, ra, intType);

      // else if (ttisnumber(rb) && ttisnumber(rc))
      auto isrbnum = jit_isnumber(notints, rbtype);
      auto isrcnum = jit_isnumber(notints, rctype);

      auto nums = notints->OrphanBuilder();
      auto notnums = notints->OrphanBuilder();
      notints->IfThenElse(&nums, &notnums,
      notints->           And(isrbnum, isrcnum));

      // setfltvalue(ra, luai_num[add,sub,mul](L,tonumber(rb), tonumber(rc)));
      TR::IlValue *rbnum = jit_tonumber(nums, rb, rbtype);
      TR::IlValue *rcnum = jit_tonumber(nums, rc, rctype);
      TR::IlValue *numresult = jit_performmath(nums, instruction, rbnum, rcnum);

      jit_set_fltvalue(nums, ra, numresult);
      jit_set_type(nums, ra, fltType);

      // else { Protect(luaT_trybinTM(L, rb, rc, ra, (TM_ADD | TM_SUB | TM_MUL))); }
      int operation = 0;
      switch (GET_OPCODE(instruction)) {
      case OP_ADD:
         operation = TM_ADD;
         break;
      case OP_SUB:
         operation = TM_SUB;
         break;
      case OP_MUL:
         operation = TM_MUL;
         break;
      default:
         break;
      }
      notnums->Call("luaT_trybinTM", 5,
      notnums->     Load("L"),
                    rb,
                    rc,
                    ra,
      notnums->     Const(operation));
      jit_Protect(notnums);
   }

   return true;
}

bool Lua::FunctionBuilder::do_mod(TR::BytecodeBuilder* builder, Instruction instruction) {
   builder->Store("base",
   builder->      Call("vm_mod", 2,
   builder->           Load("L"),
   builder->           ConstInt32(instruction)));

#if DEBUG
   fprintf(stderr, " rb %d rc %d ", GETARG_B(instruction), GETARG_C(instruction));
#endif

   return true;
}

bool Lua::FunctionBuilder::do_pow(TR::BytecodeBuilder* builder, Instruction instruction) {
   builder->Store("base",
   builder->      Call("vm_pow", 2,
   builder->           Load("L"),
   builder->           ConstInt32(instruction)));

#if DEBUG
   fprintf(stderr, " rb %d (%d) rc %d  (%d)", GETARG_B(instruction), GETARG_C(instruction));
#endif

   return true;
}

bool Lua::FunctionBuilder::do_div(TR::BytecodeBuilder* builder, Instruction instruction, int instructionIndex) {
   // rb = RKB(i);
   // rc = RKC(i);
   int arg_a = GETARG_A(instruction);
   int arg_b = GETARG_B(instruction);
   int arg_c = GETARG_C(instruction);
   TR::IlValue *ra = jit_RK(builder, arg_a);
   TR::IlValue *rb = jit_RK(builder, arg_b);
   TR::IlValue *rc = jit_RK(builder, arg_c);

#if DEBUG
   fprintf(stderr, " rb %d (%d) rc %d  (%d)", arg_b, get_data_type(datatypes, instructionIndex, arg_b), arg_c, get_data_type(datatypes, instructionIndex, arg_c));
#endif

   int b_type = get_data_type(datatypes, instructionIndex, arg_b);
   int c_type = get_data_type(datatypes, instructionIndex, arg_c);
   if (is_data_type_number(b_type) && is_data_type_number(c_type)) {
      jit_number_math(builder, instruction, arg_a, arg_b, b_type, arg_c, c_type);
   } else {
      // if (ttisnumber(rb) && ttisnumber(rc))
      auto rbtype = jit_get_type(builder, arg_b);
      auto isrbnum = jit_isnumber(builder, rbtype);
      auto rctype = jit_get_type(builder, arg_c);
      auto isrcnum = jit_isnumber(builder, rctype);

      auto nums = builder->OrphanBuilder();
      auto notnums = builder->OrphanBuilder();
      builder->IfThenElse(&nums, &notnums,
      builder->           And(isrbnum, isrcnum));

      // setfltvalue(ra, luai_numdiv(L,tonumber(rb), tonumber(rc)));
      TR::IlValue *rbnum = jit_tonumber(nums, rb, rbtype);
      TR::IlValue *rcnum = jit_tonumber(nums, rc, rctype);
      TR::IlValue *numresult = jit_performmath(nums, instruction, rbnum, rcnum);

      jit_set_fltvalue(nums, ra, numresult);
      jit_set_type(nums, ra, fltType);

      // else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_DIV)); }
      notnums->Call("luaT_trybinTM", 5,
      notnums->     Load("L"),
                    rb,
                    rc,
                    ra,
      notnums->     Const(TM_DIV));
      jit_Protect(notnums);
   }

   return true;
}

bool Lua::FunctionBuilder::do_idiv(TR::BytecodeBuilder* builder, Instruction instruction) {
   builder->Store("base",
   builder->      Call("vm_idiv", 2,
   builder->           Load("L"),
   builder->           ConstInt32(instruction)));

   return true;
}

bool Lua::FunctionBuilder::do_band(TR::BytecodeBuilder* builder, Instruction instruction) {
   builder->Store("base",
   builder->      Call("vm_band", 2,
   builder->           Load("L"),
   builder->           ConstInt32(instruction)));

   return true;
}

bool Lua::FunctionBuilder::do_bor(TR::BytecodeBuilder* builder, Instruction instruction) {
   builder->Store("base",
   builder->      Call("vm_bor", 2,
   builder->           Load("L"),
   builder->           ConstInt32(instruction)));

   return true;
}

bool Lua::FunctionBuilder::do_bxor(TR::BytecodeBuilder* builder, Instruction instruction) {
   builder->Store("base",
   builder->      Call("vm_bxor", 2,
   builder->           Load("L"),
   builder->           ConstInt32(instruction)));

   return true;
}

bool Lua::FunctionBuilder::do_shl(TR::BytecodeBuilder* builder, Instruction instruction) {
   builder->Store("base",
   builder->      Call("vm_shl", 2,
   builder->           Load("L"),
   builder->           ConstInt32(instruction)));

   return true;
}

bool Lua::FunctionBuilder::do_shr(TR::BytecodeBuilder* builder, Instruction instruction) {
   builder->Store("base",
   builder->      Call("vm_shr", 2,
   builder->           Load("L"),
   builder->           ConstInt32(instruction)));

   return true;
}

bool Lua::FunctionBuilder::do_unm(TR::BytecodeBuilder* builder, Instruction instruction) {
   builder->Store("base",
   builder->      Call("vm_unm", 2,
   builder->           Load("L"),
   builder->           ConstInt32(instruction)));

   return true;
}

bool Lua::FunctionBuilder::do_bnot(TR::BytecodeBuilder* builder, Instruction instruction) {
   builder->Store("base",
   builder->      Call("vm_bnot", 2,
   builder->           Load("L"),
   builder->           ConstInt32(instruction)));

   return true;
}

bool Lua::FunctionBuilder::do_not(TR::BytecodeBuilder* builder, Instruction instruction) {
   builder->Store("base",
   builder->      Call("vm_not", 2,
   builder->           Load("L"),
   builder->           ConstInt32(instruction)));

   return true;
}

bool Lua::FunctionBuilder::do_len(TR::BytecodeBuilder* builder, Instruction instruction) {
   builder->Store("base",
   builder->      Call("vm_len", 2,
   builder->           Load("L"),
   builder->           ConstInt32(instruction)));

   return true;
}

bool Lua::FunctionBuilder::do_concat(TR::BytecodeBuilder* builder, Instruction instruction) {
   builder->Store("base",
   builder->      Call("vm_concat", 2,
   builder->           Load("L"),
   builder->           ConstInt32(instruction)));

   return true;
}

bool Lua::FunctionBuilder::do_jmp(TR::BytecodeBuilder* builder, Instruction instruction) {
#ifdef FULL_IL_GEN
   builder->Store("a",
   builder->      ConstInt32(GETARG_A(instruction)));

   TR::IlBuilder* call_luaF_close = OrphanBuilder();
   builder->IfThen(&call_luaF_close,
   builder->       NotEqualTo(
   builder->               Load("a"),
   builder->               ConstInt32(0)));

   call_luaF_close->Call("luaF_close", 2,
   call_luaF_close->     Load("L"),
   call_luaF_close->     Add(
   call_luaF_close->         LoadIndirect("CallInfo", "u.l.base",
   call_luaF_close->                      Load("ci")),
   call_luaF_close->         Sub(
   call_luaF_close->             Load("a"),
   call_luaF_close->             ConstInt32(1))));
#else
   int arg_a = GETARG_A(instruction);
   if (arg_a != 0) {
      builder->Call("luaF_close", 2,
      builder->     Load("L"),
      builder->     Add(
      builder->         LoadIndirect("CallInfo", "u.l.base",
      builder->                      Load("ci")),
      builder->         ConstInt32(arg_a - 1)));
   }
#endif

   return true;
}

bool Lua::FunctionBuilder::do_eq(TR::BytecodeBuilder* builder, TR::BytecodeBuilder* dest, Instruction instruction, int instructionIndex) {
   int b_type = get_data_type(datatypes, instructionIndex, GETARG_B(instruction));
   int c_type = get_data_type(datatypes, instructionIndex, GETARG_C(instruction));
   if (is_data_type_int(b_type) && is_data_type_int(c_type)) {
      jit_integer_cmp(builder, instruction, GETARG_B(instruction), GETARG_C(instruction));
   } else if (is_data_type_flt(b_type) && is_data_type_flt(c_type)) {
      jit_number_cmp(builder, instruction, GETARG_B(instruction), b_type, GETARG_C(instruction), c_type);
   } else {
      TR::IlValue *left = jit_RK(builder, GETARG_B(instruction));
      TR::IlValue *right = jit_RK(builder, GETARG_C(instruction));

      builder->Store("cmp",
      builder->      Call("luaV_equalobj", 3,
      builder->           Load("L"),
                          left,
                          right));

      jit_Protect(builder); // from code inspection it appears like the comparison call
                            // is the only thing that needs to be Protected because it
                            // calls `luaT_callTM`, which can reallocat the stack
   }

   // if (cmp != GETARG_A(i)) ci->u.l.savedpc++;
   builder->IfCmpNotEqual(&dest,
   builder->              Load("cmp"),
   builder->              ConstInt32(GETARG_A(instruction)));

   return true;
}

bool Lua::FunctionBuilder::do_cmp(const char* cmpFunc, TR::BytecodeBuilder* builder, TR::BytecodeBuilder* dest, Instruction instruction, int instructionIndex) {
   int b_type = get_data_type(datatypes, instructionIndex, GETARG_B(instruction));
   int c_type = get_data_type(datatypes, instructionIndex, GETARG_C(instruction));
   if (is_data_type_int(b_type) && is_data_type_int(c_type)) {
      jit_integer_cmp(builder, instruction, GETARG_B(instruction), GETARG_C(instruction));
   } else if (is_data_type_number(b_type) && is_data_type_number(c_type)) {
      jit_number_cmp(builder, instruction, GETARG_B(instruction), b_type, GETARG_C(instruction), c_type);
   } else {
      // if (ttisinteger(rb) && ttisinteger(rc))
      TR::IlValue *left = jit_RK(builder, GETARG_B(instruction));
      TR::IlValue *right = jit_RK(builder, GETARG_C(instruction));
      auto lefttype = jit_get_type(builder, GETARG_B(instruction));
      auto righttype = jit_get_type(builder, GETARG_C(instruction));
      auto isleftint = jit_isinteger(builder, lefttype);
      auto isrightint = jit_isinteger(builder, righttype);

      TR::IlBuilder *cmpints = nullptr;
      TR::IlBuilder *notints = nullptr;
      builder->IfThenElse(&cmpints, &notints,
      builder->           And(isleftint, isrightint));

      // return (int)l [<,<=] (int)r
      TR::IlValue *leftint = jit_get_intvalue(cmpints, GETARG_B(instruction));
      TR::IlValue *rightint = jit_get_intvalue(cmpints, GETARG_C(instruction));

      jit_performcmp(cmpints, instruction, leftint, rightint);

      // else if (ttisnumber(rb) && ttisnumber(rc))
      auto isleftnum = jit_isnumber(notints, lefttype);
      auto isrightnum = jit_isnumber(notints, righttype);

      TR::IlBuilder *cmpnums = nullptr;
      TR::IlBuilder *notnums = nullptr;
      notints->IfThenElse(&cmpnums, &notnums,
      notints->           And(isleftnum, isrightnum));

      // return (float)l [<,<=] (float)r
      TR::IlValue *leftnum = jit_tonumber(cmpnums, left, lefttype);
      TR::IlValue *rightnum = jit_tonumber(cmpnums, right, righttype);

      jit_performcmp(cmpnums, instruction, leftnum, rightnum);

      // else call comparison helper
      notnums->Store("cmp",
      notnums->      Call(cmpFunc, 3,
      notnums->           Load("L"),
                          left,
                          right));

      jit_Protect(notnums); // from code inspection it appears like the comparison call
                            // is the only thing that needs to be Protected because it
                            // calls `luaT_callTM`, which can reallocat the stack
   }

   // if (cmp != GETARG_A(i)) ci->u.l.savedpc++;
   builder->IfCmpNotEqual(&dest,
   builder->              Load("cmp"),
   builder->              ConstInt32(GETARG_A(instruction)));

   return true;
}

bool Lua::FunctionBuilder::do_test(TR::BytecodeBuilder* builder, TR::BytecodeBuilder* dest, Instruction instruction) {
   builder->Store("cmp",
   builder->      Call("vm_test", 2,
   builder->           Load("L"),
   builder->           ConstInt32(instruction)));

   builder->IfCmpNotEqualZero(&dest,
   builder->              Load("cmp"));

   return true;
}

bool Lua::FunctionBuilder::do_testset(TR::BytecodeBuilder* builder, TR::BytecodeBuilder* dest, Instruction instruction) {
   builder->Store("cmp",
   builder->      Call("vm_testset", 2,
   builder->           Load("L"),
   builder->           ConstInt32(instruction)));

   builder->IfCmpNotEqualZero(&dest,
   builder->              Load("cmp"));

   return true;
}

bool Lua::FunctionBuilder::do_call(TR::BytecodeBuilder* builder, Instruction instruction, int instructionIndex) {
   // b = GETARG_B(i);
   builder->Store("b",
   builder->      ConstInt32(GETARG_B(instruction)));

   // nresults = GETARG_C(i) - 1;
   builder->Store("nresults",
   builder->      Sub(
   builder->          ConstInt32(GETARG_C(instruction)),
   builder->          ConstInt32(1)));

   // if (b != 0) L->top = ra+b;
   TR::IlBuilder* settop = builder->OrphanBuilder();
   builder->IfThen(&settop,
   builder->       NotEqualTo(
   builder->                  Load("b"),
   builder->                  ConstInt32(0)));

   settop->StoreIndirect("lua_State", "top",
   settop->               Load("L"),
   settop->               IndexAt(luaTypes.StkId,
   settop->                   Load("ra"),
   settop->                   Load("b")));

   //before we do any calls set savedpc so hooks can work
   jit_setsavedpc(builder, instructionIndex);

   // if (luaD_precall(L, ra, nresults))
   auto isCFunc = builder->Call("luaD_precall", 3,
                  builder->     Load("L"),
                  builder->     Load("ra"),
                  builder->     Load("nresults"));

   TR::IlBuilder* cFunc = builder->OrphanBuilder();
   TR::IlBuilder* luaFunc = builder->OrphanBuilder();
   builder->IfThenElse(&cFunc, &luaFunc, isCFunc);

   // if (nresults >= 0) L->top = ci->top;
   // Protect((void)0);
   TR::IlBuilder* adjusttop = cFunc->OrphanBuilder();
   cFunc->IfThen(&adjusttop,
   cFunc->       Or(
   cFunc->          GreaterThan(
   cFunc->                      Load("nresults"),
   cFunc->                      ConstInt32(0)),
   cFunc->          EqualTo(
   cFunc->                  Load("nresults"),
   cFunc->                  ConstInt32(0))));

   adjusttop->StoreIndirect("lua_State", "top",
   adjusttop->              Load("L"),
   adjusttop->              LoadIndirect("CallInfo", "top",
   adjusttop->                           Load("ci")));

   jit_Protect(cFunc);

   // p = getproto(L->ci->func);
   luaFunc->Store("p",
   luaFunc->      LoadIndirect("LClosure", "p",jit_clLvalue(luaFunc,
   luaFunc->                   LoadIndirect("CallInfo", "func",
   luaFunc->                                LoadIndirect("lua_State", "ci",
   luaFunc->                                             Load("L"))))));

   // f (!(p->jitflags & LUA_JITBLACKLIST) &&  p->callcounter == 0 &&  p->compiledcode == NULL)
   auto notblacklisted =
   luaFunc->EqualTo(
   luaFunc->        And(
   luaFunc->            LoadIndirect("Proto", "jitflags",
   luaFunc->                         Load("p")),
   luaFunc->            ConstInt32(LUA_JITBLACKLIST)),
   luaFunc->        ConstInt32(0));
   auto callcounter0 =
   luaFunc->EqualTo(
   luaFunc->        LoadIndirect("Proto", "callcounter",
   luaFunc->                     Load("p")),
   luaFunc->        ConstInt16(0));
   auto isnotcompiled =
   luaFunc->EqualTo(
   luaFunc->        LoadIndirect("Proto", "compiledcode",
   luaFunc->                     Load("p")),
   luaFunc->        ConstInt32(0));
   auto readycompile =
   luaFunc->And(notblacklisted,
   luaFunc->    And(callcounter0, isnotcompiled));
   TR::IlBuilder* trycompile = luaFunc->OrphanBuilder();
   luaFunc->IfThen(&trycompile, readycompile);

   // luaJ_compile(p);
   trycompile->Call("luaJ_compile", 1,
   trycompile->     Load("p"));

   // LUAJ_BLACKLIST(p); // p->jitflags = p->jitflags | LUA_JITBLACKLIST
   trycompile->StoreIndirect("Proto", "jitflags",
   trycompile->              Load("p"),
   trycompile->              Or(
   trycompile->                 LoadIndirect("Proto", "jitflags",
   trycompile->                           Load("p")),
   trycompile->                 ConstInt32(LUA_JITBLACKLIST)));

   // if (p->compiledcode) { p->compiledcode(L); }
   TR::IlBuilder* callcompiled = luaFunc->OrphanBuilder();
   TR::IlBuilder* callinterpreter = luaFunc->OrphanBuilder();
   luaFunc->IfThenElse(&callcompiled, &callinterpreter,
   luaFunc->           NotEqualTo(
   luaFunc->           LoadIndirect("Proto", "compiledcode",
   luaFunc->                        Load("p")),
   luaFunc->           ConstInt32(0)));

   callcompiled->ComputedCall("compiledbody", 2,
   callcompiled->             LoadIndirect("Proto", "compiledcode",
   callcompiled->                          Load("p")),
   callcompiled->             Load("L"));

   // else { luaV_execute(L); }
   callinterpreter->Call("luaV_execute", 1,
   callinterpreter->     Load("L"));

   jit_Protect(builder);

   return true;
}

bool Lua::FunctionBuilder::do_return(TR::BytecodeBuilder* builder, Instruction instruction) {
   // b = GETARG_B(i)
   auto arg_b = GETARG_B(instruction);
   builder->Store("b",
   builder->      ConstInt32(arg_b));

   // if (cl->p->sizep > 0) luaF_close(L, base);
   auto close_upvals = builder->OrphanBuilder();
   builder->IfThen(&close_upvals,
   builder->       GreaterThan(
   builder->                   LoadIndirect("Proto", "sizep",
   builder->                                LoadIndirect("LClosure", "p",
   builder->                                             Load("cl"))),
   builder->                   ConstInt32(0)));

   close_upvals->Call("luaF_close", 2,
   close_upvals->     Load("L"),
   close_upvals->     Load("base"));

   // b = luaD_poscall(L, ci, ra, (b != 0 ? b - 1 : cast_int(L->top - ra)))
   builder->Store("b",
   builder->      Call("luaD_poscall", 4,
   builder->           Load("L"),
   builder->           Load("ci"),
   builder->           Load("ra"),
                       (arg_b != 0 ?
   builder->                        ConstInt32(arg_b - 1) :
   builder->                        IndexAt(luaTypes.StkId,
   builder->                                LoadIndirect("lua_State", "top",
   builder->                                             Load("L")),
   builder->                                Sub(
   builder->                                    ConstInt32(0),
   builder->                                    Load("ra"))))));

   // Cheat: because of where the JIT dispatch happens in the VM, a JITed function can
   //        never be a fresh interpreter invocation. We can therefore safely skip the
   //        check and do the cleanup.
   //
   // Note:  because of where the JIT dispatch happens in the VM, the `ci = L->ci` is
   //        done by the interpreter immediately after the JIT/JITed function returns.

   // if (b) L->top = ci->top;
   TR::IlBuilder* resetTop = nullptr;
   builder->IfThen(&resetTop,
   builder->       NotEqualTo(
   builder->                  Load("b"),
   builder->                  ConstInt32(0)));

   resetTop->StoreIndirect("lua_State", "top",
   resetTop->              Load("L"),
   resetTop->              LoadIndirect("CallInfo", "top",
   resetTop->                           Load("ci")));

   builder->Return();
   
   return true;
}

bool Lua::FunctionBuilder::do_forloop(TR::BytecodeBuilder* builder, TR::BytecodeBuilder* loopStart, Instruction instruction, int instructionIndex) {
   int raIndex = GETARG_A(instruction);
   TR::IlValue *index = jit_R(builder, raIndex);
   TR::IlValue *limit = jit_R(builder, raIndex + 1);
   TR::IlValue *step = jit_R(builder, raIndex + 2);
   TR::IlValue *externalIndex = jit_R(builder, raIndex + 3);

   TR::IlBuilder *intloop = nullptr;
   int index_type = get_data_type(datatypes, instructionIndex, raIndex);
   if (is_data_type_int(index_type)) {
      // If data flow analysis say that index is an int then do not generate any checks
      intloop = builder;
   } else {
      TR::IlValue *isindexint = jit_checktag(builder, index, intType);
      TR::IlBuilder *notints = nullptr;
      builder->IfThenElse(&intloop, &notints, isindexint);

      notints->Store("continueLoop",
      notints->      Call("vm_forloop", 2,
      notints->           Load("L"),
      notints->           ConstInt32(instruction)));
   }

   TR::IlValue *indexValue = intloop->LoadIndirect("Value", "i", StructFieldAddress(intloop, "TValue", "value_", index));
   TR::IlValue *limitValue = intloop->LoadIndirect("Value", "i", StructFieldAddress(intloop, "TValue", "value_", limit));
   TR::IlValue *stepValue = intloop->LoadIndirect("Value", "i", StructFieldAddress(intloop, "TValue", "value_", step));
   TR::IlValue *nextIndexValue = intloop->Add(indexValue, stepValue);

   //if ((0 < step) ? (idx <= limit) : (limit <= idx))
   TR::IlBuilder *negativeStepInt = nullptr;
   TR::IlBuilder *positiveStepInt = nullptr;
   intloop->IfThenElse(&positiveStepInt, &negativeStepInt,
   intloop->          LessThan(
   intloop->                   ConvertTo(luaTypes.lua_Integer,
   intloop->                             ConstInt32(0)), stepValue));

   TR::IlBuilder *continueLoopInt = nullptr;
   TR::IlBuilder *breakLoopInt = nullptr;
   negativeStepInt->IfThenElse(&breakLoopInt, &continueLoopInt,
   negativeStepInt->           GreaterThan(limitValue, nextIndexValue));
   positiveStepInt->IfThenElse(&breakLoopInt, &continueLoopInt,
   positiveStepInt->           GreaterThan(nextIndexValue, limitValue));

   breakLoopInt->Store("continueLoop",
   breakLoopInt->      ConstInt32(0));

   continueLoopInt->StoreIndirect("Value", "i", StructFieldAddress(continueLoopInt, "TValue", "value_", index), nextIndexValue);
   /* Do NOT have to set type on "index" as it is already an int */
   continueLoopInt->StoreIndirect("Value", "i", StructFieldAddress(continueLoopInt, "TValue", "value_", externalIndex), nextIndexValue);
   continueLoopInt->StoreIndirect("TValue", "tt_", externalIndex, intType);
   continueLoopInt->Store("continueLoop",
   continueLoopInt->      ConstInt32(1));

   builder->IfCmpNotEqualZero(&loopStart,
   builder->              Load("continueLoop"));

   return true;
}

bool Lua::FunctionBuilder::do_forprep(TR::BytecodeBuilder* builder, Instruction instruction, int instructionIndex) {
   int raIndex = GETARG_A(instruction);
   TR::IlValue *index = jit_R(builder, raIndex);
   TR::IlValue *limit = jit_R(builder, raIndex + 1);
   TR::IlValue *step = jit_R(builder, raIndex + 2);
   TR::IlBuilder *intloop = nullptr;
   int index_type = get_data_type(datatypes, instructionIndex, raIndex);
   int limit_type = get_data_type(datatypes, instructionIndex, raIndex + 1);
   int step_type = get_data_type(datatypes, instructionIndex, raIndex + 2);
   if (is_data_type_int(index_type) && is_data_type_int(limit_type) && is_data_type_int(step_type)) {
      // If data flow analysis say that index, limit and step are ints then do not generate any checks
      intloop = builder;
   } else {
      TR::IlValue *isindexint = jit_checktag(builder, index, intType);
      TR::IlValue *islimitint = jit_checktag(builder, limit, intType);
      TR::IlValue *isstepint = jit_checktag(builder, step, intType);

      TR::IlBuilder *notints = nullptr;
      builder->IfThenElse(&intloop, &notints,
      builder->           And(isindexint,
      builder->               And(islimitint, isstepint)));

      notints->Call("vm_forprep", 2,
      notints->     Load("L"),
      notints->     ConstInt32(instruction));
   }


   TR::IlValue *indexValue = intloop->LoadIndirect("Value", "i", StructFieldAddress(intloop, "TValue", "value_", index));
   TR::IlValue *stepValue = intloop->LoadIndirect("Value", "i", StructFieldAddress(intloop, "TValue", "value_", step));
   TR::IlValue *startIndex = intloop->Sub(indexValue, stepValue);
   intloop->StoreIndirect("Value", "i", StructFieldAddress(intloop, "TValue", "value_", index), startIndex);

   return true;
}

bool Lua::FunctionBuilder::do_tforcall(TR::BytecodeBuilder* builder, Instruction instruction) {
   // StkId cb = ra + 3;  /* call base */
   builder->Store("cb",
   builder->      IndexAt(luaTypes.StkId,
   builder->              Load("ra"),
   builder->              ConstInt32(3)));

   // setobjs2s(L, cb+2, ra+2);
   // setobjs2s(L, cb+1, ra+1);
   // setobjs2s(L, cb, ra);
   jit_setobj(builder,
   builder->  IndexAt(luaTypes.StkId,
   builder->          Load("cb"),
   builder->          ConstInt32(2)),
   builder->  IndexAt(luaTypes.StkId,
   builder->          Load("ra"),
   builder->          ConstInt32(2)));
   jit_setobj(builder,
   builder->  IndexAt(luaTypes.StkId,
   builder->          Load("cb"),
   builder->          ConstInt32(1)),
   builder->  IndexAt(luaTypes.StkId,
   builder->          Load("ra"),
   builder->          ConstInt32(1)));
   jit_setobj(builder,
   builder->  Load("cb"),
   builder->  Load("ra"));

   // L->top = cb + 3;  /* func. + 2 args (state and index) */
   builder->StoreIndirect("lua_State", "top",
   builder->              Load("L"),
   builder->              IndexAt(luaTypes.StkId,
   builder->                      Load("cb"),
   builder->                      ConstInt32(3)));

   // Protect(luaD_call(L, cb, GETARG_C(i)));
   builder->Call("luaD_call", 3,
   builder->     Load("L"),
   builder->     Load("cb"),
   builder->     ConstInt32(GETARG_C(instruction)));
   jit_Protect(builder);

   // L->top = ci->top;
   builder->StoreIndirect("lua_State", "top",
   builder->              Load("L"),
   builder->              LoadIndirect("CallInfo", "top",
   builder->                           Load("ci")));

   return true;
}

bool Lua::FunctionBuilder::do_tforloop(TR::BytecodeBuilder* builder, TR::IlBuilder* loopStart) {
   // if (!ttisnil(ra + 1)) /* continue loop? */
   TR::IlBuilder* continueLoop = nullptr;
   //builder->Call("printInt", 1,
   //builder->     );
   builder->IfThen(&continueLoop,
                   jit_ttnotnil(builder,
   builder->                   IndexAt(luaTypes.StkId,
   builder->                           Load("ra"),
   builder->                           Const(1))));

   // setobjs2s(L, ra, ra + 1);  /* save control variable */
   jit_setobj(continueLoop,
   continueLoop->Load("ra"),
   continueLoop->           IndexAt(luaTypes.StkId,
   continueLoop->                   Load("ra"),
   continueLoop->                   Const(1)));

   // jump back
   continueLoop->Goto(&loopStart);

   return true;
}

bool Lua::FunctionBuilder::do_setlist(TR::BytecodeBuilder* builder, Instruction instruction) {
   builder->Call("vm_setlist", 2,
   builder->     Load("L"),
   builder->     ConstInt32(instruction));

   return true;
}

bool Lua::FunctionBuilder::do_closure(TR::BytecodeBuilder* builder, Instruction instruction) {
   builder->Store("base",
   builder->      Call("vm_closure", 2,
   builder->           Load("L"),
   builder->           ConstInt32(instruction)));

   return true;
}

bool Lua::FunctionBuilder::do_vararg(TR::BytecodeBuilder* builder, Instruction instruction) {
   //int n = cast_int(base - ci->func) - cl->p->numparams - 1;
   builder->Store("n",
   builder->	Sub(
   builder->		ConvertTo(Int32,
   builder->                  Div(
   builder->                      Sub(
   builder->                          ConvertTo(Int64,
   builder->                                    Load("base")),
   builder->                          ConvertTo(Int64,
   builder->                                    LoadIndirect("CallInfo", "func",
   builder->                                                 Load("ci")))),
   builder->                      ConstInt64(luaTypes.TValue->getSize()))),
   builder->        ConstInt32(prototype->numparams + 1)));

   TR::IlBuilder *lessthan = nullptr;
   builder->IfThen(&lessthan,
   builder->       LessThan(
   builder->                Load("n"),
   builder->                ConstInt32(0)));

   lessthan->Store("n",
   lessthan->      ConstInt32(0));

   int b = GETARG_B(instruction) - 1;  /* required results */
   /* Since b is a constant value for this instruction we can make this decision
    * compile time one instead of a runtime one.*/
   //if (b < 0) {
   if (b < 0) {
      //b = n;  /* get all var. arguments */
      builder->Store("b",
      builder->      Load("n"));

      //Protect(luaD_checkstack(L, n));
      builder->Call("jit_checkstack", 2,
      builder->     Load("L"),
      builder->     Load("n"));
      jit_Protect(builder);

      //ra = RA(i);  /* previous call may change the stack */
      builder->Store("ra", jit_R(builder, GETARG_A(instruction)));
      //L->top = ra + n;
      builder->StoreIndirect("lua_State", "top",
      builder->              Load("L"),
      builder->              IndexAt(typeDictionary()->PointerTo(luaTypes.TValue),
      builder->                      Load("ra"),
      builder->                      Load("n")));
   } else {
      /* Just store b as the constant value if it was >= 0 */
      builder->Store("b",
      builder->      ConstInt32(b));
   }

   //for (j = 0; j < b && j < n; j++)
   TR::IlBuilder *setvals = nullptr;
   TR::IlBuilder *loopbreak = nullptr;
   builder->ForLoopWithBreak(true, "j", &setvals, &loopbreak,
   builder->                 ConstInt32(0),
   builder->                 Load("b"),
   builder->                 ConstInt32(1));

   // j < n break condition
   TR::IlValue *breakcondition = setvals->LessThan(
                                 setvals->         Load("j"),
                                 setvals->         Load("n"));
   setvals->IfCmpEqualZero(&loopbreak, breakcondition);

   //setobjs2s(L, ra + j, base - n + j);
   setvals->Store("dest",
   setvals->      IndexAt(typeDictionary()->PointerTo(luaTypes.TValue),
   setvals->              Load("ra"),
   setvals->              Load("j")));
   //base - n + j is equivalent to base[0 - (n+j)] which can be used with IndexAt
   setvals->Store("src",
   setvals->      IndexAt(typeDictionary()->PointerTo(luaTypes.TValue),
   setvals->              Load("base"),
   setvals->              Sub(
   setvals->                  ConstInt32(0),
   setvals->                  Sub(
   setvals->                      Load("n"),
   setvals->                      Load("j")))));

   jit_setobj(setvals, setvals->Load("dest"), setvals->Load("src"));

   //for (; j < b; j++)  /* complete required results with nil */
   auto setnils = OrphanBuilder();
   builder->ForLoopUp("j", &setnils,
   builder->          Load("j"),
   builder->          Load("n"),
   builder->          ConstInt32(1));

   //setnilvalue(ra + j);
   setnils->StoreIndirect("TValue", "tt_",
   setnils->              IndexAt(typeDictionary()->PointerTo(luaTypes.TValue),
   setnils->                      Load("ra"),
   setnils->                      Load("j")),
   setnils->              ConstInt32(LUA_TNIL));

   return true;
}


//~ IL generation helpers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

void Lua::FunctionBuilder::jit_setobj(TR::IlBuilder* builder, TR::IlValue* dest, TR::IlValue* src) {
   // *dest = *src;
   auto src_value = jit_get_value(builder, src);
   auto src_tt = jit_get_type(builder, src);
   builder->StoreIndirect("TValue", "value_", dest, src_value);
   builder->StoreIndirect("TValue", "tt_", dest, src_tt);
}

void Lua::FunctionBuilder::jit_Protect(TR::IlBuilder* builder) {
   builder->Store("base",
   builder->      LoadIndirect("CallInfo", "u.l.base",
   builder->                   Load("ci")));
}

void Lua::FunctionBuilder::jit_setsavedpc(TR::IlBuilder* builder, int instructionIndex) {
   auto newpc = builder->IndexAt(typeDictionary()->PointerTo(luaTypes.Instruction),
                builder->        Const(prototype->code),
                builder->        ConstInt32(instructionIndex));

   //set savedpc to newpc so the interpreter will start executing where the JIT stopped
   builder->StoreIndirect("CallInfo", "u.l.savedpc",
   builder->              Load("ci"), newpc);
}

void Lua::FunctionBuilder::jit_return(TR::IlBuilder* builder, int instructionIndex) {
   // When a VirtualMachineRegister is added we will need to write out the registers here
   jit_setsavedpc(builder, instructionIndex);
   builder->Return();
}

TR::IlValue* Lua::FunctionBuilder::jit_R(TR::IlBuilder* builder, int arg) {
   auto reg = builder->IndexAt(typeDictionary()->PointerTo(luaTypes.TValue),
              builder->        Load("base"),
              builder->        ConstInt32(arg));
   return reg;
}

TR::IlValue* Lua::FunctionBuilder::jit_K(TR::IlBuilder* builder, int arg) {
   auto constant = builder->IndexAt(typeDictionary()->PointerTo(luaTypes.TValue),
                   builder->        ConstAddress(prototype->k),
                   builder->        ConstInt32(arg));
   return constant;
}

TR::IlValue* Lua::FunctionBuilder::jit_RK(TR::IlBuilder* builder, int arg) {
   return ISK(arg) ? jit_K(builder, INDEXK(arg)) : jit_R(builder, arg);
}

TR::IlValue* Lua::FunctionBuilder::jit_get_type(TR::IlBuilder* builder, TR::IlValue *obj) {
   return builder->LoadIndirect("TValue", "tt_", obj);
}

TR::IlValue* Lua::FunctionBuilder::jit_get_type(TR::IlBuilder* builder, int arg) {
   TR::IlValue *type = nullptr;
   if (ISK(arg)) {
      arg = INDEXK(arg);
      type = builder->ConvertTo(typeDictionary()->toIlType<decltype(TValue::tt_)>(),
             builder->          Const(prototype->k[arg].tt_));
   } else {
      TR::IlValue *object = jit_R(builder, arg);
      type =  jit_get_type(builder, object);
   }
   return type;
}

TR::IlValue* Lua::FunctionBuilder::jit_get_value(TR::IlBuilder* builder, TR::IlValue *obj) {
   return builder->LoadIndirect("TValue", "value_", obj);
}

TR::IlValue* Lua::FunctionBuilder::jit_get_intvalue(TR::IlBuilder* builder, int arg) {
   TR::IlValue *value = nullptr;
   if (ISK(arg)) {
      arg = INDEXK(arg);
      value = builder->ConvertTo(typeDictionary()->toIlType<decltype(TValue::value_.i)>(),
              builder->          ConstInt64(prototype->k[arg].value_.i));
   } else {
      TR::IlValue *object = jit_R(builder, arg);
      TR::IlValue *structAddr = StructFieldAddress(builder, "TValue", "value_", object);
      value =  builder->LoadIndirect("Value", "i", structAddr);
   }
   return value;
}

TR::IlValue* Lua::FunctionBuilder::jit_get_fltvalue(TR::IlBuilder* builder, int arg) {
   TR::IlValue *value = nullptr;
   if (ISK(arg)) {
      arg = INDEXK(arg);
      value = builder->ConvertTo(typeDictionary()->toIlType<decltype(TValue::value_.n)>(),
              builder->          ConstDouble(prototype->k[arg].value_.n));
   } else {
      TR::IlValue *object = jit_R(builder, arg);
      TR::IlValue *structAddr = StructFieldAddress(builder, "TValue", "value_", object);
      value =  builder->LoadIndirect("Value", "n", structAddr);
   }
   return value;
}

void Lua::FunctionBuilder::jit_set_type(TR::IlBuilder* builder, TR::IlValue *obj, TR::IlValue *type) {
   builder->StoreIndirect("TValue", "tt_", obj, type);
}

void Lua::FunctionBuilder::jit_set_value(TR::IlBuilder* builder, TR::IlValue *obj, TR::IlValue *value) {
   builder->StoreIndirect("TValue", "value_", obj, value);
}

void Lua::FunctionBuilder::jit_set_intvalue(TR::IlBuilder* builder, TR::IlValue *obj, TR::IlValue *value) {
   TR::IlValue *valueAddr = StructFieldAddress(builder, "TValue", "value_", obj);
   builder->StoreIndirect("Value", "i", valueAddr, value);
}

void Lua::FunctionBuilder::jit_set_fltvalue(TR::IlBuilder* builder, TR::IlValue *obj, TR::IlValue *value) {
   TR::IlValue *valueAddr = StructFieldAddress(builder, "TValue", "value_", obj);
   builder->StoreIndirect("Value", "n", valueAddr, value);
}

TR::IlValue* Lua::FunctionBuilder::jit_clLvalue(TR::IlBuilder* builder, TR::IlValue* func) {
   // &(cast<GCUnion*>(func->value_.gc)->cl.l)
   return builder->ConvertTo(typeDictionary()->PointerTo(luaTypes.LClosure),
          builder->          LoadIndirect("TValue", "value_", func));  // pretend `value_` is really `value_.gc` because it's a union
}

TR::IlValue* Lua::FunctionBuilder::jit_checktag(TR::IlBuilder* builder, TR::IlValue* value, TR::IlValue* type) {
   auto tt = jit_get_type(builder, value);
   return builder->EqualTo(tt, type);
}

/*
The IL generated by this function will not be exactly the same as the actual implementation
of the `tonumber` macro. This is because there is no efficient way of representing in-out
parameters in JitBuilder. So, instead, the generated IL assumes that the `TValue` stores a
number (a `lua_Integer` or a `lua_Number`) and returns the stored value as a `lua_Number`,
applying a conversion if needed.
*/
TR::IlValue* Lua::FunctionBuilder::jit_tonumber(TR::IlBuilder* builder, TR::IlValue* value, TR::IlValue* type) {
   auto isnum = builder->OrphanBuilder();
   auto isint = builder->OrphanBuilder();
   auto v = StructFieldAddress(builder, "TValue", "value_", value);

   builder->IfThenElse(&isint, &isnum, jit_isinteger(builder, type));

   isnum->Store("num",
   isnum->      LoadIndirect("Value", "n", v));

   isint->Store("num",
   isint->      ConvertTo(luaTypes.lua_Number,
   isint->                LoadIndirect("Value", "i", v)));

   return builder->Load("num");
}

TR::IlValue *Lua::FunctionBuilder::jit_tonumber(TR::IlBuilder *builder, int arg, int type) {
   TR::IlValue *value = nullptr;
   if (type == LUA_TNUMFLT) {
      value = jit_get_fltvalue(builder, arg);
   } else {
      value = builder->ConvertTo(luaTypes.lua_Number, jit_get_intvalue(builder, arg));
   }
   return value;
}

TR::IlValue* Lua::FunctionBuilder::jit_tointeger(TR::IlBuilder* builder, TR::IlValue* value, TR::IlValue* type) {
   auto isflt = builder->OrphanBuilder();
   auto isint = builder->OrphanBuilder();
   auto v = StructFieldAddress(builder, "TValue", "value_", value);

   builder->IfThenElse(&isflt, &isint, jit_isfloat(builder, type));

   isint->Store("int",
   isint->      LoadIndirect("Value", "i", v));

   isflt->Store("int",
   isflt->      ConvertTo(luaTypes.lua_Integer,
   isflt->                LoadIndirect("Value", "n", v)));

   return builder->Load("int");
}

TR::IlValue* Lua::FunctionBuilder::jit_isinteger(TR::IlBuilder* builder, TR::IlValue* type) {
   return builder->EqualTo(type, intType);
}

TR::IlValue* Lua::FunctionBuilder::jit_isfloat(TR::IlBuilder* builder, TR::IlValue* type) {
   return builder->EqualTo(type, fltType);
}

TR::IlValue* Lua::FunctionBuilder::jit_isnumber(TR::IlBuilder* builder, TR::IlValue* type) {
   return builder->And(type, numType);
}

TR::IlValue* Lua::FunctionBuilder::jit_ttnotnil(TR::IlBuilder* builder, TR::IlValue* value) {
   return builder->NotEqualTo(
          builder->        LoadIndirect("TValue", "tt_", value),
          builder->        Const(LUA_TNIL));
}

TR::IlValue* Lua::FunctionBuilder::jit_performmath(TR::IlBuilder *builder, Instruction instruction, TR::IlValue *left, TR::IlValue *right) {
   switch (GET_OPCODE(instruction)) {
   case OP_ADD:
      return builder->Add(left, right);
      break;
   case OP_SUB:
      return builder->Sub(left, right);
      break;
   case OP_MUL:
      return builder->Mul(left, right);
      break;
   case OP_DIV:
      return builder->Div(left, right);
   default:
      return nullptr;
      break;
   }
}

void Lua::FunctionBuilder::jit_performcmp(TR::IlBuilder *builder, Instruction instruction, TR::IlValue *left, TR::IlValue *right) {
   TR::IlBuilder *success = nullptr;
   TR::IlBuilder *fail = nullptr;

   switch (GET_OPCODE(instruction)) {
   case OP_LT:
      // l < r
      builder->IfThenElse(&success, &fail,
      builder->           LessThan(left, right));
      break;
   case OP_LE:
      // l <= r can be replaced with !(l > r)
      builder->IfThenElse(&fail, &success,
      builder->           GreaterThan(left, right));
      break;
   case OP_EQ:
      // l == r
      builder->IfThenElse(&success, &fail,
      builder->           EqualTo(left, right));
   default:
      break;
   }

   success->Store("cmp",
   success->      ConstInt32(1));
   fail->Store("cmp",
   fail->      ConstInt32(0));
}

// jitbuilder extensions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TR::IlValue* Lua::FunctionBuilder::StructFieldAddress(TR::IlBuilder* builder, const char* structName, const char* fieldName, TR::IlValue* obj) {
   auto offset = typeDictionary()->OffsetOf(structName, fieldName);
   auto ptype = typeDictionary()->PointerTo(typeDictionary()->GetFieldType(structName, fieldName));
   auto addr = builder->Add(
                            obj,
               builder->    ConstInt64(offset));
   return builder->ConvertTo(ptype, addr);
}

TR::IlValue* Lua::FunctionBuilder::UnionFieldAddress(TR::IlBuilder* builder, const char* unionName, const char* fieldName, TR::IlValue* obj) {
   auto ptype = typeDictionary()->PointerTo(typeDictionary()->GetFieldType(unionName, fieldName));
   return builder->ConvertTo(ptype, obj);
}

int **Lua::FunctionBuilder::alloc_data_flow_array(int instructionCount, int registerCount) {
   int size = instructionCount * sizeof(int*);
   int **dataFlowTypes = (int**)malloc(size);
   if (dataFlowTypes == nullptr) {
      return dataFlowTypes;
   }
   memset(dataFlowTypes, 0 , size);

   for (int i = 0; i < instructionCount; i++) {
      size = registerCount * sizeof(int);
      dataFlowTypes[i] = (int*)malloc(size);
      if (dataFlowTypes[i] == nullptr) {
         for (int j = 0; j < i; j++) {
            free(dataFlowTypes[j]);
         }
         free(dataFlowTypes);
         return nullptr;
      } else {
         for (int j = 0; j < registerCount; j++) {
            dataFlowTypes[i][j] = LUA_TNONE;
         }
      }
   }

   return dataFlowTypes;
}

void Lua::FunctionBuilder::free_data_flow_array(int **dataTypeArray, int instructionCount) {
   for (int i = 0; i < instructionCount; i++) {
      free(dataTypeArray[i]);
   }
   free(dataTypeArray);
}

/*
 * result of an operation with any unknown type results in an unknown type
 * by default the result of an operation can modify the data type
 * fall through copy can not modify.  If it does it has to set to unknown
 * jump copies to destination (jumps are all forward???) so nothing should be changed
 * forloop copies to destination and if it changes you need to replay loop body until there are no changes.  changes result in unknown type
 */
void Lua::FunctionBuilder::complete_data_flow_analysis(Proto *prototype, int startInstrution, int endInstruction, int **dataTypes, bool allowModifications) {
   /* Seed the register types for op code index 0 with the parameter types */
   for (auto i = 0; i < prototype->numparams; i++) {
      dataTypes[0][i] = prototype->paramtypes[i];
   }

   for (int instructionIndex = startInstrution; instructionIndex < endInstruction; instructionIndex++) {
      data_flow_for_opcode(prototype, prototype->code[instructionIndex], instructionIndex, dataTypes, allowModifications);
   }
}

void Lua::FunctionBuilder::data_flow_for_opcode(Proto* prototype, Instruction instruction, int instructionIndex, int **dataTypes, bool allowModifications) {
   bool forcedModification = false;
   int ra = GETARG_A(instruction);
   int raType = dataTypes[instructionIndex][ra];
   int nextInstructionIndex = instructionIndex + 1;

   switch (GET_OPCODE(instruction)) {
      case OP_MOVE:
         raType = dataTypes[instructionIndex][GETARG_B(instruction)];
         break;
      case OP_LOADK:
         raType = prototype->k[GETARG_Bx(instruction)].tt_;
         forcedModification = true;
         break;
      case OP_ADD:
      case OP_SUB:
      case OP_MUL:
      case OP_MOD:
      case OP_IDIV:
      {
         int rbType = get_data_type(dataTypes, instructionIndex, GETARG_B(instruction));
         int rcType = get_data_type(dataTypes, instructionIndex, GETARG_C(instruction));
         if (is_data_type_int(rbType) && is_data_type_int(rcType)) {
            raType = LUA_TNUMINT;
         } else if (is_data_type_number(rbType) && is_data_type_number(rcType)) {
            raType = LUA_TNUMFLT;
         } else {
            /* This only happens if rb and rc are not known to be numbers */
            raType = LUA_NUMTAGS;
         }
      }
         break;
      case OP_DIV:
      case OP_POW:
      {
         int rbType = get_data_type(dataTypes, instructionIndex, GETARG_B(instruction));
         int rcType = get_data_type(dataTypes, instructionIndex, GETARG_C(instruction));
         if (is_data_type_number(rbType) && is_data_type_number(rcType)) {
            raType = LUA_TNUMFLT;
         } else {
            /* This only happens if rb and rc are not known to be numbers */
            raType = LUA_NUMTAGS;
         }
      }
         break;
      case OP_UNM:
      {
         int rbType = get_data_type(dataTypes, instructionIndex, GETARG_B(instruction));
         if (is_data_type_int(rbType)) {
            raType = LUA_TNUMINT;
         } else if (is_data_type_number(rbType)) {
            raType = LUA_TNUMFLT;
         } else {
            /* This only happens if rb and rc are not known to be numbers */
            raType = LUA_NUMTAGS;
         }
      }
         break;
      case OP_BNOT:
         if (is_data_type_int(get_data_type(dataTypes, instructionIndex, GETARG_B(instruction)))) {
            raType = LUA_TNUMINT;
         } else {
            raType = LUA_NUMTAGS;
         }
         break;
      case OP_NOT:
         raType = LUA_TBOOLEAN;
         break;
      case OP_LEN:
         // Could this be int since it should always return an int unless there is an error */
         raType = LUA_NUMTAGS;
         break;
      case OP_CONCAT:
         raType = LUA_NUMTAGS;
         break;
      case OP_FORPREP:
      {
         int index = ra;
         int indexType = get_data_type(dataTypes, instructionIndex, index);
         int limit = ra + 1;
         int limitType = get_data_type(dataTypes, instructionIndex, limit);
         int step = ra + 2;
         int stepType = get_data_type(dataTypes, instructionIndex, step);
         int externalIndex = ra + 3;
         if (is_data_type_int(indexType) && is_data_type_int(limitType) && is_data_type_number(stepType)) {
            raType = LUA_TNUMINT;
         } else {
            raType = LUA_NUMTAGS;
         }
         /* ra is not changing.  externalIndex is so set ra to externalIndex */
         ra = externalIndex;
      }
         break;
      case OP_FORLOOP:
      {
         int destinationIndex = instructionIndex + 1 + GETARG_sBx(instruction);
         int **loopDataTypes = alloc_data_flow_array(prototype->sizecode, prototype->maxstacksize);
         if (nullptr == loopDataTypes) {
            // Failed to allocate a new data flow array.  Make all types for this section NUMTAGS and continue
            for (int index = destinationIndex; index < instructionIndex; index++) {
               for (int i = 0; i < prototype->maxstacksize; i++) {
                  set_data_type(dataTypes, index, i, LUA_NUMTAGS, false, true);
               }
            }
         } else {
            int **srcDataTypes = dataTypes;

            do {
               // copy the data types for the end of the loop back to the beginning.
               //This allows a test to see if it would have any impact on the data types
               for (int i = 0; i < prototype->maxstacksize; i++) {
                  set_data_type(loopDataTypes, destinationIndex, i, get_data_type(srcDataTypes, instructionIndex, i), false, true);
               }

               // Initialize the rest of the registers to TNONE
               for (int index = destinationIndex + 1; index < instructionIndex; index++) {
                  for (int i = 0; i < prototype->maxstacksize; i++) {
                     set_data_type(loopDataTypes, index, i, LUA_TNONE, false, true);
                  }
               }

               //run through all opcodes
               for (int index = destinationIndex; index < instructionIndex; index++) {
                  data_flow_for_opcode(prototype, prototype->code[index], index, loopDataTypes, true);
               }

               //compare original types to new types. If they are different copy and loop again
               if (compare_data_flow_types(dataTypes, loopDataTypes, destinationIndex, instructionIndex, prototype->maxstacksize)) {
                  break;
               }

               // copy the current data types back into the original array
               for (int index = destinationIndex; index < instructionIndex; index++) {
                  for (int i = 0; i < prototype->maxstacksize; i++) {
                     set_data_type(dataTypes, index, i, get_data_type(loopDataTypes, index, i), false, true);
                  }
               }

               srcDataTypes = loopDataTypes;
            } while (true);

            free_data_flow_array(loopDataTypes, prototype->sizecode);

            if (is_data_type_int(get_data_type(dataTypes, instructionIndex, ra))) {
               raType = LUA_TNUMINT;
            } else {
               raType = LUA_NUMTAGS;
            }
            /* ra is not changing.  externalIndex is so set ra to externalIndex. externalIndex is ra + 3 */
            ra = ra + 3;
         }
      }
         break;
      case OP_CALL:
      {
         int nresults = GETARG_C(instruction) - 1;
         copy_data_types_to_next(prototype, dataTypes, instructionIndex, nextInstructionIndex, false);
         for (int i = 0; i < nresults; i++) {
            /* TODO update this to set type to interpreter profiled return types */
            set_data_type(dataTypes, nextInstructionIndex, ra + i, LUA_NUMTAGS, false, true);
         }
         nextInstructionIndex = -1;
      }
         break;
      case OP_RETURN:
         nextInstructionIndex = -1;
         break;
      default:
         break;
   }

   if (nextInstructionIndex != -1) {
      /* Copy the current data types to the next instruction */
      copy_data_types_to_next(prototype, dataTypes, instructionIndex, nextInstructionIndex, false);
      /* Update ra for the next instruction */
      set_data_type(dataTypes, nextInstructionIndex, ra, raType, allowModifications, forcedModification);
   }
}

bool Lua::FunctionBuilder::compare_data_flow_types(int **dataTypes1, int **dataTypes2, int startIndex, int endIndex, int registerCount) {
   for (int index = startIndex; index < endIndex; index++) {
      for (int i = 0; i < registerCount; i++) {
         if (dataTypes1[index][i] != dataTypes2[index][i]) {
            return false;
         }
      }
   }
   return true;
}

int Lua::FunctionBuilder::get_data_type(int **dataTypes, int instructionIndex, int arg) {
   if(ISK(arg)) {
      return prototype->k[INDEXK(arg)].tt_;
   } else {
      return dataTypes[instructionIndex][arg];
   }
}

void Lua::FunctionBuilder::set_data_type(int **dataTypes, int instructionIndex, int arg, int type, bool allowModifications, bool forcedModification) {
   /* TODO add assert that arg is not a constant! */
   int currentType = dataTypes[instructionIndex][arg];

   if (currentType == LUA_TNONE) {
      dataTypes[instructionIndex][arg] = type;
   } else if (currentType != type) {
      if (allowModifications || forcedModification) {
         dataTypes[instructionIndex][arg] = type;
      } else {
         dataTypes[instructionIndex][arg] = LUA_NUMTAGS;
      }
   }
}

void Lua::FunctionBuilder::copy_data_types_to_next(Proto *prototype, int **dataTypes, int instructionIndex, int destinationInstructionIndex, bool allowModifications) {
   for (int i = 0; i < prototype->maxstacksize; i++) {
      set_data_type(dataTypes, destinationInstructionIndex, i, get_data_type(dataTypes, instructionIndex, i), allowModifications, false);
   }
}

bool Lua::FunctionBuilder::is_data_type_number(int dataType) {
   if (dataType == LUA_TNUMINT) {
      return true;
   } else if (dataType == LUA_TNUMFLT) {
      return true;
   } else {
      return false;
   }
}

bool Lua::FunctionBuilder::is_data_type_int(int dataType) {
   return dataType == LUA_TNUMINT;
}

bool Lua::FunctionBuilder::is_data_type_flt(int dataType) {
   return dataType == LUA_TNUMFLT;
}

