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

#ifndef LUAFUNCTIONBUILDER_HPP
#define LUAFUNCTIONBUILDER_HPP

// JitBuilder headers
#include "ilgen/MethodBuilder.hpp"
#include "ilgen/BytecodeBuilder.hpp"

// Lua headers
#include "LuaTypeDictionary.hpp"
#include "luavm.hpp"

namespace Lua { class FunctionBuilder; }

/*
** An IL builder for Lua functions
*/
class Lua::FunctionBuilder : public TR::MethodBuilder {
public:
    FunctionBuilder(Proto* p, Lua::TypeDictionary* types);
    
    bool buildIL() override;

    /*
    All builder convenience functions take an already allocated
    builder object, generate IL for the opcode they correspond to, and
    return true if successful, false otherwise.
    */

    bool do_move(TR::BytecodeBuilder* builder, Instruction instruction);
    bool do_loadk(TR::BytecodeBuilder* builder, Instruction instruction);
    bool do_loadbool(TR::BytecodeBuilder* builder, TR::BytecodeBuilder* dest, Instruction instruction);
    bool do_loadnil(TR::BytecodeBuilder* builder, Instruction instruction);

    bool do_getupval(TR::BytecodeBuilder* builder, Instruction instruction);
    bool do_gettabup(TR::BytecodeBuilder* builder, Instruction instruction);
    bool do_gettable(TR::BytecodeBuilder* builder, Instruction instruction);
    bool do_settabup(TR::BytecodeBuilder* builder, Instruction instruction);
    bool do_setupval(TR::BytecodeBuilder* builder, Instruction instruction);
    bool do_settable(TR::BytecodeBuilder* builder, Instruction instruction);

    bool do_newtable(TR::BytecodeBuilder* builder, Instruction instruction);

    bool do_self(TR::BytecodeBuilder* builder, Instruction instruction);

    bool do_math(TR::BytecodeBuilder* builder, Instruction instruction, int instructionIndex);
    bool do_mod(TR::BytecodeBuilder* builder, Instruction instruction);
    bool do_pow(TR::BytecodeBuilder* builder, Instruction instruction);
    bool do_div(TR::BytecodeBuilder* builder, Instruction instruction, int instructionIndex);
    bool do_idiv(TR::BytecodeBuilder* builder, Instruction instruction);
    bool do_band(TR::BytecodeBuilder* builder, Instruction instruction);
    bool do_bor(TR::BytecodeBuilder* builder, Instruction instruction);
    bool do_bxor(TR::BytecodeBuilder* builder, Instruction instruction);
    bool do_shl(TR::BytecodeBuilder* builder, Instruction instruction);
    bool do_shr(TR::BytecodeBuilder* builder, Instruction instruction);
    bool do_unm(TR::BytecodeBuilder* builder, Instruction instruction);
    bool do_bnot(TR::BytecodeBuilder* builder, Instruction instruction);
    bool do_not(TR::BytecodeBuilder* builder, Instruction instruction);
    bool do_len(TR::BytecodeBuilder* builder, Instruction instruction);
    bool do_concat(TR::BytecodeBuilder* builder, Instruction instruction);

    bool do_jmp(TR::BytecodeBuilder* builder, Instruction instruction);
    bool do_eq(TR::BytecodeBuilder* builder, TR::BytecodeBuilder* dest, Instruction instruction, int instructionIndex);
    bool do_cmp(const char* cmpFunc, TR::BytecodeBuilder* builder, TR::BytecodeBuilder* dest, Instruction instruction, int instructionIndex);
    bool do_test(TR::BytecodeBuilder* builder, TR::BytecodeBuilder* dest, Instruction instruction);
    bool do_testset(TR::BytecodeBuilder* builder, TR::BytecodeBuilder* dest, Instruction instruction);

    bool do_call(TR::BytecodeBuilder* builder, Instruction instruction, int instructionIndex);
    bool do_return(TR::BytecodeBuilder* builder, Instruction instruction);

    bool do_forloop(TR::BytecodeBuilder* builder, TR::BytecodeBuilder* loopStart, Instruction instruction, int instructionIndex);
    bool do_forprep(TR::BytecodeBuilder* builder, Instruction instruction, int instructionIndex);

    bool do_tforcall(TR::BytecodeBuilder* builder, Instruction instruction);
    bool do_tforloop(TR::BytecodeBuilder* builder, TR::IlBuilder* loopStart);

    bool do_setlist(TR::BytecodeBuilder* builder, Instruction instruction);

    bool do_closure(TR::BytecodeBuilder* builder, Instruction instruction);

    bool do_vararg(TR::BytecodeBuilder* builder, Instruction instruction);

    // convenience functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    /*
    These functions generate IL builders that are equivalent to the expansion of
    corresponding macros defined in the Lua VM.
    */
    void jit_setobj(TR::IlBuilder* builder, TR::IlValue* obj1, TR::IlValue* obj2);
    void jit_Protect(TR::IlBuilder* builder); // updates local copies of values in case of stack reallocation
    void jit_setsavedpc(TR::IlBuilder* builder, int instructionIndex);
    void jit_return(TR::IlBuilder* builder, int instructionIndex);
    TR::IlValue* jit_R(TR::IlBuilder* builder, int arg);
    TR::IlValue* jit_K(TR::IlBuilder* builder, int arg);
    TR::IlValue* jit_RK(TR::IlBuilder* builder, int arg); // equivalent to RKB and RKC in `lua/lvm.c`
    TR::IlValue* jit_get_type(TR::IlBuilder* builder, TR::IlValue *obj);
    TR::IlValue* jit_get_type(TR::IlBuilder* builder, int arg);
    TR::IlValue* jit_get_value(TR::IlBuilder* builder, TR::IlValue *obj);
    TR::IlValue* jit_get_intvalue(TR::IlBuilder* builder, int arg);
    TR::IlValue* jit_get_fltvalue(TR::IlBuilder* builder, int arg);
    void jit_set_type(TR::IlBuilder* builder, TR::IlValue *obj, TR::IlValue *type);
    void jit_set_value(TR::IlBuilder* builder, TR::IlValue *obj, TR::IlValue *value);
    void jit_set_intvalue(TR::IlBuilder* builder, TR::IlValue *obj, TR::IlValue *value);
    void jit_set_fltvalue(TR::IlBuilder* builder, TR::IlValue *obj, TR::IlValue *value);
    TR::IlValue* jit_clLvalue(TR::IlBuilder* builder, TR::IlValue* func);
    TR::IlValue* jit_checktag(TR::IlBuilder* builder, TR::IlValue* value, TR::IlValue* type);
    TR::IlValue* jit_tonumber(TR::IlBuilder* builder, TR::IlValue* value, TR::IlValue* type);
    TR::IlValue *jit_tonumber(TR::IlBuilder *builder, int arg, int type);
    TR::IlValue* jit_tointeger(TR::IlBuilder* builder, TR::IlValue* value, TR::IlValue* type);
    TR::IlValue* jit_isinteger(TR::IlBuilder* builder, TR::IlValue* type);
    TR::IlValue* jit_isfloat(TR::IlBuilder* builder, TR::IlValue* type);
    TR::IlValue* jit_isnumber(TR::IlBuilder* builder, TR::IlValue* type);
    TR::IlValue* jit_ttnotnil(TR::IlBuilder* builder, TR::IlValue* value);
    
    void jit_integer_math(TR::IlBuilder *builder, Instruction instruction, int result_arg, int left_arg, int right_arg);
    void jit_integer_cmp(TR::IlBuilder *builder, Instruction instruction, int left_arg, int right_arg);
    void jit_number_math(TR::IlBuilder *builder, Instruction instruction, int result_arg, int left_arg, int left_type, int right_arg, int right_type);
    void jit_number_cmp(TR::IlBuilder *builder, Instruction instruction, int left_arg, int left_type, int right_arg, int right_type);
    TR::IlValue* jit_performmath(TR::IlBuilder *builder, Instruction instruction, TR::IlValue *left, TR::IlValue *right);
    void jit_performcmp(TR::IlBuilder *builder, Instruction instruction, TR::IlValue *left, TR::IlValue *right);

    int **alloc_data_flow_array(int instructionCount, int registerCount);
    void free_data_flow_array(int **dataTypeArray, int instructionCount);
    void complete_data_flow_analysis(Proto *prototype, int startInstruction, int endInstruction, int **dataTypes, bool allowModifications);
    void data_flow_for_opcode(Proto* prototype, Instruction instruction, int instructionIndex, int **dataTypes, bool allowModifications);
    bool compare_data_flow_types(int **dataTypes1, int **dataTypes2, int startIndex, int endIndex, int registerCount);
    int get_data_type(int **dataTypes, int instructionIndex, int arg);
    void set_data_type(int **dataTypes, int instructionIndex, int arg, int type, bool allowModifications, bool forcedModification);
    void copy_data_types_to_next(Proto *prototype, int **dataTypes, int instructionIndex, int destinationInstructionIndex, bool allowModifications);
    bool is_data_type_number(int dataType);
    bool is_data_type_int(int dataType);
    bool is_data_type_flt(int dataType);

    // jitbuilder extensions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    TR::IlValue* StructFieldAddress(TR::IlBuilder* builder, const char* structName, const char* fieldName, TR::IlValue* obj);
    TR::IlValue* UnionFieldAddress(TR::IlBuilder* builder, const char* unionName, const char* fieldName, TR::IlValue* obj);

private:
    Proto* prototype;
    TR::IlValue *intType;
    TR::IlValue *fltType;
    TR::IlValue *numType;
    int **datatypes;
    TypeDictionary::LuaTypes luaTypes;
};

#endif // LUAFUNCTIONBUILDER_HPP
