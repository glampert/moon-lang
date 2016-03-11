// -*-C++-*-
// FlexLexer.h -- define interfaces for lexical analyzer classes generated
// by flex

// Copyright (c) 1993 The Regents of the University of California.
// All rights reserved.
//
// This code is derived from software contributed to Berkeley by
// Kent Williams and Tom Epperly.
//
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions
//  are met:

//  1. Redistributions of source code must retain the above copyright
//  notice, this list of conditions and the following disclaimer.
//  2. Redistributions in binary form must reproduce the above copyright
//  notice, this list of conditions and the following disclaimer in the
//  documentation and/or other materials provided with the distribution.

//  Neither the name of the University nor the names of its contributors
//  may be used to endorse or promote products derived from this software
//  without specific prior written permission.

//  THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
//  IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
//  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
//  PURPOSE.

// This file defines FlexLexer, an abstract class which specifies the
// external interface provided to flex C++ lexer objects, and yyFlexLexer,
// which defines a particular lexer class.
//
// If you want to create multiple lexer classes, you use the -P flag
// to rename each yyFlexLexer to some other xxFlexLexer.  You then
// include <FlexLexer.h> in your other sources once per lexer class:
//
//  #undef yyFlexLexer
//  #define yyFlexLexer xxFlexLexer
//  #include <FlexLexer.h>
//
//  #undef yyFlexLexer
//  #define yyFlexLexer zzFlexLexer
//  #include <FlexLexer.h>
//  ...

// ================================================================================================
//
//                                         FlexLexer
//
// ================================================================================================

#ifndef FLEX_LEXER_H
#define FLEX_LEXER_H

#include <iostream>
#ifndef FLEX_STD
    #define FLEX_STD std::
#endif /* !FLEX_STD */

struct yy_buffer_state;
typedef int yy_state_type;

// LAMPERT:
// Notes: These are custom modification for this project only.
// This was the simplest way to add some additional context
// for our parser, but it requires keeping this local copy
// of the modified FlexLexer.h. If you'd like to avoid that,
// then another possible solution is inheriting from yyFlexLexer
// and extending it. You will probably need some type casts here
// and there, but it should also be a relatively clean solution.
namespace moon { struct ParseContext; }
#define YYLEX_EXTRA_USER_CONTEXT moon::ParseContext *

class FlexLexer
{
public:

    virtual ~FlexLexer() { }
    virtual int yylex() = 0;

    virtual void yy_switch_to_buffer(struct yy_buffer_state * new_buffer) = 0;
    virtual struct yy_buffer_state * yy_create_buffer(FLEX_STD istream * s, int size) = 0;
    virtual void yy_delete_buffer(struct yy_buffer_state * b) = 0;
    virtual void yyrestart(FLEX_STD istream * s) = 0;

    // Switch to new input/output streams.
    // A nil stream pointer indicates "keep the current one".
    virtual void switch_streams(FLEX_STD istream * new_in = 0, FLEX_STD ostream * new_out = 0) = 0;

    // Call lex with new input/output sources.
    int yylex(FLEX_STD istream * new_in, FLEX_STD ostream * new_out = 0)
    {
        switch_streams(new_in, new_out);
        return yylex();
    }

    void set_yydebug(bool flag) { yy_flex_debug = flag; }
    void set_yycontext(YYLEX_EXTRA_USER_CONTEXT new_ctx) { ctx = new_ctx; }

    const char * get_yytext() const { return yytext;   }
    int get_yyleng()          const { return yyleng;   }
    int get_yylineno()        const { return yylineno; }
    int get_yydebug()         const { return yy_flex_debug; }

protected:

    // LAMPERT:
    // This is a custom modification done to the FlexLexer.h
    // header file. Couldn't figure out a simpler way of passing
    // an extra parameter to the lex method, so I added this.
    YYLEX_EXTRA_USER_CONTEXT ctx{};

    char * yytext;
    int yyleng;
    int yylineno;      // only maintained if you use %option yylineno
    int yy_flex_debug; // only has effect with -d or "%option debug"
};

#endif // FLEX_LEXER_H

// ================================================================================================
//
//                                      yyFlexLexer
//
// ================================================================================================

// Either this is the first time through (yyFlexLexerOnce not defined),
// or this is a repeated include to define a different flavor of
// yyFlexLexer, as discussed in the flex manual.
#if defined(yyFlexLexer) || !defined(yyFlexLexerOnce)
#define yyFlexLexerOnce

class yyFlexLexer
    : public FlexLexer
{
public:

    // arg_yyin and arg_yyout default to the cin and cout, but we
    // only make that assignment when initializing in the lex() function.
    yyFlexLexer(FLEX_STD istream * arg_yyin = 0, FLEX_STD ostream * arg_yyout = 0);
    virtual ~yyFlexLexer();

    void yy_switch_to_buffer(struct yy_buffer_state * new_buffer);
    struct yy_buffer_state * yy_create_buffer(FLEX_STD istream * s, int size);
    void yy_delete_buffer(struct yy_buffer_state * b);
    void yyrestart(FLEX_STD istream * s);

    void yypush_buffer_state(struct yy_buffer_state * new_buffer);
    void yypop_buffer_state();

    virtual int yylex();
    virtual int yywrap();
    virtual void switch_streams(FLEX_STD istream * new_in, FLEX_STD ostream * new_out = 0);

    void yyunput(int c, char * buf_ptr);
    int yyinput();

protected:

    virtual int LexerInput(char * buf, int max_size);
    virtual void LexerOutput(const char * buf, int size);
    virtual void LexerError(const char * msg);

    void yy_load_buffer_state();
    void yy_init_buffer(struct yy_buffer_state * b, FLEX_STD istream * s);
    void yy_flush_buffer(struct yy_buffer_state * b);

    void yy_push_state(int new_state);
    void yy_pop_state();
    int yy_top_state();

    yy_state_type yy_get_previous_state();
    yy_state_type yy_try_NUL_trans(yy_state_type current_state);
    int yy_get_next_buffer();
    void yyensure_buffer_stack();

protected:

    FLEX_STD istream * yyin;  // input source for default LexerInput
    FLEX_STD ostream * yyout; // output sink for default LexerOutput

    // Points to current character in buffer.
    char * yy_c_buf_p;

    int * yy_start_stack;
    int yy_start_stack_ptr;
    int yy_start_stack_depth;

    // yy_hold_char holds the character lost when yytext is formed.
    char yy_hold_char;

    // Number of characters read into yy_ch_buf.
    int yy_n_chars;

    int yy_init;  // whether we need to initialize
    int yy_start; // start state number

    // Flag which is used to allow yywrap()'s to do buffer switches
    // instead of setting up a fresh yyin.  A bit of a hack ...
    int yy_did_buffer_switch_on_eof;

    size_t yy_buffer_stack_top;                /* index of top of stack. */
    size_t yy_buffer_stack_max;                /* capacity of stack. */
    struct yy_buffer_state ** yy_buffer_stack; /* Stack as an array. */

    // The following are not always needed, but may be depending
    // on use of certain flex features (like REJECT or yymore()).

    yy_state_type yy_last_accepting_state;
    char * yy_last_accepting_cpos;

    yy_state_type * yy_state_buf;
    yy_state_type * yy_state_ptr;

    char * yy_full_match;
    int * yy_full_state;
    int yy_full_lp;

    int yy_lp;
    int yy_looking_for_trail_begin;

    int yy_more_flag;
    int yy_more_len;
    int yy_more_offset;
    int yy_prev_more_offset;
};

#endif // yyFlexLexer || !yyFlexLexerOnce
