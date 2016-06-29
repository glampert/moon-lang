// A Bison parser, made by GNU Bison 3.0.4.

// Skeleton implementation for Bison LALR(1) parsers in C++

// Copyright (C) 2002-2015 Free Software Foundation, Inc.

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

// As a special exception, you may create a larger work that contains
// part or all of the Bison parser skeleton and distribute that work
// under terms of your choice, so long as that work isn't itself a
// parser generator using the skeleton or a modified version thereof
// as a parser skeleton.  Alternatively, if you modify or redistribute
// the parser skeleton itself, you may (at your option) remove this
// special exception, which will cause the skeleton and the resulting
// Bison output files to be licensed under the GNU General Public
// License without this special exception.

// This special exception was added by the Free Software Foundation in
// version 2.2 of Bison.


// First part of user declarations.
#line 23 "source/lib/parser.yxx" // lalr1.cc:404

/*
 * Nothing to be added in the prologue section at the moment.
 * Left it here because Vim's syntax highlighting gets messed
 * up if this section is missing.
 *
 * Most of the stuff depends on the header file, so they
 * must be placed inside the %code { } section below.
 */

#line 47 "generated/parser.cpp" // lalr1.cc:404

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

#include "parser.hpp"

// User implementation prologue.

#line 61 "generated/parser.cpp" // lalr1.cc:412
// Unqualified %code blocks.
#line 56 "source/lib/parser.yxx" // lalr1.cc:413


#include "semantic_check.hpp"
#include "vm.hpp"

namespace moon
{

// Slightly shorter names for local usage:
using STNode = SyntaxTreeNode::Type;
using STEval = SyntaxTreeNode::Eval;

int yylex(SemanticVal * yylval, ParseContext & ctx)
{
    ctx.yylval = yylval;
    return ctx.lexer->yylex();
}

void Parser::error(const std::string & message)
{
    parserError(ctx, message);
}

} /* namespace moon */

#line 89 "generated/parser.cpp" // lalr1.cc:413


#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> // FIXME: INFRINGES ON USER NAME SPACE.
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif



// Suppress unused-variable warnings by "using" E.
#define YYUSE(E) ((void) (E))

// Enable debugging if requested.
#if YYDEBUG

// A pseudo ostream that takes yydebug_ into account.
# define YYCDEBUG if (yydebug_) (*yycdebug_)

# define YY_SYMBOL_PRINT(Title, Symbol)         \
  do {                                          \
    if (yydebug_)                               \
    {                                           \
      *yycdebug_ << Title << ' ';               \
      yy_print_ (*yycdebug_, Symbol);           \
      *yycdebug_ << std::endl;                  \
    }                                           \
  } while (false)

# define YY_REDUCE_PRINT(Rule)          \
  do {                                  \
    if (yydebug_)                       \
      yy_reduce_print_ (Rule);          \
  } while (false)

# define YY_STACK_PRINT()               \
  do {                                  \
    if (yydebug_)                       \
      yystack_print_ ();                \
  } while (false)

#else // !YYDEBUG

# define YYCDEBUG if (false) std::cerr
# define YY_SYMBOL_PRINT(Title, Symbol)  YYUSE(Symbol)
# define YY_REDUCE_PRINT(Rule)           static_cast<void>(0)
# define YY_STACK_PRINT()                static_cast<void>(0)

#endif // !YYDEBUG

#define yyerrok         (yyerrstatus_ = 0)
#define yyclearin       (yyla.clear ())

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYRECOVERING()  (!!yyerrstatus_)

#line 17 "source/lib/parser.yxx" // lalr1.cc:479
namespace moon {
#line 156 "generated/parser.cpp" // lalr1.cc:479

  /* Return YYSTR after stripping away unnecessary quotes and
     backslashes, so that it's suitable for yyerror.  The heuristic is
     that double-quoting is unnecessary unless the string contains an
     apostrophe, a comma, or backslash (other than backslash-backslash).
     YYSTR is taken from yytname.  */
  std::string
  Parser::yytnamerr_ (const char *yystr)
  {
    if (*yystr == '"')
      {
        std::string yyr = "";
        char const *yyp = yystr;

        for (;;)
          switch (*++yyp)
            {
            case '\'':
            case ',':
              goto do_not_strip_quotes;

            case '\\':
              if (*++yyp != '\\')
                goto do_not_strip_quotes;
              // Fall through.
            default:
              yyr += *yyp;
              break;

            case '"':
              return yyr;
            }
      do_not_strip_quotes: ;
      }

    return yystr;
  }


  /// Build a parser object.
  Parser::Parser (ParseContext & ctx_yyarg)
    :
#if YYDEBUG
      yydebug_ (false),
      yycdebug_ (&std::cerr),
#endif
      ctx (ctx_yyarg)
  {}

  Parser::~Parser ()
  {}


  /*---------------.
  | Symbol types.  |
  `---------------*/

  inline
  Parser::syntax_error::syntax_error (const std::string& m)
    : std::runtime_error (m)
  {}

  // basic_symbol.
  template <typename Base>
  inline
  Parser::basic_symbol<Base>::basic_symbol ()
    : value ()
  {}

  template <typename Base>
  inline
  Parser::basic_symbol<Base>::basic_symbol (const basic_symbol& other)
    : Base (other)
    , value ()
  {
    value = other.value;
  }


  template <typename Base>
  inline
  Parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, const semantic_type& v)
    : Base (t)
    , value (v)
  {}


  /// Constructor for valueless symbols.
  template <typename Base>
  inline
  Parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t)
    : Base (t)
    , value ()
  {}

  template <typename Base>
  inline
  Parser::basic_symbol<Base>::~basic_symbol ()
  {
    clear ();
  }

  template <typename Base>
  inline
  void
  Parser::basic_symbol<Base>::clear ()
  {
    Base::clear ();
  }

  template <typename Base>
  inline
  bool
  Parser::basic_symbol<Base>::empty () const
  {
    return Base::type_get () == empty_symbol;
  }

  template <typename Base>
  inline
  void
  Parser::basic_symbol<Base>::move (basic_symbol& s)
  {
    super_type::move(s);
    value = s.value;
  }

  // by_type.
  inline
  Parser::by_type::by_type ()
    : type (empty_symbol)
  {}

  inline
  Parser::by_type::by_type (const by_type& other)
    : type (other.type)
  {}

  inline
  Parser::by_type::by_type (token_type t)
    : type (yytranslate_ (t))
  {}

  inline
  void
  Parser::by_type::clear ()
  {
    type = empty_symbol;
  }

  inline
  void
  Parser::by_type::move (by_type& that)
  {
    type = that.type;
    that.clear ();
  }

  inline
  int
  Parser::by_type::type_get () const
  {
    return type;
  }


  // by_state.
  inline
  Parser::by_state::by_state ()
    : state (empty_state)
  {}

  inline
  Parser::by_state::by_state (const by_state& other)
    : state (other.state)
  {}

  inline
  void
  Parser::by_state::clear ()
  {
    state = empty_state;
  }

  inline
  void
  Parser::by_state::move (by_state& that)
  {
    state = that.state;
    that.clear ();
  }

  inline
  Parser::by_state::by_state (state_type s)
    : state (s)
  {}

  inline
  Parser::symbol_number_type
  Parser::by_state::type_get () const
  {
    if (state == empty_state)
      return empty_symbol;
    else
      return yystos_[state];
  }

  inline
  Parser::stack_symbol_type::stack_symbol_type ()
  {}


  inline
  Parser::stack_symbol_type::stack_symbol_type (state_type s, symbol_type& that)
    : super_type (s)
  {
    value = that.value;
    // that is emptied.
    that.type = empty_symbol;
  }

  inline
  Parser::stack_symbol_type&
  Parser::stack_symbol_type::operator= (const stack_symbol_type& that)
  {
    state = that.state;
    value = that.value;
    return *this;
  }


  template <typename Base>
  inline
  void
  Parser::yy_destroy_ (const char* yymsg, basic_symbol<Base>& yysym) const
  {
    if (yymsg)
      YY_SYMBOL_PRINT (yymsg, yysym);

    // User destructor.
    YYUSE (yysym.type_get ());
  }

#if YYDEBUG
  template <typename Base>
  void
  Parser::yy_print_ (std::ostream& yyo,
                                     const basic_symbol<Base>& yysym) const
  {
    std::ostream& yyoutput = yyo;
    YYUSE (yyoutput);
    symbol_number_type yytype = yysym.type_get ();
    // Avoid a (spurious) G++ 4.8 warning about "array subscript is
    // below array bounds".
    if (yysym.empty ())
      std::abort ();
    yyo << (yytype < yyntokens_ ? "token" : "nterm")
        << ' ' << yytname_[yytype] << " (";
    YYUSE (yytype);
    yyo << ')';
  }
#endif

  inline
  void
  Parser::yypush_ (const char* m, state_type s, symbol_type& sym)
  {
    stack_symbol_type t (s, sym);
    yypush_ (m, t);
  }

  inline
  void
  Parser::yypush_ (const char* m, stack_symbol_type& s)
  {
    if (m)
      YY_SYMBOL_PRINT (m, s);
    yystack_.push (s);
  }

  inline
  void
  Parser::yypop_ (unsigned int n)
  {
    yystack_.pop (n);
  }

#if YYDEBUG
  std::ostream&
  Parser::debug_stream () const
  {
    return *yycdebug_;
  }

  void
  Parser::set_debug_stream (std::ostream& o)
  {
    yycdebug_ = &o;
  }


  Parser::debug_level_type
  Parser::debug_level () const
  {
    return yydebug_;
  }

  void
  Parser::set_debug_level (debug_level_type l)
  {
    yydebug_ = l;
  }
#endif // YYDEBUG

  inline Parser::state_type
  Parser::yy_lr_goto_state_ (state_type yystate, int yysym)
  {
    int yyr = yypgoto_[yysym - yyntokens_] + yystate;
    if (0 <= yyr && yyr <= yylast_ && yycheck_[yyr] == yystate)
      return yytable_[yyr];
    else
      return yydefgoto_[yysym - yyntokens_];
  }

  inline bool
  Parser::yy_pact_value_is_default_ (int yyvalue)
  {
    return yyvalue == yypact_ninf_;
  }

  inline bool
  Parser::yy_table_value_is_error_ (int yyvalue)
  {
    return yyvalue == yytable_ninf_;
  }

  int
  Parser::parse ()
  {
    // State.
    int yyn;
    /// Length of the RHS of the rule being reduced.
    int yylen = 0;

    // Error handling.
    int yynerrs_ = 0;
    int yyerrstatus_ = 0;

    /// The lookahead symbol.
    symbol_type yyla;

    /// The return value of parse ().
    int yyresult;

    // FIXME: This shoud be completely indented.  It is not yet to
    // avoid gratuitous conflicts when merging into the master branch.
    try
      {
    YYCDEBUG << "Starting parse" << std::endl;


    /* Initialize the stack.  The initial state will be set in
       yynewstate, since the latter expects the semantical and the
       location values to have been already stored, initialize these
       stacks with a primary value.  */
    yystack_.clear ();
    yypush_ (YY_NULLPTR, 0, yyla);

    // A new symbol was pushed on the stack.
  yynewstate:
    YYCDEBUG << "Entering state " << yystack_[0].state << std::endl;

    // Accept?
    if (yystack_[0].state == yyfinal_)
      goto yyacceptlab;

    goto yybackup;

    // Backup.
  yybackup:

    // Try to take a decision without lookahead.
    yyn = yypact_[yystack_[0].state];
    if (yy_pact_value_is_default_ (yyn))
      goto yydefault;

    // Read a lookahead token.
    if (yyla.empty ())
      {
        YYCDEBUG << "Reading a token: ";
        try
          {
            yyla.type = yytranslate_ (yylex (&yyla.value, ctx));
          }
        catch (const syntax_error& yyexc)
          {
            error (yyexc);
            goto yyerrlab1;
          }
      }
    YY_SYMBOL_PRINT ("Next token is", yyla);

    /* If the proper action on seeing token YYLA.TYPE is to reduce or
       to detect an error, take that action.  */
    yyn += yyla.type_get ();
    if (yyn < 0 || yylast_ < yyn || yycheck_[yyn] != yyla.type_get ())
      goto yydefault;

    // Reduce or error.
    yyn = yytable_[yyn];
    if (yyn <= 0)
      {
        if (yy_table_value_is_error_ (yyn))
          goto yyerrlab;
        yyn = -yyn;
        goto yyreduce;
      }

    // Count tokens shifted since error; after three, turn off error status.
    if (yyerrstatus_)
      --yyerrstatus_;

    // Shift the lookahead token.
    yypush_ ("Shifting", yyn, yyla);
    goto yynewstate;

  /*-----------------------------------------------------------.
  | yydefault -- do the default action for the current state.  |
  `-----------------------------------------------------------*/
  yydefault:
    yyn = yydefact_[yystack_[0].state];
    if (yyn == 0)
      goto yyerrlab;
    goto yyreduce;

  /*-----------------------------.
  | yyreduce -- Do a reduction.  |
  `-----------------------------*/
  yyreduce:
    yylen = yyr2_[yyn];
    {
      stack_symbol_type yylhs;
      yylhs.state = yy_lr_goto_state_(yystack_[yylen].state, yyr1_[yyn]);
      /* If YYLEN is nonzero, implement the default value of the
         action: '$$ = $1'.  Otherwise, use the top of the stack.

         Otherwise, the following line sets YYLHS.VALUE to garbage.
         This behavior is undocumented and Bison users should not rely
         upon it.  */
      if (yylen)
        yylhs.value = yystack_[yylen - 1].value;
      else
        yylhs.value = yystack_[0].value;


      // Perform the reduction.
      YY_REDUCE_PRINT (yyn);
      try
        {
          switch (yyn)
            {
  case 2:
#line 297 "source/lib/parser.yxx" // lalr1.cc:859
    { beginTranslationUnit(ctx, (yystack_[0].value.asSTNode)); }
#line 621 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 3:
#line 302 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newStatementNode(ctx, (yystack_[1].value.asSTNode), (yystack_[0].value.asSTNode)); }
#line 627 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 4:
#line 303 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = nullptr; }
#line 633 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 23:
#line 334 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newLiteralNode(ctx, STEval::Long,   (yystack_[0].value.asSymbol)); }
#line 639 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 24:
#line 335 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newLiteralNode(ctx, STEval::Double, (yystack_[0].value.asSymbol)); }
#line 645 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 25:
#line 336 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newLiteralNode(ctx, STEval::Bool,   (yystack_[0].value.asSymbol)); }
#line 651 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 26:
#line 337 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newLiteralNode(ctx, STEval::Str,    (yystack_[0].value.asSymbol)); }
#line 657 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 27:
#line 338 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newLiteralNode(ctx, STEval::Null, ctx.symTable->findSymbol("null")); }
#line 663 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 28:
#line 342 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newTypeIdNode(ctx, STEval::Int);    }
#line 669 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 29:
#line 343 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newTypeIdNode(ctx, STEval::Long);   }
#line 675 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 30:
#line 344 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newTypeIdNode(ctx, STEval::Float);  }
#line 681 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 31:
#line 345 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newTypeIdNode(ctx, STEval::Double); }
#line 687 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 32:
#line 346 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newTypeIdNode(ctx, STEval::Bool);   }
#line 693 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 33:
#line 347 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newTypeIdNode(ctx, STEval::Str);    }
#line 699 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 34:
#line 348 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newTypeIdNode(ctx, STEval::Array);  }
#line 705 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 35:
#line 349 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newTypeIdNode(ctx, STEval::Range);  }
#line 711 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 36:
#line 350 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newTypeIdNode(ctx, STEval::Any);    }
#line 717 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 37:
#line 351 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newTypeIdNode(ctx, STEval::Tid);    }
#line 723 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 38:
#line 352 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newTypeIdNode(ctx, STEval::Func);   }
#line 729 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 40:
#line 357 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newUDTNode(ctx, (yystack_[0].value.asSymbol)); }
#line 735 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 41:
#line 361 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newIdentNode(ctx, (yystack_[0].value.asSymbol)); }
#line 741 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 42:
#line 365 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = handleImportDirective(ctx, (yystack_[1].value.asSymbol)); }
#line 747 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 48:
#line 386 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newBinaryOpNode(ctx, STNode::ExprModAssign,       (yystack_[2].value.asSTNode), (yystack_[0].value.asSTNode)); }
#line 753 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 49:
#line 387 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newBinaryOpNode(ctx, STNode::ExprDivAssign,       (yystack_[2].value.asSTNode), (yystack_[0].value.asSTNode)); }
#line 759 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 50:
#line 388 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newBinaryOpNode(ctx, STNode::ExprMulAssign,       (yystack_[2].value.asSTNode), (yystack_[0].value.asSTNode)); }
#line 765 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 51:
#line 389 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newBinaryOpNode(ctx, STNode::ExprSubAssign,       (yystack_[2].value.asSTNode), (yystack_[0].value.asSTNode)); }
#line 771 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 52:
#line 390 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newBinaryOpNode(ctx, STNode::ExprAddAssign,       (yystack_[2].value.asSTNode), (yystack_[0].value.asSTNode)); }
#line 777 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 53:
#line 391 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newBinaryOpNode(ctx, STNode::ExprAssign,          (yystack_[2].value.asSTNode), (yystack_[0].value.asSTNode)); }
#line 783 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 54:
#line 392 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newBinaryOpNode(ctx, STNode::ExprLogicOr,         (yystack_[2].value.asSTNode), (yystack_[0].value.asSTNode)); }
#line 789 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 55:
#line 393 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newBinaryOpNode(ctx, STNode::ExprLogicAnd,        (yystack_[2].value.asSTNode), (yystack_[0].value.asSTNode)); }
#line 795 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 56:
#line 394 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newBinaryOpNode(ctx, STNode::ExprCmpNotEqual,     (yystack_[2].value.asSTNode), (yystack_[0].value.asSTNode)); }
#line 801 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 57:
#line 395 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newBinaryOpNode(ctx, STNode::ExprCmpEqual,        (yystack_[2].value.asSTNode), (yystack_[0].value.asSTNode)); }
#line 807 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 58:
#line 396 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newBinaryOpNode(ctx, STNode::ExprCmpGreaterEqual, (yystack_[2].value.asSTNode), (yystack_[0].value.asSTNode)); }
#line 813 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 59:
#line 397 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newBinaryOpNode(ctx, STNode::ExprCmpGreaterThan,  (yystack_[2].value.asSTNode), (yystack_[0].value.asSTNode)); }
#line 819 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 60:
#line 398 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newBinaryOpNode(ctx, STNode::ExprCmpLessEqual,    (yystack_[2].value.asSTNode), (yystack_[0].value.asSTNode)); }
#line 825 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 61:
#line 399 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newBinaryOpNode(ctx, STNode::ExprCmpLessThan,     (yystack_[2].value.asSTNode), (yystack_[0].value.asSTNode)); }
#line 831 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 62:
#line 400 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newBinaryOpNode(ctx, STNode::ExprSubtract,        (yystack_[2].value.asSTNode), (yystack_[0].value.asSTNode)); }
#line 837 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 63:
#line 401 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newBinaryOpNode(ctx, STNode::ExprAdd,             (yystack_[2].value.asSTNode), (yystack_[0].value.asSTNode)); }
#line 843 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 64:
#line 402 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newBinaryOpNode(ctx, STNode::ExprModulo,          (yystack_[2].value.asSTNode), (yystack_[0].value.asSTNode)); }
#line 849 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 65:
#line 403 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newBinaryOpNode(ctx, STNode::ExprDivide,          (yystack_[2].value.asSTNode), (yystack_[0].value.asSTNode)); }
#line 855 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 66:
#line 404 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newBinaryOpNode(ctx, STNode::ExprMultiply,        (yystack_[2].value.asSTNode), (yystack_[0].value.asSTNode)); }
#line 861 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 67:
#line 405 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newUnaryOpNode(ctx, STNode::ExprLogicNot,   OpCode::LogicNot, (yystack_[0].value.asSTNode)); }
#line 867 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 68:
#line 406 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newUnaryOpNode(ctx, STNode::ExprUnaryMinus, OpCode::Negate,   (yystack_[0].value.asSTNode)); }
#line 873 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 69:
#line 407 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newUnaryOpNode(ctx, STNode::ExprUnaryPlus,  OpCode::Plus,     (yystack_[0].value.asSTNode)); }
#line 879 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 75:
#line 414 "source/lib/parser.yxx" // lalr1.cc:859
    {
            // Wrapping an expr in ( ) gives it the highest precedence.
            // So just reapply the rule with the new precedence.
            (yylhs.value.asSTNode) = (yystack_[1].value.asSTNode);
        }
#line 889 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 78:
#line 428 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = newTypeofNode(ctx, (yystack_[1].value.asSTNode));
        }
#line 897 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 79:
#line 435 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = newTypecastNode(ctx, (yystack_[2].value.asSTNode), (yystack_[0].value.asSTNode));
        }
#line 905 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 80:
#line 442 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = newBinaryOpNode(ctx, STNode::ExprMemberRef, (yystack_[2].value.asSTNode), (yystack_[0].value.asSTNode));
        }
#line 913 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 82:
#line 455 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = (yystack_[2].value.asSTNode);
            (yylhs.value.asSTNode)->setChild(0, (yystack_[0].value.asSTNode));
        }
#line 922 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 83:
#line 460 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = nullptr;
        }
#line 930 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 85:
#line 468 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = (yystack_[2].value.asSTNode);
            (yylhs.value.asSTNode)->setChild(0, (yystack_[0].value.asSTNode));
        }
#line 939 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 86:
#line 473 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = newVarArgsNode(ctx, (yystack_[1].value.asSymbol));
        }
#line 947 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 87:
#line 477 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = nullptr;
        }
#line 955 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 88:
#line 484 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = newFunctionCallNode(ctx, (yystack_[3].value.asSymbol), (yystack_[1].value.asSTNode));
        }
#line 963 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 89:
#line 490 "source/lib/parser.yxx" // lalr1.cc:859
    { ctx.varInfo->beginFunc(ctx, (yystack_[0].value.asSymbol)); }
#line 969 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 90:
#line 491 "source/lib/parser.yxx" // lalr1.cc:859
    { registerNewFunction(ctx, (yystack_[5].value.asSymbol), (yystack_[2].value.asSTNode), (yystack_[0].value.asSTNode)); }
#line 975 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 91:
#line 492 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = newFunctionDeclNode(ctx, (yystack_[8].value.asSymbol), (yystack_[5].value.asSTNode), (yystack_[3].value.asSTNode), (yystack_[1].value.asSTNode));
            ctx.varInfo->endFunc(ctx);
        }
#line 984 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 92:
#line 501 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = (yystack_[0].value.asSTNode);
        }
#line 992 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 93:
#line 505 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = newTypeIdNode(ctx, STEval::Void);
        }
#line 1000 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 94:
#line 516 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = newArrayLiteralNode(ctx, (yystack_[1].value.asSTNode));
        }
#line 1008 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 95:
#line 523 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = newArraySubscriptNode(ctx, (yystack_[3].value.asSTNode), (yystack_[1].value.asSTNode));
        }
#line 1016 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 96:
#line 534 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = newObjConstructorCallNode(ctx, (yystack_[3].value.asSTNode), (yystack_[1].value.asSTNode));
        }
#line 1024 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 97:
#line 546 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = newVarDeclNode(ctx, (yystack_[2].value.asSymbol), nullptr, (yystack_[0].value.asSTNode), (yystack_[0].value.asSTNode)->evalType);
        }
#line 1032 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 98:
#line 554 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = newVarDeclNode(ctx, (yystack_[5].value.asSymbol), (yystack_[1].value.asSTNode), (yystack_[3].value.asSTNode), (yystack_[3].value.asSTNode)->evalType);
        }
#line 1040 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 99:
#line 558 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = newVarDeclNode(ctx, (yystack_[3].value.asSymbol), nullptr, (yystack_[1].value.asSTNode), (yystack_[1].value.asSTNode)->evalType);
        }
#line 1048 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 100:
#line 562 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = newVarDeclNode(ctx, (yystack_[3].value.asSymbol), (yystack_[1].value.asSTNode), nullptr, (yystack_[1].value.asSTNode)->evalType);
        }
#line 1056 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 101:
#line 573 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = newCompareNode(ctx, STNode::IfThenStatement, (yystack_[3].value.asSTNode), (yystack_[1].value.asSTNode), nullptr);
        }
#line 1064 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 102:
#line 577 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = newCompareNode(ctx, STNode::IfThenElseStatement, (yystack_[3].value.asSTNode), (yystack_[1].value.asSTNode), (yystack_[0].value.asSTNode));
        }
#line 1072 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 103:
#line 581 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = newCompareNode(ctx, STNode::IfThenElseIfStatement, (yystack_[3].value.asSTNode), (yystack_[1].value.asSTNode), (yystack_[0].value.asSTNode));
        }
#line 1080 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 104:
#line 588 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = newCompareNode(ctx, STNode::IfThenElseIfStatement, (yystack_[3].value.asSTNode), (yystack_[1].value.asSTNode), (yystack_[0].value.asSTNode));
        }
#line 1088 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 105:
#line 594 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = (yystack_[1].value.asSTNode); }
#line 1094 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 108:
#line 600 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = nullptr; }
#line 1100 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 109:
#line 608 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newReturnNode(ctx, nullptr); }
#line 1106 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 110:
#line 609 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newReturnNode(ctx, (yystack_[1].value.asSTNode)); }
#line 1112 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 111:
#line 613 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newLoopJumpNode(ctx, STNode::BreakStatement); }
#line 1118 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 112:
#line 617 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newLoopJumpNode(ctx, STNode::ContinueStatement); }
#line 1124 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 113:
#line 621 "source/lib/parser.yxx" // lalr1.cc:859
    { ctx.varInfo->insideLoopStmt = true; }
#line 1130 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 114:
#line 622 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = newLoopNode(ctx, STNode::LoopStatement, nullptr, (yystack_[1].value.asSTNode));
            ctx.varInfo->insideLoopStmt = false;
        }
#line 1139 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 115:
#line 629 "source/lib/parser.yxx" // lalr1.cc:859
    { ctx.varInfo->insideLoopStmt = true; }
#line 1145 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 116:
#line 630 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = newLoopNode(ctx, STNode::WhileStatement, (yystack_[4].value.asSTNode), (yystack_[1].value.asSTNode));
            ctx.varInfo->insideLoopStmt = false;
        }
#line 1154 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 117:
#line 638 "source/lib/parser.yxx" // lalr1.cc:859
    { ctx.varInfo->beginForLoop(ctx, (yystack_[3].value.asSymbol), (yystack_[1].value.asSTNode)); }
#line 1160 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 118:
#line 639 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = newLoopNode(ctx, STNode::ForStatement, (yystack_[4].value.asSTNode), (yystack_[1].value.asSTNode));
            ctx.varInfo->endForLoop(ctx);
        }
#line 1169 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 125:
#line 665 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = ctx.syntTree->newNode(STNode::MatchStatement, (yystack_[3].value.asSTNode), (yystack_[1].value.asSTNode), nullptr, (yystack_[3].value.asSTNode)->evalType);
        }
#line 1177 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 126:
#line 672 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = ctx.syntTree->newNode(STNode::MatchDefaultStatement, nullptr, (yystack_[1].value.asSTNode), nullptr, STEval::Void);
        }
#line 1185 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 127:
#line 679 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = ctx.syntTree->newNode(STNode::MatchCaseStatement, (yystack_[3].value.asSTNode), (yystack_[1].value.asSTNode), nullptr, (yystack_[3].value.asSTNode)->evalType);
        }
#line 1193 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 128:
#line 686 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = (yystack_[1].value.asSTNode);
            (yylhs.value.asSTNode)->setChild(2, (yystack_[0].value.asSTNode));
        }
#line 1202 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 130:
#line 692 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = nullptr;
        }
#line 1210 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 136:
#line 711 "source/lib/parser.yxx" // lalr1.cc:859
    {
            // First child reserved for parameter_list chains
            (yylhs.value.asSTNode) = ctx.syntTree->newNode(STNode::ExprRange, nullptr, (yystack_[2].value.asSTNode), (yystack_[0].value.asSTNode), STEval::Range);
        }
#line 1219 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 139:
#line 727 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newRangeParamNode(ctx, (yystack_[0].value.asSymbol)); }
#line 1225 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 140:
#line 728 "source/lib/parser.yxx" // lalr1.cc:859
    { (yylhs.value.asSTNode) = newRangeParamNode(ctx, (yystack_[0].value.asSymbol)); }
#line 1231 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 141:
#line 736 "source/lib/parser.yxx" // lalr1.cc:859
    { ctx.varInfo->beginUDT(ctx, (yystack_[1].value.asSymbol), "enum"); }
#line 1237 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 142:
#line 737 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = newEnumDeclNode(ctx, (yystack_[4].value.asSymbol), (yystack_[1].value.asSTNode));
            ctx.varInfo->endUDT(ctx);
        }
#line 1246 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 143:
#line 745 "source/lib/parser.yxx" // lalr1.cc:859
    {
            // Uninitialized enum constant (sequential integer values)
            (yylhs.value.asSTNode) = newEnumConstNode(ctx, (yystack_[0].value.asSymbol), nullptr, STEval::Long);
        }
#line 1255 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 144:
#line 750 "source/lib/parser.yxx" // lalr1.cc:859
    {
            // Constant initialized from compile-time literal value
            (yylhs.value.asSTNode) = newEnumConstNode(ctx, (yystack_[2].value.asSymbol), (yystack_[0].value.asSTNode), (yystack_[0].value.asSTNode)->evalType);
        }
#line 1264 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 145:
#line 755 "source/lib/parser.yxx" // lalr1.cc:859
    {
            // Constant with explicit initializer (a prev constant from this enum)
            (yylhs.value.asSTNode) = newEnumConstNode(ctx, (yystack_[2].value.asSymbol), (yystack_[0].value.asSTNode), (yystack_[0].value.asSTNode)->evalType);
        }
#line 1273 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 146:
#line 760 "source/lib/parser.yxx" // lalr1.cc:859
    {
            // Constant initialized from a reference to a member constant of another enum
            const STEval stEval = enumMemberConstantReference(ctx, (yystack_[2].value.asSTNode), (yystack_[0].value.asSTNode));
            const SyntaxTreeNode * initExpr = newBinaryOpNode(ctx, STNode::ExprMemberRef, (yystack_[2].value.asSTNode), (yystack_[0].value.asSTNode));
            (yylhs.value.asSTNode) = newEnumConstNode(ctx, (yystack_[4].value.asSymbol), initExpr, stEval);
        }
#line 1284 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 148:
#line 771 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = (yystack_[2].value.asSTNode);
            (yylhs.value.asSTNode)->setChild(0, (yystack_[0].value.asSTNode));
        }
#line 1293 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 149:
#line 776 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = nullptr;
        }
#line 1301 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 150:
#line 786 "source/lib/parser.yxx" // lalr1.cc:859
    { requireGlobalScope(ctx, "type alias"); }
#line 1307 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 151:
#line 787 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = newTypeAliasNode(ctx, (yystack_[4].value.asSymbol), (yystack_[2].value.asSTNode));
        }
#line 1315 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 152:
#line 793 "source/lib/parser.yxx" // lalr1.cc:859
    { ctx.varInfo->beginUDT(ctx, (yystack_[1].value.asSymbol), "struct"); }
#line 1321 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 153:
#line 794 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = newStructDeclNode(ctx, (yystack_[4].value.asSymbol), (yystack_[1].value.asSTNode));
            ctx.varInfo->endUDT(ctx);
        }
#line 1330 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 155:
#line 803 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = (yystack_[2].value.asSTNode);
            (yylhs.value.asSTNode)->setChild(0, (yystack_[0].value.asSTNode));
        }
#line 1339 "generated/parser.cpp" // lalr1.cc:859
    break;

  case 156:
#line 808 "source/lib/parser.yxx" // lalr1.cc:859
    {
            (yylhs.value.asSTNode) = nullptr;
        }
#line 1347 "generated/parser.cpp" // lalr1.cc:859
    break;


#line 1351 "generated/parser.cpp" // lalr1.cc:859
            default:
              break;
            }
        }
      catch (const syntax_error& yyexc)
        {
          error (yyexc);
          YYERROR;
        }
      YY_SYMBOL_PRINT ("-> $$ =", yylhs);
      yypop_ (yylen);
      yylen = 0;
      YY_STACK_PRINT ();

      // Shift the result of the reduction.
      yypush_ (YY_NULLPTR, yylhs);
    }
    goto yynewstate;

  /*--------------------------------------.
  | yyerrlab -- here on detecting error.  |
  `--------------------------------------*/
  yyerrlab:
    // If not already recovering from an error, report this error.
    if (!yyerrstatus_)
      {
        ++yynerrs_;
        error (yysyntax_error_ (yystack_[0].state, yyla));
      }


    if (yyerrstatus_ == 3)
      {
        /* If just tried and failed to reuse lookahead token after an
           error, discard it.  */

        // Return failure if at end of input.
        if (yyla.type_get () == yyeof_)
          YYABORT;
        else if (!yyla.empty ())
          {
            yy_destroy_ ("Error: discarding", yyla);
            yyla.clear ();
          }
      }

    // Else will try to reuse lookahead token after shifting the error token.
    goto yyerrlab1;


  /*---------------------------------------------------.
  | yyerrorlab -- error raised explicitly by YYERROR.  |
  `---------------------------------------------------*/
  yyerrorlab:

    /* Pacify compilers like GCC when the user code never invokes
       YYERROR and the label yyerrorlab therefore never appears in user
       code.  */
    if (false)
      goto yyerrorlab;
    /* Do not reclaim the symbols of the rule whose action triggered
       this YYERROR.  */
    yypop_ (yylen);
    yylen = 0;
    goto yyerrlab1;

  /*-------------------------------------------------------------.
  | yyerrlab1 -- common code for both syntax error and YYERROR.  |
  `-------------------------------------------------------------*/
  yyerrlab1:
    yyerrstatus_ = 3;   // Each real token shifted decrements this.
    {
      stack_symbol_type error_token;
      for (;;)
        {
          yyn = yypact_[yystack_[0].state];
          if (!yy_pact_value_is_default_ (yyn))
            {
              yyn += yyterror_;
              if (0 <= yyn && yyn <= yylast_ && yycheck_[yyn] == yyterror_)
                {
                  yyn = yytable_[yyn];
                  if (0 < yyn)
                    break;
                }
            }

          // Pop the current state because it cannot handle the error token.
          if (yystack_.size () == 1)
            YYABORT;

          yy_destroy_ ("Error: popping", yystack_[0]);
          yypop_ ();
          YY_STACK_PRINT ();
        }


      // Shift the error token.
      error_token.state = yyn;
      yypush_ ("Shifting", error_token);
    }
    goto yynewstate;

    // Accept.
  yyacceptlab:
    yyresult = 0;
    goto yyreturn;

    // Abort.
  yyabortlab:
    yyresult = 1;
    goto yyreturn;

  yyreturn:
    if (!yyla.empty ())
      yy_destroy_ ("Cleanup: discarding lookahead", yyla);

    /* Do not reclaim the symbols of the rule whose action triggered
       this YYABORT or YYACCEPT.  */
    yypop_ (yylen);
    while (1 < yystack_.size ())
      {
        yy_destroy_ ("Cleanup: popping", yystack_[0]);
        yypop_ ();
      }

    return yyresult;
  }
    catch (...)
      {
        YYCDEBUG << "Exception caught: cleaning lookahead and stack"
                 << std::endl;
        // Do not try to display the values of the reclaimed symbols,
        // as their printer might throw an exception.
        if (!yyla.empty ())
          yy_destroy_ (YY_NULLPTR, yyla);

        while (1 < yystack_.size ())
          {
            yy_destroy_ (YY_NULLPTR, yystack_[0]);
            yypop_ ();
          }
        throw;
      }
  }

  void
  Parser::error (const syntax_error& yyexc)
  {
    error (yyexc.what());
  }

  // Generate an error message.
  std::string
  Parser::yysyntax_error_ (state_type yystate, const symbol_type& yyla) const
  {
    // Number of reported tokens (one for the "unexpected", one per
    // "expected").
    size_t yycount = 0;
    // Its maximum.
    enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
    // Arguments of yyformat.
    char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];

    /* There are many possibilities here to consider:
       - If this state is a consistent state with a default action, then
         the only way this function was invoked is if the default action
         is an error action.  In that case, don't check for expected
         tokens because there are none.
       - The only way there can be no lookahead present (in yyla) is
         if this state is a consistent state with a default action.
         Thus, detecting the absence of a lookahead is sufficient to
         determine that there is no unexpected or expected token to
         report.  In that case, just report a simple "syntax error".
       - Don't assume there isn't a lookahead just because this state is
         a consistent state with a default action.  There might have
         been a previous inconsistent state, consistent state with a
         non-default action, or user semantic action that manipulated
         yyla.  (However, yyla is currently not documented for users.)
       - Of course, the expected token list depends on states to have
         correct lookahead information, and it depends on the parser not
         to perform extra reductions after fetching a lookahead from the
         scanner and before detecting a syntax error.  Thus, state
         merging (from LALR or IELR) and default reductions corrupt the
         expected token list.  However, the list is correct for
         canonical LR with one exception: it will still contain any
         token that will not be accepted due to an error action in a
         later state.
    */
    if (!yyla.empty ())
      {
        int yytoken = yyla.type_get ();
        yyarg[yycount++] = yytname_[yytoken];
        int yyn = yypact_[yystate];
        if (!yy_pact_value_is_default_ (yyn))
          {
            /* Start YYX at -YYN if negative to avoid negative indexes in
               YYCHECK.  In other words, skip the first -YYN actions for
               this state because they are default actions.  */
            int yyxbegin = yyn < 0 ? -yyn : 0;
            // Stay within bounds of both yycheck and yytname.
            int yychecklim = yylast_ - yyn + 1;
            int yyxend = yychecklim < yyntokens_ ? yychecklim : yyntokens_;
            for (int yyx = yyxbegin; yyx < yyxend; ++yyx)
              if (yycheck_[yyx + yyn] == yyx && yyx != yyterror_
                  && !yy_table_value_is_error_ (yytable_[yyx + yyn]))
                {
                  if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                    {
                      yycount = 1;
                      break;
                    }
                  else
                    yyarg[yycount++] = yytname_[yyx];
                }
          }
      }

    char const* yyformat = YY_NULLPTR;
    switch (yycount)
      {
#define YYCASE_(N, S)                         \
        case N:                               \
          yyformat = S;                       \
        break
        YYCASE_(0, YY_("syntax error"));
        YYCASE_(1, YY_("syntax error, unexpected %s"));
        YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
        YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
        YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
        YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
      }

    std::string yyres;
    // Argument number.
    size_t yyi = 0;
    for (char const* yyp = yyformat; *yyp; ++yyp)
      if (yyp[0] == '%' && yyp[1] == 's' && yyi < yycount)
        {
          yyres += yytnamerr_ (yyarg[yyi++]);
          ++yyp;
        }
      else
        yyres += *yyp;
    return yyres;
  }


  const short int Parser::yypact_ninf_ = -186;

  const short int Parser::yytable_ninf_ = -141;

  const short int
  Parser::yypact_[] =
  {
    -186,    16,   317,  -186,   441,   441,   441,  -186,  -186,  -186,
    -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,   -37,   441,
     379,   -42,   -30,   441,   -26,   441,  -186,   -10,    -6,    -8,
       5,  -186,    57,  -186,  -186,    86,   441,   441,   -59,  -186,
    -186,  -186,   100,    94,  -186,   472,  -186,  -186,  -186,    95,
    -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,
    -186,  -186,  -186,  -186,    97,  -186,  -186,  -186,    20,    20,
      20,   102,   738,  -186,   542,     0,  -186,   128,     1,   925,
      24,   132,   134,    32,   805,  -186,  -186,  -186,    91,   441,
     824,   650,   109,   441,   441,   441,   441,   441,   441,   441,
     441,   441,   441,   441,   441,   441,   441,   441,   441,   441,
     441,   441,   441,   441,  1271,   441,   441,  -186,    93,  -186,
    -186,  -186,   441,  1271,   116,   112,   441,  -186,   255,  1271,
    -186,  -186,   117,   925,   119,  -186,   441,  -186,   122,   114,
    1092,    23,    35,    20,     3,    90,  1059,  1025,   992,   958,
     925,  1224,   137,  1206,   261,     7,  1191,  1158,  1125,  -186,
    -186,  -186,   719,  -186,  -186,   124,  -186,  -186,  -186,   193,
     561,     2,   113,   441,   115,  -186,   112,   153,     9,    12,
     -28,    29,   145,    85,  -186,   127,  -186,  -186,   123,   126,
    -186,  -186,  -186,  -186,  -186,  -186,   441,  -186,  -186,  -186,
    -186,   441,  -186,    61,   130,   133,   118,   129,  -186,  -186,
    -186,  -186,  -186,   255,  -186,  -186,   135,   205,   139,   153,
     138,   158,   153,   255,   891,   631,  1271,  -186,   161,   113,
    -186,   255,  -186,  -186,  -186,   103,   123,  -186,   126,  -186,
    -186,  -186,  -186,  -186,  1271,  -186,  -186,   255,  -186,   255,
    -186,  -186,  -186,  -186,   174,  -186,  -186,   193,  -186,  -186,
    -186,  -186,   163,  -186,  -186,  -186,  -186,   255,  -186,  -186
  };

  const unsigned char
  Parser::yydefact_[] =
  {
       4,     0,     2,     1,     0,     0,     0,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,     0,     0,
       0,     0,     0,     0,     0,     0,   113,     0,     0,     0,
       0,    27,    23,    24,    25,    26,     0,    83,    41,     3,
      44,    39,     0,    43,     5,     0,    71,    70,    74,    73,
       9,    45,    72,    47,    10,    11,    16,    17,    18,    12,
      13,    14,    15,    46,     0,     7,     8,     6,    69,    68,
      67,     0,     0,   109,     0,     0,    89,    44,    43,     0,
      73,    72,     0,     0,     0,     4,   111,   112,     0,     0,
       0,    81,     0,    83,    83,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    19,     0,    42,
       4,   110,     0,     0,     0,   130,     0,   115,     0,     0,
     141,   152,    39,    76,     0,    75,    83,    94,     0,     0,
      53,    63,    62,    66,    65,    64,    52,    51,    50,    49,
      48,    59,    61,    58,    60,    57,    56,    55,    54,    40,
      79,    80,     0,   139,   140,    41,   138,   137,   136,     0,
       0,     0,    87,     0,     0,   129,   130,     0,    43,    73,
      45,    72,     0,    46,     4,    21,   114,   150,   149,   156,
      78,    82,    88,    96,    95,     4,     0,   101,   103,   102,
     100,     0,    99,     0,     0,    84,    44,     0,   124,     4,
     128,   125,   117,     0,    20,    22,     0,   143,   147,     0,
       0,   154,     0,     0,     0,     0,     0,    86,    93,    87,
       4,     0,     4,   116,   151,     0,   149,   142,   156,   153,
     105,     4,    98,    97,     0,    90,    85,     0,   126,     0,
      23,    26,    41,   144,   145,   148,   155,     0,    92,     4,
     127,   118,     0,   108,   106,   107,   104,     0,   146,    91
  };

  const short int
  Parser::yypgoto_[] =
  {
    -186,  -186,   -57,  -186,  -186,  -167,   -22,   152,   -89,   -23,
    -186,    13,  -186,  -186,  -186,  -186,   -35,    19,    14,  -186,
    -186,  -186,  -186,  -119,     4,  -186,  -185,  -186,  -186,     6,
      15,  -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,  -186,
    -186,  -186,  -186,  -186,  -186,  -186,    73,    78,   -82,   143,
    -186,  -186,  -186,    21,  -186,  -186,  -186,  -186,    26
  };

  const short int
  Parser::yydefgoto_[] =
  {
      -1,     1,     2,    39,   215,   186,    40,    41,    42,    43,
      44,    45,   134,    46,    47,    48,    92,   204,    49,    50,
     124,   259,   245,    51,    52,    53,   205,    54,    55,   198,
     199,   266,    56,    57,    58,    59,    85,    60,   184,    61,
     232,    82,   207,    62,   175,   176,   177,   182,    63,    64,
      65,   188,   218,   219,    66,   216,    67,   189,   222
  };

  const short int
  Parser::yytable_[] =
  {
      78,    77,   197,   122,   221,   201,    93,   180,   -40,    98,
     211,    96,    97,    98,    99,   100,     3,    68,    69,    70,
    -132,   106,   107,   108,   109,   160,    71,    81,   128,    98,
      99,   100,    72,    74,   171,    75,    79,    80,    84,    96,
     187,    98,    99,   100,   183,  -120,   233,    76,  -132,    90,
      91,    83,   237,   221,   180,   239,   240,  -135,   138,   139,
    -134,   114,    86,   169,   248,   114,    87,   115,  -122,    88,
      89,   115,   116,   123,   202,  -138,   116,  -133,   114,   126,
     260,   114,   261,  -138,   115,  -135,  -137,   115,  -134,   116,
     263,   183,   116,   114,   129,   166,    98,    99,  -137,   115,
     269,   191,   133,   178,   116,  -133,    91,    91,   140,   141,
     142,   143,   144,   145,   146,   147,   148,   149,   150,   151,
     152,   153,   154,   155,   156,   157,   158,   213,   161,   162,
     181,  -139,   167,  -131,   226,   170,   227,   243,   223,    79,
     179,    96,    97,    98,    99,   100,   130,   131,   114,    91,
     178,   206,   231,   163,   115,   258,   164,   173,   174,   116,
    -140,  -131,    31,   250,    33,    34,   251,    94,  -138,  -137,
     165,   118,  -119,   247,   119,   249,  -121,   181,   125,   137,
     252,   172,   193,   -77,   257,   190,    79,   179,   192,    93,
     203,   209,   185,   212,  -123,   114,   228,     4,     5,   214,
     217,   115,   267,   220,   229,   230,   116,   234,   235,   224,
     236,   226,   254,   253,   225,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,   238,
     195,   196,   185,    20,    21,    22,    23,   244,   262,   268,
     252,   132,    24,    25,    26,    27,    28,    29,   246,   210,
      30,   208,    31,    32,    33,    34,    35,   255,    36,     4,
       5,   168,    37,   264,   256,    96,    97,    98,    99,   100,
      38,     0,   265,     0,     0,     0,   107,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,     0,     0,   185,    20,    21,    22,    23,     0,
       0,     0,     0,     0,    24,    25,    26,    27,    28,    29,
       0,     0,    30,     0,    31,    32,    33,    34,    35,   114,
      36,     4,     5,     0,    37,   115,     0,     0,     0,     0,
     116,     0,    38,     0,     0,     0,     0,     0,     0,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,     0,     0,     0,    20,    21,    22,
      23,     0,     0,     0,     0,     0,    24,    25,    26,    27,
      28,    29,     0,     0,    30,     0,    31,    32,    33,    34,
      35,     0,    36,     4,     5,     0,    37,     0,     0,     0,
       0,     0,     0,     0,    38,     0,     0,     0,     0,     0,
       0,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    30,     0,    31,    32,
      33,    34,    35,     0,    36,     4,     5,     0,    37,     0,
       0,    73,     0,     0,     0,     0,    38,     0,     0,     0,
       0,     0,     0,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    95,    96,    97,    98,    99,
     100,   101,   102,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,     0,     0,     0,     0,    30,     0,
      31,    32,    33,    34,    35,     0,    36,     0,     0,     0,
      37,     0,     0,     0,     0,     0,     0,     0,    38,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     114,     0,     0,     0,     0,     0,   115,     0,     0,     0,
       0,   116,     0,     0,   117,    95,    96,    97,    98,    99,
     100,   101,   102,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     114,     0,     0,     0,     0,     0,   115,     0,     0,     0,
       0,   116,     0,     0,   121,     0,     0,     0,     0,   114,
       0,     0,     0,     0,     0,   115,     0,     0,     0,     0,
     116,     0,     0,   200,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,    95,    96,    97,    98,    99,   100,   101,
     102,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   114,
       0,     0,     0,     0,     0,   115,     0,     0,     0,     0,
     116,     0,     0,   242,     0,     0,     0,     0,   114,     0,
       0,     0,     0,     0,   115,     0,     0,     0,     0,   116,
       0,   136,    95,    96,    97,    98,    99,   100,   101,   102,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,    95,    96,    97,    98,    99,   100,   101,   102,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   120,     0,     0,   114,     0,     0,
       0,     0,     0,   115,     0,     0,     0,     0,   116,   194,
       0,     0,     0,     0,     0,     0,   114,     0,     0,     0,
       0,     0,   115,     0,     0,     0,     0,   116,    95,    96,
      97,    98,    99,   100,   101,   102,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,    95,    96,    97,
      98,    99,   100,   101,   102,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,     0,     0,     0,     0,
       0,     0,     0,   127,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   114,     0,     0,     0,     0,     0,   115,
       0,     0,     0,     0,   116,     0,     0,     0,     0,     0,
       0,     0,   114,     0,     0,     0,     0,     0,   115,     0,
     135,     0,     0,   116,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   241,    95,    96,
      97,    98,    99,   100,   101,   102,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,     0,     0,   114,
       0,     0,     0,     0,     0,   115,     0,     0,     0,     0,
     116,    95,    96,    97,    98,    99,   100,   101,   102,   103,
     104,     0,   106,   107,   108,   109,   110,   111,   112,   113,
       0,     0,     0,   114,     0,     0,     0,     0,     0,   115,
       0,     0,     0,     0,   116,    95,    96,    97,    98,    99,
     100,   101,   102,   103,     0,     0,   106,   107,   108,   109,
     110,   111,   112,   113,     0,     0,   114,     0,     0,     0,
       0,     0,   115,     0,     0,     0,     0,   116,    95,    96,
      97,    98,    99,   100,   101,   102,     0,     0,     0,   106,
     107,   108,   109,   110,   111,   112,   113,     0,     0,     0,
     114,     0,     0,     0,     0,     0,   115,     0,     0,     0,
       0,   116,    95,    96,    97,    98,    99,   100,   101,     0,
       0,     0,     0,   106,   107,   108,   109,   110,   111,   112,
     113,     0,     0,   114,     0,     0,     0,     0,     0,   115,
       0,     0,     0,     0,   116,    95,    96,    97,    98,    99,
     100,     0,     0,     0,     0,     0,   106,   107,   108,   109,
     110,   111,   112,   113,     0,     0,     0,   114,     0,     0,
       0,     0,     0,   115,     0,     0,     0,     0,   116,    96,
      97,    98,    99,   100,     0,     0,     0,     0,     0,   106,
     107,   108,   109,   110,   111,   112,     0,     0,     0,     0,
     114,     0,     0,     0,     0,     0,   115,     0,     0,     0,
       0,   116,    96,    97,    98,    99,   100,     0,     0,     0,
       0,     0,   106,   107,   108,   109,   110,   111,     0,     0,
       0,     0,     0,   114,     0,     0,     0,     0,     0,   115,
       0,     0,     0,     0,   116,    96,    97,    98,    99,   100,
       0,     0,     0,     0,     0,   106,   107,   108,   109,   110,
      96,    97,    98,    99,   100,     0,   114,     0,     0,     0,
     106,   107,   115,   109,     0,     0,     0,   116,    96,    97,
      98,    99,   100,     0,     0,     0,     0,     0,     0,   107,
       0,   109,     0,     0,     0,     0,     0,     0,     0,   114,
       0,     0,     0,     0,     0,   115,     0,     0,     0,     0,
     116,     0,     0,     0,   114,     0,     0,     0,     0,     0,
     115,     0,     0,     0,     0,   116,     0,     0,     0,     0,
       0,     0,   114,     0,     0,     0,     0,     0,   115,     0,
       0,     0,     0,   116,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   159
  };

  const short int
  Parser::yycheck_[] =
  {
      23,    23,   169,     3,   189,     3,    65,   126,    67,     6,
     177,     4,     5,     6,     7,     8,     0,     4,     5,     6,
      48,    14,    15,    16,    17,   114,    63,    23,    85,     6,
       7,     8,    19,    20,   123,    77,    23,    23,    25,     4,
     129,     6,     7,     8,   126,    44,   213,    77,    76,    36,
      37,    77,   219,   238,   173,   222,   223,    48,    93,    94,
      48,    58,    72,   120,   231,    58,    72,    64,    44,    77,
      65,    64,    69,    73,    72,    74,    69,    48,    58,    47,
     247,    58,   249,    74,    64,    76,    74,    64,    76,    69,
     257,   173,    69,    58,     3,   118,     6,     7,    74,    64,
     267,   136,    89,   126,    69,    76,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   184,   115,   116,
     126,    74,   118,    48,    73,   122,    75,   226,   195,   126,
     126,     4,     5,     6,     7,     8,    55,    56,    58,   136,
     173,   173,   209,    60,    64,   244,    63,    45,    46,    69,
      74,    76,    59,    60,    61,    62,    63,    67,    74,    74,
      77,    74,    44,   230,    72,   232,    44,   173,    44,    70,
      77,    65,    68,    66,   241,    66,   173,   173,    66,    65,
      77,    76,    39,    48,    76,    58,    66,     4,     5,    72,
      77,    64,   259,    77,    71,    76,    69,    72,     3,   196,
      71,    73,   235,   235,   201,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    71,
      37,    38,    39,    40,    41,    42,    43,    76,    64,   262,
      77,    89,    49,    50,    51,    52,    53,    54,   229,   176,
      57,   173,    59,    60,    61,    62,    63,   236,    65,     4,
       5,   118,    69,   257,   238,     4,     5,     6,     7,     8,
      77,    -1,   257,    -1,    -1,    -1,    15,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    -1,    -1,    -1,    39,    40,    41,    42,    43,    -1,
      -1,    -1,    -1,    -1,    49,    50,    51,    52,    53,    54,
      -1,    -1,    57,    -1,    59,    60,    61,    62,    63,    58,
      65,     4,     5,    -1,    69,    64,    -1,    -1,    -1,    -1,
      69,    -1,    77,    -1,    -1,    -1,    -1,    -1,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    -1,    -1,    -1,    -1,    40,    41,    42,
      43,    -1,    -1,    -1,    -1,    -1,    49,    50,    51,    52,
      53,    54,    -1,    -1,    57,    -1,    59,    60,    61,    62,
      63,    -1,    65,     4,     5,    -1,    69,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    77,    -1,    -1,    -1,    -1,    -1,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    57,    -1,    59,    60,
      61,    62,    63,    -1,    65,     4,     5,    -1,    69,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    77,    -1,    -1,    -1,
      -1,    -1,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    -1,    -1,    -1,    -1,    57,    -1,
      59,    60,    61,    62,    63,    -1,    65,    -1,    -1,    -1,
      69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    77,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      58,    -1,    -1,    -1,    -1,    -1,    64,    -1,    -1,    -1,
      -1,    69,    -1,    -1,    72,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      58,    -1,    -1,    -1,    -1,    -1,    64,    -1,    -1,    -1,
      -1,    69,    -1,    -1,    72,    -1,    -1,    -1,    -1,    58,
      -1,    -1,    -1,    -1,    -1,    64,    -1,    -1,    -1,    -1,
      69,    -1,    -1,    72,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    58,
      -1,    -1,    -1,    -1,    -1,    64,    -1,    -1,    -1,    -1,
      69,    -1,    -1,    72,    -1,    -1,    -1,    -1,    58,    -1,
      -1,    -1,    -1,    -1,    64,    -1,    -1,    -1,    -1,    69,
      -1,    71,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    36,    -1,    -1,    58,    -1,    -1,
      -1,    -1,    -1,    64,    -1,    -1,    -1,    -1,    69,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    58,    -1,    -1,    -1,
      -1,    -1,    64,    -1,    -1,    -1,    -1,    69,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    58,    -1,    -1,    -1,    -1,    -1,    64,
      -1,    -1,    -1,    -1,    69,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    58,    -1,    -1,    -1,    -1,    -1,    64,    -1,
      66,    -1,    -1,    69,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    36,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    -1,    -1,    58,
      -1,    -1,    -1,    -1,    -1,    64,    -1,    -1,    -1,    -1,
      69,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    -1,    14,    15,    16,    17,    18,    19,    20,    21,
      -1,    -1,    -1,    58,    -1,    -1,    -1,    -1,    -1,    64,
      -1,    -1,    -1,    -1,    69,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    14,    15,    16,    17,
      18,    19,    20,    21,    -1,    -1,    58,    -1,    -1,    -1,
      -1,    -1,    64,    -1,    -1,    -1,    -1,    69,     3,     4,
       5,     6,     7,     8,     9,    10,    -1,    -1,    -1,    14,
      15,    16,    17,    18,    19,    20,    21,    -1,    -1,    -1,
      58,    -1,    -1,    -1,    -1,    -1,    64,    -1,    -1,    -1,
      -1,    69,     3,     4,     5,     6,     7,     8,     9,    -1,
      -1,    -1,    -1,    14,    15,    16,    17,    18,    19,    20,
      21,    -1,    -1,    58,    -1,    -1,    -1,    -1,    -1,    64,
      -1,    -1,    -1,    -1,    69,     3,     4,     5,     6,     7,
       8,    -1,    -1,    -1,    -1,    -1,    14,    15,    16,    17,
      18,    19,    20,    21,    -1,    -1,    -1,    58,    -1,    -1,
      -1,    -1,    -1,    64,    -1,    -1,    -1,    -1,    69,     4,
       5,     6,     7,     8,    -1,    -1,    -1,    -1,    -1,    14,
      15,    16,    17,    18,    19,    20,    -1,    -1,    -1,    -1,
      58,    -1,    -1,    -1,    -1,    -1,    64,    -1,    -1,    -1,
      -1,    69,     4,     5,     6,     7,     8,    -1,    -1,    -1,
      -1,    -1,    14,    15,    16,    17,    18,    19,    -1,    -1,
      -1,    -1,    -1,    58,    -1,    -1,    -1,    -1,    -1,    64,
      -1,    -1,    -1,    -1,    69,     4,     5,     6,     7,     8,
      -1,    -1,    -1,    -1,    -1,    14,    15,    16,    17,    18,
       4,     5,     6,     7,     8,    -1,    58,    -1,    -1,    -1,
      14,    15,    64,    17,    -1,    -1,    -1,    69,     4,     5,
       6,     7,     8,    -1,    -1,    -1,    -1,    -1,    -1,    15,
      -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    58,
      -1,    -1,    -1,    -1,    -1,    64,    -1,    -1,    -1,    -1,
      69,    -1,    -1,    -1,    58,    -1,    -1,    -1,    -1,    -1,
      64,    -1,    -1,    -1,    -1,    69,    -1,    -1,    -1,    -1,
      -1,    -1,    58,    -1,    -1,    -1,    -1,    -1,    64,    -1,
      -1,    -1,    -1,    69,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    77
  };

  const unsigned char
  Parser::yystos_[] =
  {
       0,    81,    82,     0,     4,     5,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      40,    41,    42,    43,    49,    50,    51,    52,    53,    54,
      57,    59,    60,    61,    62,    63,    65,    69,    77,    83,
      86,    87,    88,    89,    90,    91,    93,    94,    95,    98,
      99,   103,   104,   105,   107,   108,   112,   113,   114,   115,
     117,   119,   123,   128,   129,   130,   134,   136,    91,    91,
      91,    63,    91,    72,    91,    77,    77,    86,    89,    91,
      98,   104,   121,    77,    91,   116,    72,    72,    77,    65,
      91,    91,    96,    65,    67,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    58,    64,    69,    72,    74,    72,
      36,    72,     3,    73,   100,    44,    47,    48,    82,     3,
      55,    56,    87,    91,    92,    66,    71,    70,    96,    96,
      91,    91,    91,    91,    91,    91,    91,    91,    91,    91,
      91,    91,    91,    91,    91,    91,    91,    91,    91,    77,
      88,    91,    91,    60,    63,    77,    89,    98,   129,    82,
      91,    88,    65,    45,    46,   124,   125,   126,    89,    98,
     103,   104,   127,   128,   118,    39,    85,    88,   131,   137,
      66,    96,    66,    68,    70,    37,    38,    85,   109,   110,
      72,     3,    72,    77,    97,   106,    86,   122,   127,    76,
     126,    85,    48,    82,    72,    84,   135,    77,   132,   133,
      77,   106,   138,    82,    91,    91,    73,    75,    66,    71,
      76,    82,   120,    85,    72,     3,    71,    85,    71,    85,
      85,    36,    72,    88,    76,   102,    97,    82,    85,    82,
      60,    63,    77,    86,    89,   133,   138,    82,    88,   101,
      85,    85,    64,    85,   109,   110,   111,    82,    89,    85
  };

  const unsigned char
  Parser::yyr1_[] =
  {
       0,    80,    81,    82,    82,    83,    83,    83,    83,    83,
      83,    83,    83,    83,    83,    83,    83,    83,    83,    83,
      84,    84,    85,    86,    86,    86,    86,    86,    87,    87,
      87,    87,    87,    87,    87,    87,    87,    87,    87,    88,
      88,    89,    90,    91,    91,    91,    91,    91,    91,    91,
      91,    91,    91,    91,    91,    91,    91,    91,    91,    91,
      91,    91,    91,    91,    91,    91,    91,    91,    91,    91,
      91,    91,    91,    91,    91,    91,    92,    92,    93,    94,
      95,    96,    96,    96,    97,    97,    97,    97,    98,   100,
     101,    99,   102,   102,   103,   104,   105,   106,   107,   107,
     107,   108,   108,   108,   109,   110,   111,   111,   111,   112,
     112,   113,   114,   116,   115,   118,   117,   120,   119,   121,
     121,   121,   121,   122,   122,   123,   124,   125,   126,   126,
     126,   127,   127,   127,   127,   127,   128,   129,   129,   129,
     129,   131,   130,   132,   132,   132,   132,   133,   133,   133,
     135,   134,   137,   136,   138,   138,   138
  };

  const unsigned char
  Parser::yyr2_[] =
  {
       0,     2,     1,     2,     0,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       1,     0,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     3,     1,     1,     1,     1,     1,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     2,     2,     2,
       1,     1,     1,     1,     1,     3,     1,     1,     4,     3,
       3,     1,     3,     0,     1,     3,     2,     0,     4,     0,
       0,    10,     2,     0,     3,     4,     4,     3,     7,     5,
       5,     5,     5,     5,     5,     3,     1,     1,     1,     2,
       3,     2,     2,     0,     4,     0,     6,     0,     8,     1,
       1,     1,     1,     1,     1,     5,     4,     5,     2,     1,
       0,     1,     1,     1,     1,     1,     3,     1,     1,     1,
       1,     0,     6,     1,     3,     3,     5,     1,     3,     0,
       0,     6,     0,     6,     1,     3,     0
  };



  // YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
  // First, the terminals, then, starting at \a yyntokens_, nonterminals.
  const char*
  const Parser::yytname_[] =
  {
  "\"end of file\"", "error", "$undefined", "\"=\"", "\"+\"", "\"-\"",
  "\"*\"", "\"/\"", "\"%\"", "\"+=\"", "\"-=\"", "\"*=\"", "\"/=\"",
  "\"%=\"", "\">\"", "\"<\"", "\">=\"", "\"<=\"", "\"==\"", "\"!=\"",
  "\"and\"", "\"or\"", "\"not\"", "\"int\"", "\"long\"", "\"float\"",
  "\"double\"", "\"bool\"", "\"str\"", "\"array\"", "\"range\"", "\"any\"",
  "\"tid\"", "\"function\"", "\"import\"", "\"if\"", "\"then\"",
  "\"else\"", "\"elseif\"", "\"end\"", "\"return\"", "\"let\"", "\"func\"",
  "\"match\"", "\"with\"", "\"case\"", "\"default\"", "\"in\"", "\"do\"",
  "\"for\"", "\"while\"", "\"loop\"", "\"break\"", "\"continue\"",
  "\"type\"", "\"enum\"", "\"struct\"", "\"type_of\"", "\"as\"",
  "\"null\"", "\"literal integer constant\"",
  "\"literal floating-point constant\"", "\"literal boolean constant\"",
  "\"literal string constant\"", "\".\"", "\"(\"", "\")\"", "\"{\"",
  "\"}\"", "\"[\"", "\"]\"", "\",\"", "\";\"", "\":\"", "\"..\"",
  "\"...\"", "\"->\"", "\"identifier\"", "UNARY_MINUS", "UNARY_PLUS",
  "$accept", "translation_unit", "optional_statement_list", "statement",
  "optional_semicolon", "end_statement", "literal_const_expression",
  "builtin_type", "type_identifier", "identifier_expression",
  "import_statement", "expression", "expression_or_type_identifier",
  "typeof_expression", "typecast_expression", "member_ref_expression",
  "parameter_list", "func_parameter_list_declaration",
  "func_call_expression", "func_declaration", "$@1", "$@2",
  "optional_return_type", "array_expression", "array_subscript_expression",
  "object_constructor_expression", "typed_var_declaration",
  "let_var_declaration", "if_statement", "elseif_statement",
  "else_statement", "elseif_or_else_or_end", "return_statement",
  "break_statement", "continue_statement", "loop_statement", "$@3",
  "while_statement", "$@4", "for_statement", "$@5", "match_param",
  "match_case_param", "match_statement", "match_default", "match_case",
  "match_case_list", "range_or_array_or_ident_or_call", "range_expression",
  "range_param", "enum_declaration", "$@6", "enum_constant",
  "enum_constant_list", "type_alias_declaration", "$@7",
  "struct_declaration", "$@8", "struct_member_list", YY_NULLPTR
  };

#if YYDEBUG
  const unsigned short int
  Parser::yyrline_[] =
  {
       0,   297,   297,   302,   303,   307,   308,   309,   310,   311,
     312,   313,   314,   315,   316,   317,   318,   319,   320,   321,
     325,   326,   330,   334,   335,   336,   337,   338,   342,   343,
     344,   345,   346,   347,   348,   349,   350,   351,   352,   356,
     357,   361,   365,   381,   382,   383,   384,   385,   386,   387,
     388,   389,   390,   391,   392,   393,   394,   395,   396,   397,
     398,   399,   400,   401,   402,   403,   404,   405,   406,   407,
     408,   409,   410,   411,   412,   413,   422,   423,   427,   434,
     441,   453,   454,   459,   466,   467,   472,   476,   483,   490,
     491,   490,   500,   504,   515,   522,   533,   545,   553,   557,
     561,   572,   576,   580,   587,   594,   598,   599,   600,   608,
     609,   613,   617,   621,   621,   629,   629,   638,   637,   651,
     652,   653,   654,   659,   660,   664,   671,   678,   685,   690,
     691,   702,   703,   704,   705,   706,   710,   725,   726,   727,
     728,   736,   736,   744,   749,   754,   759,   769,   770,   775,
     786,   786,   793,   793,   801,   802,   807
  };

  // Print the state stack on the debug stream.
  void
  Parser::yystack_print_ ()
  {
    *yycdebug_ << "Stack now";
    for (stack_type::const_iterator
           i = yystack_.begin (),
           i_end = yystack_.end ();
         i != i_end; ++i)
      *yycdebug_ << ' ' << i->state;
    *yycdebug_ << std::endl;
  }

  // Report on the debug stream that the rule \a yyrule is going to be reduced.
  void
  Parser::yy_reduce_print_ (int yyrule)
  {
    unsigned int yylno = yyrline_[yyrule];
    int yynrhs = yyr2_[yyrule];
    // Print the symbols being reduced, and their result.
    *yycdebug_ << "Reducing stack by rule " << yyrule - 1
               << " (line " << yylno << "):" << std::endl;
    // The symbols being reduced.
    for (int yyi = 0; yyi < yynrhs; yyi++)
      YY_SYMBOL_PRINT ("   $" << yyi + 1 << " =",
                       yystack_[(yynrhs) - (yyi + 1)]);
  }
#endif // YYDEBUG

  // Symbol number corresponding to token number t.
  inline
  Parser::token_number_type
  Parser::yytranslate_ (int t)
  {
    static
    const token_number_type
    translate_table[] =
    {
     0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79
    };
    const unsigned int user_token_number_max_ = 334;
    const token_number_type undef_token_ = 2;

    if (static_cast<int>(t) <= yyeof_)
      return yyeof_;
    else if (static_cast<unsigned int> (t) <= user_token_number_max_)
      return translate_table[t];
    else
      return undef_token_;
  }

#line 17 "source/lib/parser.yxx" // lalr1.cc:1167
} // moon
#line 2195 "generated/parser.cpp" // lalr1.cc:1167
#line 813 "source/lib/parser.yxx" // lalr1.cc:1168

