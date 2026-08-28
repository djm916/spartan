package spartan.compiling;

import spartan.data.Datum;
import spartan.data.List;
import spartan.data.Symbol;
import spartan.data.IEq;

class CompilerUtils
{
  /* Check that a binding list for a "let" expression is well-formed */
  
  static boolean checkBindingList(List bindings)
  {
    for (; !bindings.isEmpty(); bindings = bindings.rest())
      if (!(bindings.first() instanceof List list && list.length() == 2 && list.first() instanceof Symbol s && s.isSimple()))
        return false;
    return true;
  }
  
  /* Check that a binding list for a "rep" expression is well-formed */
  
  static boolean checkRepBindings(List bindings)
  {
    for (; !bindings.isEmpty(); bindings = bindings.rest())
      if (!(bindings.first() instanceof List list && list.length() == 3 && list.first() instanceof Symbol s && s.isSimple()))
        return false;
    return true;
  }
  
  /* Check that a parameter list is well-formed according to the grammar:
 
     <parameter-list> => "(" <symbol>* [:option (<symbol> <exp>)+] [:rest <symbol>] ")"
   */
  static boolean checkParamList(List params)
  {
    for (; !params.isEmpty(); params = params.rest()) {
      if (Symbol.OPTARG.equals(params.first()))
        break;
      if (Symbol.RESTARG.equals(params.first()))
        break;
      if (!(params.first() instanceof Symbol s && s.isSimple()))
        return false;
    }
    if (!params.isEmpty() && Symbol.OPTARG.equals(params.first())) {
      params = params.rest();
      for (; !params.isEmpty(); params = params.rest()) {
        if (Symbol.RESTARG.equals(params.first()))
          break;
        if (!(params.first() instanceof List pair && pair.length() == 2
            && pair.first() instanceof Symbol s && s.isSimple()))
          return false;
      }
    }
    if (!params.isEmpty() && Symbol.RESTARG.equals(params.first())) {
      params = params.rest();
      if (!(!params.isEmpty() && params.first() instanceof Symbol s && s.isSimple()))
        return false;
      params = params.rest();
    }
    if (!params.isEmpty())
      return false;
    return true;
  }
  
  /* Check that a clause list is well-formed
   * 
   * <clause-list> => "(" <clause>+ <else-clause>? ")"
   * <clause> => "(" <expr> <expr>+ ")"
   * <else-clause> => "(" "else" <expr>+ ")"
   */
  static boolean checkCondClauses(List clauses)
  {
    for (; !clauses.isEmpty(); clauses = clauses.rest())
      if (!(clauses.first() instanceof List clause) || clause.length() < 2 || (Symbol.ELSE.equals(clause.first()) && !clauses.rest().isEmpty()))
        return false;
    return true;
  }
  
  /* Check the clauses in a match expression */
  static boolean checkMatchClauses(List clauses)
  {
    for (; !clauses.isEmpty(); clauses = clauses.rest())
      if (!(clauses.first() instanceof List clause && clause.length() >= 2))
        return false;
    return true;
  }
  
  /* Check a quote pattern
   * 
   * Syntax: (quote <datum>)
   *
   * Checks that <datum> is equality-comparable (i.e., implements in IEq interface)
   */
  static boolean checkQuotePattern(List pattern)
  {
    return pattern.length() == 2 && pattern.second() instanceof IEq;
  }
  
  /* Check an "or" pattern */
  static boolean checkOrPattern(List patterns)
  {
    var vars = patternVars(patterns.first());
    for (patterns = patterns.rest(); !patterns.isEmpty(); patterns = patterns.rest())
      if (!vars.equals(patternVars(patterns.first())))
        return false;
    return true;
  }
  
  /* Check a record pattern
     
     Syntax: (record <type-name> <pattern>...)
     
     Checks that the type name is a symbol and that the number of patterns matches
     the number of fields in the record type.
  */
  static boolean checkRecordPattern(List pattern)
  {
    return pattern.length() >= 2
        && pattern.second() instanceof Symbol typeName
        && spartan.Runtime.lookupRTD(typeName)
           .map(rtd -> rtd.fields().length == pattern.drop2().length())
           .orElse(false);
  }
  
  
  // Extract the first sub-element from each element in a list of lists
  static List extractFirst(List bindings)
  {
    return bindings.map(list -> ((List)list).first());
  }

  // Extract the second sub-element from each element in a list of lists
  static List extractSecond(List bindings)
  {
    return bindings.map(list -> ((List)list).second());
  }
  
  // Extract the third sub-element from each element in a list of lists
  static List extractThird(List bindings)
  {
    return bindings.map(list -> ((List)list).third());
  }
  
  static List patternVars(Datum pattern)
  {
    if (pattern instanceof Symbol symb && !symb.isKeyword() && !Symbol.UNDERSCORE.equals(symb))
      return List.of(symb);
    if (pattern instanceof List list && !list.isEmpty() && list.first() instanceof Symbol first) {
      if (first.equals(Symbol.LIST))
        return patternVarsInner(list.rest());
      if (first.equals(Symbol.LIST_STAR))
        return patternVarsInner(list.rest());
      if (first.equals(Symbol.VECTOR))
        return patternVarsInner(list.rest());
      if (first.equals(Symbol.RECORD))
        return patternVarsInner(list.drop2());
      if (first.equals(Symbol.AND))
        return patternVarsInner(list.rest());
      if (first.equals(Symbol.OR))
        return patternVarsInner(list.rest());
    }
    return List.EMPTY;
  }
  
  static List patternVarsInner(List patterns)
  {
    if (patterns.isEmpty())
      return List.EMPTY;
    else
      return List.concat2(patternVars(patterns.first()), patternVarsInner(patterns.rest()));
  }
  
  static int matchEnvSize(List clauses)
  {
    int maxVars = 0;
    for (; !clauses.isEmpty(); clauses = clauses.rest()) {
      var clause = (List) clauses.first();
      var patt = clause.first();
      var vars = patternVars(patt);
      var len = vars.length();
      if (len > maxVars)
        maxVars = len;
    }
    return maxVars;
  }
  
  record ParsedParams(List requiredArgs, List optionalArgs, List restArg)
  {};

  /* Parse a parameter list of the form
  
       "(" <symbol>* [:option (<symbol> <exp>)+] [:rest <symbol>] ")"
     
     Returns 3 lists of symbols (each of which may be empty):
     
     requiredArgs - The function's required arguments
     optionalArgs - A list of pairs denoting the function's optional arguments and default values
     restArg      - Contains the rest argument, if any
  */
  static ParsedParams parseParamList(List params)
  {
    var requiredArgs = new List.Builder();
    var optionalArgs = new List.Builder();
    var restArg = List.EMPTY;
    
    // Parse required arguments, stopping at end of list or next keyword
    for (; !params.isEmpty(); params = params.rest()) {
      if (Symbol.OPTARG.equals(params.first()))
        break;
      if (Symbol.RESTARG.equals(params.first()))
        break;
      requiredArgs.add(params.first());
    }
    
    // Parse optional arguments, stopping at end of list or next keyword
    // NOTE: Assumes each optional argument is a binding pair: (<symbol> <exp>)
    if (!params.isEmpty() && Symbol.OPTARG.equals(params.first())) {
      params = params.rest();
      for (; !params.isEmpty(); params = params.rest()) {
        if (Symbol.RESTARG.equals(params.first()))
          break;
        optionalArgs.add(params.first());
      }
    }
    
    // Parse the rest argument
    // NOTE: Assumes exactly 1 symbol in the rest of the parameter list
    if (!params.isEmpty() && Symbol.RESTARG.equals(params.first()))
      restArg = params.rest();
    
    return new ParsedParams(requiredArgs.build(), 
                            optionalArgs.build(),
                            restArg);
  }
  
  
  // Determine if a form is an inner definition (i.e., "def", "defun")
  // These can only appear at the top of a function body
  static boolean isInnerDefinition(Datum exp)
  {
    return exp instanceof List form && !form.isEmpty() && (Symbol.DEF.equals(form.first()) || Symbol.DEFUN.equals(form.first()));
  }

  // Determine if form is a top-level definition (i.e., "def", "defun", "defmacro")
  // These can only appear at the top-level and within "do" forms at the top-level
  static boolean isTopLevelDefinition(Datum exp)
  {
    //return exp instanceof List form && !form.isEmpty() && (Symbol.DEF.equals(form.first()) || Symbol.DEFUN.equals(form.first()) || Symbol.DEFMACRO.equals(form.first()) || Symbol.DO.equals(form.first()));
    return exp instanceof List form && !form.isEmpty() && (Symbol.DEF.equals(form.first()) || Symbol.DEFUN.equals(form.first()) || Symbol.DEFMACRO.equals(form.first()));
  }
  
  
}
