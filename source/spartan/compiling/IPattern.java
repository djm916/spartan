package spartan.compiling;

import spartan.runtime.Env;
import spartan.data.Datum;
import spartan.data.List;
import spartan.data.Record;
import spartan.data.RecordDescriptor;
import spartan.data.Symbol;
import spartan.data.Vector;
import spartan.data.TypeRegistry;
import spartan.data.IEq;

public sealed interface IPattern
permits MatchAny, MatchVar, MatchEqual, MatchList, MatchEmpty, MatchVector, MatchRecord, MatchOr, MatchAnd, MatchFail
{
  boolean match(Datum arg, Env env);
}

record MatchAny() implements IPattern
{
  public boolean match(Datum arg, Env env)
  {
    return true;
  }
}

record MatchVar(int offset) implements IPattern
{
  public boolean match(Datum arg, Env env)
  {
    env.set(offset, arg);
    return true;
  }
}

record MatchEmpty() implements IPattern
{
  public boolean match(Datum arg, Env env)
  {
    return arg == List.EMPTY;
  }
}

record MatchList(IPattern first, IPattern rest) implements IPattern
{
  public boolean match(Datum arg, Env env)
  {
    return (arg instanceof List list) && !list.isEmpty()
        && first.match(list.first(), env) && rest.match(list.rest(), env);
  }
}

record MatchEqual(IEq value) implements IPattern
{
  public boolean match(Datum arg, Env env)
  {
    return (arg instanceof IEq) && value.isEqual((IEq)arg);
  }
}

record MatchVector(IPattern[] patterns) implements IPattern
{
  public boolean match(Datum arg, Env env)
  {
    return (arg instanceof Vector vector)
        && vector.length() == patterns.length
        && elemsMatch(vector, env);
  }
  
  private boolean elemsMatch(Vector vector, Env env)
  {
    for (int i = 0; i < patterns.length; ++i)
      if (!patterns[i].match(vector.get(i), env))
        return false;
    return true;
  }
}

record MatchRecord(RecordDescriptor rtd, IPattern[] patterns) implements IPattern
{
  public boolean match(Datum arg, Env env)
  {
    return arg instanceof Record(var rtd2, var fields)
        && rtd == rtd2
        && fieldsMatch(fields, env);
  }
  
  private boolean fieldsMatch(Datum[] fields, Env env)
  {
    for (int i = 0; i < patterns.length; ++i)
      if (!patterns[i].match(fields[i], env))
        return false;
    return true;
  }
}

record MatchFail() implements IPattern
{
  public boolean match(Datum arg, Env env)
  {
    return false;
  }
}

record MatchAnd(IPattern first, IPattern rest) implements IPattern
{
  public boolean match(Datum arg, Env env)
  {
    return first.match(arg, env) && rest.match(arg, env);
  }
}

record MatchOr(IPattern first, IPattern rest) implements IPattern
{
  public boolean match(Datum arg, Env env)
  {
    return first.match(arg, env) || rest.match(arg, env);
  }
}
