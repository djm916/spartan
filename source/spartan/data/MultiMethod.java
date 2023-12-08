package spartan.data;

import spartan.data.IFun;
import spartan.compiling.Signature;
//import spartan.runtime.LocalEnv;
//import spartan.runtime.Inst;
import spartan.runtime.VirtualMachine;
import spartan.errors.Error;
import spartan.errors.TypeMismatch;
//import spartan.errors.WrongNumberArgs;
import java.util.Optional;
import java.util.Map;
import java.util.HashMap;
import java.util.Arrays;

public final class MultiMethod implements Datum, IFun
{
  private static class TypeSignature
  {
    static TypeSignature fromArgs(List args)
    {
      var result = new String[args.length()];
      for (int i = 0; !args.isEmpty(); i++, args = args.cdr())
        result[i] = args.car().type().name();
      return new TypeSignature(result);
    }
    
    static TypeSignature fromTypes(List types)
    {
      var result = new String[types.length()];
      for (int i = 0; !types.isEmpty(); i++, types = types.cdr())
        result[i] = ((Symbol)types.car()).name();
      return new TypeSignature(result);
    }
    
    @Override
    public boolean equals(Object other)
    {
      return Arrays.equals(this.types, ((TypeSignature)other).types);
    }
    
    @Override
    public int hashCode()
    {
      return hashCode;
    }
        
    private TypeSignature(String... types)
    {
      this.types = types;
      this.hashCode = Arrays.hashCode(types);
    }
    
    private final String[] types;
    private final int hashCode;
  }
  
  public MultiMethod(Signature sig)
  {
    this.sig = sig;
  }
  
  @Override // Datum
  public Type type()
  {
    return TypeRegistry.PROCEDURE_TYPE;
  }
  
  @Override // IFun
  public void apply(VirtualMachine vm)
  {
    var key = TypeSignature.fromArgs(vm.args);
    var method = Optional.ofNullable(methodTable.get(key)).orElseThrow(() -> new TypeMismatch());
    method.apply(vm);
  }
  
  @Override // IFun
  public Signature signature()
  {
    return sig;
  }
  
  public void addMethod(List paramTypes, IFun method)
  {
    if (!sig.equals(method.signature()))
      throw new Error("invalid signature in generic function definition");
    var key = TypeSignature.fromTypes(paramTypes);
    if (methodTable.containsKey(key))
      throw new Error("multiple definition of generic function");
    methodTable.put(key, method);
  }
  
  private final Signature sig;
  private final HashMap<TypeSignature, IFun> methodTable = new HashMap<>();
}
