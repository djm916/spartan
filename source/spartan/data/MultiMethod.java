package spartan.data;

//import spartan.data.IFun;
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

public class MultiMethod implements Datum, IFun
{
  public static class TypeSignature
  {
    public static TypeSignature of(List args)
    {
      var argTypes = new Type[args.length()];
      for (int i = 0; !args.isEmpty(); ++i)
        argTypes[i] = args.car().type();
      return new TypeSignature(argTypes);
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
    
    public TypeSignature(Type... types)
    {
      this.types = types;
      this.hashCode = Arrays.hashCode(types);
    }
    
    private final Type[] types;
    private final int hashCode;
  }
  
  public MultiMethod(Signature sig)
  {
    this.sig = sig;
  }
  
  public MultiMethod(int numArgs, boolean isVariadic)
  {
    this(new Signature(numArgs, isVariadic));
  }
  
  @Override // Datum
  public Type type()
  {
    return TypeRegistry.PROCEDURE_TYPE;
  }
  
  @Override // IFun
  public void apply(VirtualMachine vm)
  {
    var method = Optional.ofNullable(methodTable.get(TypeSignature.of(vm.args)))
                 .or(() -> defaultMethod)
                 .orElseThrow(() -> new TypeMismatch());
    method.apply(vm);
  }
  
  @Override // IFun
  public Signature signature()
  {
    return sig;
  }
  
  public void addMethod(TypeSignature signature, IFun method)
  {
    addMethod(signature.types, method);
  }
  
  public void addMethod(Type[] types, IFun method)
  {
    if (!sig.equals(method.signature()))
      throw new Error("invalid signature in generic function definition");
    var key = new TypeSignature(types);
    if (methodTable.containsKey(key))
      throw new Error("multiple definition of generic function");
    methodTable.put(key, method);
  }
  
  public void addDefault(IFun method)
  {
    if (!sig.equals(method.signature()))
      throw new Error("invalid signature in generic function definition");
    defaultMethod = Optional.of(method);
  }
  
  private final Signature sig;
  private final HashMap<TypeSignature, IFun> methodTable = new HashMap<>();
  private Optional<IFun> defaultMethod = Optional.empty();
}
