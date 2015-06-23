package rwt.forth;

import java.lang.reflect.Method;

// this class covers all opcodes that
// call into static methods on java classes.
// This helps organize the java code and keeps
// the number of classes we load into the VM down.
// There is a small speed penalty for this, but
// it should still be faster than pure forth, so
// it's a tradeoff.  The most basic ops have their
// own class.
public class VM_JavaMethodOp extends OpCode {
	private Method meth;
	
	public VM_JavaMethodOp(Method m, boolean immed) {
		meth = m;
		immediate = immed;
	}
	
	public static VM_JavaMethodOp generate(Class<?> which, 
			                               String name, 
			                               boolean immed) {
		try {
		   Method m = which.getMethod(name, VM.class);
		   return new VM_JavaMethodOp(m,immed);
		} catch(Exception e){
			e.printStackTrace(); System.exit(1);
		}
		return null;
		
	}
	
	@Override
	public void run(VM vm) {
		// TODO Auto-generated method stub
		try {
			meth.invoke(null, vm);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

}
