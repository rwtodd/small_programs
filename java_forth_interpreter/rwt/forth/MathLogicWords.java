package rwt.forth;

//-----------------------------------------------------
//INTEGERS -------------------------------------------
//-----------------------------------------------------
class VM_Integer_OP extends OpCode {
 private long myValue;
 
 public VM_Integer_OP(long value) {
	   myValue = value;
 }

 public long getInteger() {
	   return myValue;
 }
 
 @Override 
 public void run(VM vm) {
	   vm.push(myValue);
 }
 
}

class VM_Integer_Plus extends OpCode {
	@Override
	public void run(VM vm) {
		vm.push(vm.pop() + vm.pop());
	}	
}

class VM_Integer_Minus extends OpCode {
	@Override
	public void run(VM vm) {
		long v2 = vm.pop();
		long v1 = vm.pop();
		vm.push(v1 - v2);
	}	
}

class VM_Integer_Times extends OpCode {
	@Override
	public void run(VM vm) {
		vm.push(vm.pop()*vm.pop());
	}		
}
class VM_Integer_Divides extends OpCode {
	@Override
	public void run(VM vm) {
		long v2 = vm.pop();
		long v1 = vm.pop();
		vm.push(v1/v2);
	}		
}

class VM_Integer_EqualTo extends OpCode {
	@Override
	public void run(VM vm) {
		long v2 = vm.pop();
		long v1 = vm.pop();
		vm.push(v1 == v2 ? -1L : 0L);
	}			
}

// ---------------------------------
// Math and Logic JavaMethod ops...
// ---------------------------------
public class MathLogicWords {

	// logical and
	public static void and(VM vm) {
		vm.push(vm.pop() & vm.pop());
	}

	public static void or(VM vm) {
		vm.push(vm.pop() | vm.pop());
	}
	
	// NOT gives -1 for 0 and 0 for anything else...
	public static void not(VM vm) {
		vm.push(vm.pop()==0L?-1L:0L);
	}
	
	public static void xor(VM vm) {
		vm.push(vm.pop() ^ vm.pop());
	}
	
	public static void invert(VM vm) {
		vm.push(~vm.pop());
	}
	
	public static void negate(VM vm) {
		vm.push(-vm.pop());
	}
	
	public static void lessThan(VM vm) {
		long b = vm.pop();
		long a = vm.pop();
		vm.push(a<b?-1L:0L);
	}
	
	public static void grThan(VM vm) {
		long b = vm.pop();
		long a = vm.pop();
		vm.push(a>b?-1L:0L);
	}
	
	public static void lessEq(VM vm) {
		long b = vm.pop();
		long a = vm.pop();
		vm.push(a<=b?-1L:0L);
	}
	
	public static void grEq(VM vm) {
		long b = vm.pop();
		long a = vm.pop();
		vm.push(a>=b?-1L:0L);
	}
	
	public static void notEq(VM vm) {
		long b = vm.pop();
		long a = vm.pop();
		vm.push(a==b?0L:-1L);
	}

	// : max  2dup > if nip else drop then ;
	public static void max(VM vm) {
		long b = vm.pop();
		long a = vm.pop();
		vm.push(a>b?a:b);
	}
	
	// : min 2dup < if drop else nip then ; 
	public static void min(VM vm) {
		long b = vm.pop();
		long a = vm.pop();
		vm.push(a<b?a:b);
	}
	
	// (n amt LSHIFT -- n<<amt) 
	public static void lshift(VM vm) {
		long amt = vm.pop();
		if(amt > 0) vm.push(vm.pop() << amt);
	}
	
	// ( n amt RSHIFT -- n>>amt )
	public static void rshift(VM vm) {
		long amt = vm.pop();
		if(amt > 0) vm.push(vm.pop() >> amt);
	}
	
	// ( x low high -- t|f )  low <= x <= high
	public static void within(VM vm) {
	   long high = vm.pop();
	   long low = vm.pop();
	   long n = vm.pop();
	   vm.push((low <= n && n <= high)?-1L:0L);
	}
	
	public static void __init__(VM vm) {
		vm.DCT.addWord("-10", new VM_Integer_OP(-10L));
		vm.DCT.addWord("-9", new VM_Integer_OP(-9L));
		vm.DCT.addWord("-8", new VM_Integer_OP(-8L));
		vm.DCT.addWord("-7", new VM_Integer_OP(-7L));
		vm.DCT.addWord("-6", new VM_Integer_OP(-6L));
		vm.DCT.addWord("-5", new VM_Integer_OP(-5L));
		vm.DCT.addWord("-4", new VM_Integer_OP(-4L));
		vm.DCT.addWord("-3", new VM_Integer_OP(-3L));
		vm.DCT.addWord("-2", new VM_Integer_OP(-2L));
		OpCode negOne = new VM_Integer_OP(-1L);
		OpCode zero =   new VM_Integer_OP(0L);
		vm.DCT.addWord("-1", negOne);
		vm.DCT.addWord("0", zero);
		vm.DCT.addWord("TRUE", negOne);
		vm.DCT.addWord("FALSE", zero);
		vm.DCT.addWord("1", new VM_Integer_OP(1));
		vm.DCT.addWord("2", new VM_Integer_OP(2));
		vm.DCT.addWord("3", new VM_Integer_OP(3));
		vm.DCT.addWord("4", new VM_Integer_OP(4));
		vm.DCT.addWord("5", new VM_Integer_OP(5));
		vm.DCT.addWord("6", new VM_Integer_OP(6));
		vm.DCT.addWord("7", new VM_Integer_OP(7));
		vm.DCT.addWord("8", new VM_Integer_OP(8));
		vm.DCT.addWord("9", new VM_Integer_OP(9));
		vm.DCT.addWord("10", new VM_Integer_OP(10));		
	
		vm.DCT.addWord("+", new VM_Integer_Plus());
		vm.DCT.addWord("*", new VM_Integer_Times());
		vm.DCT.addWord("/", new VM_Integer_Divides());
		vm.DCT.addWord("-", new VM_Integer_Minus());
		vm.DCT.addWord("=", new VM_Integer_EqualTo());
		
		Class<?> me = MathLogicWords.class;

		vm.DCT.addWord("AND", 
				VM_JavaMethodOp.generate(me,"and",false));
		vm.DCT.addWord("OR", 
				VM_JavaMethodOp.generate(me,"or",false));
		vm.DCT.addWord("XOR", 
				VM_JavaMethodOp.generate(me,"xor",false));
		vm.DCT.addWord("NOT", 
				VM_JavaMethodOp.generate(me,"not",false));
		vm.DCT.addWord("INVERT", 
				VM_JavaMethodOp.generate(me,"invert",false));
		vm.DCT.addWord("NEGATE", 
				VM_JavaMethodOp.generate(me,"negate",false));

		vm.DCT.addWord("<", 
				VM_JavaMethodOp.generate(me,"lessThan",false));
		vm.DCT.addWord(">", 
				VM_JavaMethodOp.generate(me,"grThan",false));
		vm.DCT.addWord("<=", 
				VM_JavaMethodOp.generate(me,"lessEq",false));
		vm.DCT.addWord(">=", 
				VM_JavaMethodOp.generate(me,"grEq",false));
		vm.DCT.addWord("<>", 
				VM_JavaMethodOp.generate(me,"notEq",false));

		vm.DCT.addWord("MAX", 
				VM_JavaMethodOp.generate(me,"max",false));
		vm.DCT.addWord("MIN", 
				VM_JavaMethodOp.generate(me,"min",false));
		vm.DCT.addWord("WITHIN",
				VM_JavaMethodOp.generate(me, "within", false));
		
		vm.DCT.addWord("LSHIFT",
				VM_JavaMethodOp.generate(me, "lshift", false));
		vm.DCT.addWord("RSHIFT",
				VM_JavaMethodOp.generate(me, "rshift", false));
	}
}
