package rwt.forth;

class VM_Swap_OP extends OpCode {
	@Override
	public void run(VM vm) {
		long tmp = vm.peek(0);
		long tmp2 = vm.peek(1);
		vm.stackSet(1,tmp);
		vm.stackSet(0,tmp2);
	}
}

class VM_Rot_OP extends OpCode {
	@Override
	public void run(VM vm) {
		// a b c -> b c a
		long tmpc = vm.peek(0);
		long tmpb = vm.peek(1);
		long tmpa = vm.peek(2);
		vm.stackSet(0,tmpa);
		vm.stackSet(1,tmpc);
		vm.stackSet(2,tmpb);
	}
}

class VM_Pick_OP extends OpCode {

	@Override
	public void run(VM vm) {
		int idx = (int)vm.pop();
		vm.push(vm.peek(idx));
	}
}

class VM_Dup_OP extends OpCode {
	@Override
	public void run(VM vm) {
		vm.push(vm.peek(0));
	}
}

class VM_Drop_OP extends OpCode {
	@Override
	public void run(VM vm) {
		vm.pop();
	}
}

// -------------------------------------------
// JavaMethod stack words....
// -------------------------------------------
public class StackWords {
	public static void rsPush(VM vm) {
		vm.RSPush(vm.pop());
	}
	
	// 2>R ( n1 n2 -- R: n1 n2 )
	public static void rsTwoPush(VM vm) {
		long n2 = vm.pop();
		long n1 = vm.pop();
		vm.RSPush(n1); vm.RSPush(n2);
	}	
	
	public static void rsPop(VM vm) {
		vm.push(vm.RSPop());
	}
	
	// 2R> ( R: n1 n2 -- n1 n2 )
	public static void rsTwoPop(VM vm) {
		long n2 = vm.RSPop();
		long n1 = vm.RSPop();
		vm.push(n1); vm.push(n2);
	}
	
	
	public static void rsAt(VM vm) {
		vm.push(vm.RSPeek(0));
	}
	
	// ( R: n1 n2 -- n1 n2 R: n1 n2 )
	public static void rsTwoAt(VM vm) {
		vm.push(vm.RSPeek(1));
		vm.push(vm.RSPeek(0));
	}
	
	public static void rsDrop(VM vm) {
		vm.RSPop();
	}

	// ( R: n1 n2 -- R: )
	public static void rsTwoDrop(VM vm) {
		vm.RSPop(); vm.RSPop();
	}
	
	public static void negRot(VM vm) {
		// a b c -> c a b
		// : -ROT   ROT ROT
		long tmpc = vm.peek(0);
		long tmpb = vm.peek(1);
		long tmpa = vm.peek(2);
		vm.stackSet(0,tmpb);
		vm.stackSet(1,tmpa);
		vm.stackSet(2,tmpc);
	}
	
	public static void over(VM vm) {
		// a b -> a b a
		// : OVER   1 PICK
		vm.push(vm.peek(1));
	}
	
	public static void nip(VM vm) {
		// a b c -> a c
		// : NIP    SWAP DROP ;
		long toPush = vm.pop();
		vm.stackSet(0,toPush);
	}
	
	public static void questionDup(VM vm) {
		// : ?DUP   DUP   IF DUP THEN ;
		long peek = vm.peek(0);
		if(peek != 0) {
			vm.push(peek);
		}
	}
	
	public static void twoDrop(VM vm) {
		// : 2DROP  DROP DROP ;
		vm.pop();
		vm.pop();		
	}
	
	// : TUCK SWAP OVER ;        \ a b    ->  b a b
	public static void tuck(VM vm) {
		long b = vm.peek(0);
		long a = vm.peek(1);
		vm.stackSet(0, a);
		vm.stackSet(1, b);
		vm.push(b);
	}
	
	// : 2DUP OVER OVER ;        \ a b    ->  a b a b
	public static void twoDup(VM vm) {
		long b = vm.peek(0);
		long a = vm.peek(1);
		vm.push(a);
		vm.push(b);
	}
	
	// ( xn ... x0 n | xn-1 ... x0 xn ) 
	public static void roll(VM vm) {
		int which = (int)vm.pop();
		long xn = vm.peek(which);
		for(int i = which; i > 0; --i) {
			vm.stackSet(i, vm.peek(i-1));
		}
		vm.stackSet(0,xn);
	}
	
	// add the words above to the VM...
	public static void __init__(VM vm) {
		vm.DCT.addWord("DROP", new VM_Drop_OP());
		vm.DCT.addWord("DUP", new VM_Dup_OP());
		vm.DCT.addWord("SWAP", new VM_Swap_OP());
		vm.DCT.addWord("ROT",  new VM_Rot_OP());
		vm.DCT.addWord("PICK", new VM_Pick_OP());
				
		Class<?> me = StackWords.class;
		
		vm.DCT.addWord(">R", 
				VM_JavaMethodOp.generate(me,"rsPush",false));
		vm.DCT.addWord("R>", 
				VM_JavaMethodOp.generate(me,"rsPop",false));
		vm.DCT.addWord("R@", 
				VM_JavaMethodOp.generate(me,"rsAt",false));
		vm.DCT.addWord("R>DROP", 
				VM_JavaMethodOp.generate(me,"rsDrop",false));			
		vm.DCT.addWord("2>R", 
				VM_JavaMethodOp.generate(me,"rsTwoPush",false));
		vm.DCT.addWord("2R>", 
				VM_JavaMethodOp.generate(me,"rsTwoPop",false));
		vm.DCT.addWord("2R@", 
				VM_JavaMethodOp.generate(me,"rsTwoAt",false));
		vm.DCT.addWord("2R>DROP", 
				VM_JavaMethodOp.generate(me,"rsTwoDrop",false));			
		vm.DCT.addWord("-ROT", 
				VM_JavaMethodOp.generate(me,"negRot",false));
		vm.DCT.addWord("OVER",
				VM_JavaMethodOp.generate(me,"over",false));
		vm.DCT.addWord("TUCK", 
				VM_JavaMethodOp.generate(me,"tuck",false));
		vm.DCT.addWord("NIP", 
				VM_JavaMethodOp.generate(me,"nip",false));
		vm.DCT.addWord("?DUP", 
				VM_JavaMethodOp.generate(me,"questionDup",false));
		vm.DCT.addWord("2DROP", 
				VM_JavaMethodOp.generate(me,"twoDrop",false));
		vm.DCT.addWord("2DUP", 
				VM_JavaMethodOp.generate(me,"twoDup",false));
		vm.DCT.addWord("ROLL", 
				VM_JavaMethodOp.generate(me,"roll",false));
		
	}
}
