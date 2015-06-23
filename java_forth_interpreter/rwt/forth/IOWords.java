package rwt.forth;

import java.nio.ByteBuffer;

public class IOWords {

	// ( ch -- ) outputs a single char..
	public static void emit(VM vm) {
		byte b = (byte)vm.pop();
		System.out.print((char)b);
	}
	
	// ( addr len -- ) outputs a string
	public static void type(VM vm) {
		long sz = vm.pop();
		long chars = vm.pop();
		ByteBuffer bb = vm.MM.addressToBuffer(chars);
		int st = (int)chars;
		int end = st + (int)sz;
		for(int idx = (int)chars; idx < end; ++idx) {
			System.out.print((char)bb.get(idx));
		}		
	}
	
	// ( num -- ) output a number
	public static void period(VM vm) {
		long num = vm.pop();
		System.out.print(num);		
	}
	
	// ( -- ) output a carriage return
	public static void cr(VM vm) {
		System.out.println("");
	}
	
	// : SPACE BL EMIT ;
	public static void space(VM vm) {
		System.out.print(' ');
	}
	
	// : SPACES 0 DO SPACE LOOP ;
    public static void spaces(VM vm) {
    	int amt = (int)vm.pop();
    	for(int i = 0; i < amt; ++i ) {
    		System.out.print(' '); // faster to build a string and print at once??
    	}
    }
    
    // NUMBER> ( caddr len -- n caddr' len' ) 
    public static void number(VM vm) {
    	int len = (int)vm.pop();
    	long addr = vm.pop();
    	long ans = 0;
    	ByteBuffer bb = vm.MM.addressToBuffer(addr);
    	int pos = (int)addr;
    	byte b = 0;
    	boolean digits = false;
    	boolean positive = true;
    	while(len > 0) {
    		b = bb.get(pos);
            if(!digits && b == '+') { digits = true; }
            else if(!digits && b == '-') { digits = true; positive=false; }
            else if(b >= (byte)'0' && b <= (byte)'9') {
            	ans = ans * 10 + (b - (byte)'0');
            	digits = true;
            } else {
            	break;
            }
            --len; ++pos;
    	}
    	vm.push(positive?ans:-ans);
    	vm.push(addr - (int)addr + pos);
    	vm.push(len);
    }
    
	public static void __init__(VM vm) {
		Class<?> me = IOWords.class;

		vm.DCT.addWord("EMIT", 
				VM_JavaMethodOp.generate(me,"emit",false));
		vm.DCT.addWord("TYPE", 
				VM_JavaMethodOp.generate(me,"type",false));
		vm.DCT.addWord(".", 
				VM_JavaMethodOp.generate(me,"period",false));
		vm.DCT.addWord("CR", 
				VM_JavaMethodOp.generate(me,"cr",false));
		vm.DCT.addWord("SPACE", 
				VM_JavaMethodOp.generate(me,"space",false));
		vm.DCT.addWord("SPACES", 
				VM_JavaMethodOp.generate(me,"spaces",false));
		vm.DCT.addWord("NUMBER>", 
				VM_JavaMethodOp.generate(me,"number",false));
		
	}
	
}
