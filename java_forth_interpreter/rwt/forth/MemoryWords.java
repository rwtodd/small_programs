package rwt.forth;

import java.nio.ByteBuffer;

public class MemoryWords {

	// c@ ( addr -- 8-bit-byte )
	public static void cAt(VM vm) {
		long addr = vm.pop();
		ByteBuffer bb = vm.MM.addressToBuffer(addr);
		vm.push(bb.get((int)addr));		
	}

	// c! ( byte addr -- ) 
	public static void cExclaim(VM vm) {
		long addr  = vm.pop();
		long val = vm.pop();
		ByteBuffer bb = vm.MM.addressToBuffer(addr);
		bb.put((int)addr,(byte)val);		
	}
	
	// w@ ( addr -- 16-bit-word )
	public static void wAt(VM vm) {
		long addr = vm.pop();
		ByteBuffer bb = vm.MM.addressToBuffer(addr);
		vm.push(bb.getShort((int)addr));		
	}

	// w! ( word addr -- ) 
	public static void wExclaim(VM vm) {
		long addr  = vm.pop();
		long val = vm.pop();
		ByteBuffer bb = vm.MM.addressToBuffer(addr);
		bb.putShort((int)addr,(short)val);		
	}
	
	// d@ ( addr -- 32-bit-dword )
	public static void dAt(VM vm) {
		long addr = vm.pop();
		ByteBuffer bb = vm.MM.addressToBuffer(addr);
		vm.push(bb.getInt((int)addr));		
	}

	// d! ( dword addr -- ) 
	public static void dExclaim(VM vm) {
		long addr  = vm.pop();
		long val = vm.pop();
		ByteBuffer bb = vm.MM.addressToBuffer(addr);
		bb.putInt((int)addr,(int)val);		
	}

	// q@ ( addr -- 64-bit-qword )
	public static void qAt(VM vm) {
		long addr = vm.pop();
		ByteBuffer bb = vm.MM.addressToBuffer(addr);
		vm.push(bb.getLong((int)addr));		
	}

	// q! ( qword addr -- ) 
	public static void qExclaim(VM vm) {
		long addr  = vm.pop();
		long val = vm.pop();
		ByteBuffer bb = vm.MM.addressToBuffer(addr);
		bb.putLong((int)addr,val);		
	}
		
	
	// ( n -- sizeof(n cells) )
	public static void cells(VM vm) {
		long num = vm.pop();
		vm.push(num*8); // 8 bytes per 64-bit cell..
	}
	
	public static void chars(VM vm) {
		// do nothing... n chars takes n bytes;
		return;
	}
	
	// -----------------------------------------
	// Incrementing getters and setters ...
	
	// C@++ ( addr -- addr+1 ch )
	public static void cAtPlusPlus(VM vm) {
		long addr = vm.pop();
		vm.push(addr+1);
		ByteBuffer bb = vm.MM.addressToBuffer(addr);
		vm.push(bb.get((int)addr));
	}
	// C!++ (  ch addr -- addr+1 )
	public static void cExclaimPlusPlus(VM vm) {
		long addr = vm.pop();
		byte ch = (byte)vm.pop();
		vm.push(addr+1);
		ByteBuffer bb = vm.MM.addressToBuffer(addr);
		bb.put((int)addr,ch);
	}
	// Q@++ ( addr -- addr+8 n )
	public static void qAtPlusPlus(VM vm) {
		long addr = vm.pop();
		vm.push(addr+8);
		ByteBuffer bb = vm.MM.addressToBuffer(addr);
		vm.push(bb.getLong((int)addr));
	}
	// Q!++ (  n addr -- addr+1 )
	public static void qExclaimPlusPlus(VM vm) {
		long addr = vm.pop();
		long ch = vm.pop();
		vm.push(addr+8);
		ByteBuffer bb = vm.MM.addressToBuffer(addr);
		bb.putLong((int)addr,ch);
	}
	
	public static void __init__(VM vm) {
		Class<?> me = MemoryWords.class;

		vm.DCT.addWord("c@", 
				VM_JavaMethodOp.generate(me,"cAt",false));
		vm.DCT.addWord("c!", 
				VM_JavaMethodOp.generate(me,"cExclaim",false));
		vm.DCT.addWord("c@++", 
				VM_JavaMethodOp.generate(me,"cAtPlusPlus",false));
		vm.DCT.addWord("c!++", 
				VM_JavaMethodOp.generate(me,"cExclaimPlusPlus",false));
		vm.DCT.addWord("w@", 
				VM_JavaMethodOp.generate(me,"wAt",false));
		vm.DCT.addWord("w!", 
				VM_JavaMethodOp.generate(me,"wExclaim",false));
		vm.DCT.addWord("d@", 
				VM_JavaMethodOp.generate(me,"dAt",false));
		vm.DCT.addWord("d!", 
				VM_JavaMethodOp.generate(me,"dExclaim",false));

		VM_JavaMethodOp op = VM_JavaMethodOp.generate(me,"qAt",false);
		vm.DCT.addWord("q@", op);
		vm.DCT.addWord("@", op);
		op = VM_JavaMethodOp.generate(me,"qExclaim",false);
		vm.DCT.addWord("q!", op);
		vm.DCT.addWord("!",op);
		op = VM_JavaMethodOp.generate(me,"qAtPlusPlus",false);
		vm.DCT.addWord("q@++", op);
		vm.DCT.addWord("@++", op);
		op = VM_JavaMethodOp.generate(me,"qExclaimPlusPlus",false);
		vm.DCT.addWord("q!++", op);
		vm.DCT.addWord("!++", op);
		
		vm.DCT.addWord("CHARS", 
				VM_JavaMethodOp.generate(me,"chars",false));
		vm.DCT.addWord("CELLS", 
				VM_JavaMethodOp.generate(me,"cells",false));
	}
	
}
