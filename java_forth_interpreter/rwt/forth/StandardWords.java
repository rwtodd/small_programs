package rwt.forth;

import java.nio.ByteBuffer;

//  size  buffer  -- address
class VM_Buf_Alloc extends OpCode {
	private boolean reserveSpace;
	
	public VM_Buf_Alloc(boolean reserve) {
		reserveSpace = reserve;
	}
	
	@Override
	public void run(VM vm) {
		long addr = vm.pop();
		int size = (int)vm.pop();
		vm.push(vm.MM.allocFromBuffer(addr,size,reserveSpace));
	}
}

// ( addr -- )
class VM_Buf_Delete extends OpCode {
	@Override
	public void run(VM vm) {
		vm.MM.deleteBuffer(vm.pop());
	}
}

// ( size -- addr )
class VM_Buf_Create extends OpCode {
	@Override
	public void run(VM vm) {
		int sz = (int)vm.pop();
		int newbuf = vm.MM.createBuffer(sz);
		vm.push( ((long)newbuf) << 32 );
	}
}

// 1 BL FILE-SKIP   .. skip blanks from fileno 1
class VM_Skip_OP extends OpCode {
	@Override
	public void run(VM vm) {
		byte delim = (byte)vm.pop();
		int fileno = (int)vm.pop();
		vm.CREAD.skip(fileno,delim);
	}
}

// 1 FILE-CHAR  .... read a character from fieno 1
class VM_Char_OP extends OpCode {
	@Override
	public void run(VM vm) {
		int fileno = (int)vm.pop();
		vm.push((long)(vm.CREAD.readRawChar(fileno)));		
	}
}


//   1 BL FILE-WORD  to get a word from buffer 1
class VM_NextWord_OP extends OpCode {
	@Override
	public void run(VM vm) {
		byte delim = (byte)vm.pop();
		int fileno = (int)vm.pop();
		BufferString bs = vm.CREAD.readWord(fileno, delim);
		// FIXME handle null return!
		vm.push(bs.baseAddr + bs.startPosition); // address
		vm.push(bs.length);
	}
}

// 1 CHAR e FILE-PARSE ... parse until 'e' in file 1
class VM_Parse_OP extends OpCode {
	@Override
	public void run(VM vm) {
		byte delim = (byte)vm.pop();
		int fileno = (int)vm.pop();
		BufferString bs = vm.CREAD.parse(fileno,delim);
		// FIXME handle null return
		vm.push(bs.baseAddr + bs.startPosition); // address
		vm.push(bs.length);
	}
}

class VM_CustomWord extends OpCode {
	public java.util.ArrayList<OpCode> words;
	
	public VM_CustomWord() {
		words = new java.util.ArrayList<OpCode>();
	}
	
	@Override
	public void run(VM vm) {
		vm.runCustomOP(this);
	}
}

class VM_TailCall_OP extends OpCode {
	@Override
	public void run(VM vm) {
		vm.tailCall(vm.peekAtPC());
	}
}

class VM_Exit_OP extends OpCode {
	@Override
	public void run(VM vm) {
		vm.tailCall(null);
	}
}

class VM_Execute_OP extends OpCode {

	@Override
	public void run(VM vm) {
		if(vm.runningInVM()) {
  		   vm.executeNext(vm.MM.addressToOpCode(vm.pop()));
		} else {
			// we aren't running in the VM at the moment,
			// so start...
			OpCode op = vm.MM.addressToOpCode(vm.pop());
			op.run(vm);
		}
	}
	
}

// in customOP word-stream, __COMPNXT__ op 
// compiles 'op' into the current DEF...
class VM_CompileNext_OP extends OpCode {
	@Override
	public void run(VM vm) {
		OpCode op = vm.peekAtPC();
		vm.CDEF.words.add(op); // compile in the designated op
		vm.relativeJump(1);    // jump over the OP
	}	
}


//---------------------------------------------------------------
// Control flow
//---------------------------------------------------------------

//the "jump on zero" op helps implement an IF statement,
//since IF considers 0 to be "false"
//flag JZ -> jump by jmp-dist if flag is 0
// jmp-dist is the number AFTER __JZ__ in the word list!!!
//remember that when this runs, PC will already point to
//the jmp-dist, so the distance is relative to that point.
class VM_JZ_OP extends OpCode {
	@Override
	public void run(VM vm) {
		long bool = vm.pop();
		if(bool == 0) {
			long dist = ((VM_Integer_OP)vm.peekAtPC()).getInteger();
			vm.relativeJump((int)dist);			
		} else {
			vm.relativeJump(1); // skip the distance...
		}
	}	
}

//the "jump" op helps implement an IF statement,
//remember that when this runs, PC will already point to
//the jump distance, so the move is relative to that point.
class VM_JMP_OP extends OpCode {
	@Override
	public void run(VM vm) {
		long dist = ((VM_Integer_OP)vm.peekAtPC()).getInteger();
		vm.relativeJump((int)dist);			
	}	
}

// FILE$ ( address count -- fileno )  
class VM_StringFile_OP extends OpCode {

	@Override
	public void run(VM vm) {
		long cnt = vm.pop();
		long addr = vm.pop();
		BufferString bs = vm.MM.addressCntToBufferString(addr,(int)cnt);
		if(bs == null) {
			System.out.println("MEM-FILE: BAD STRING!");
			vm.push(0); // stdin...
		}
		long ans = vm.CREAD.newSource(bs);
		vm.push(ans);
	}
}

// EVALUATE-FILE
class VM_EVAL_FILE_OP extends OpCode {	
	@Override
	public void run(VM vm) {
		long fileno =  vm.pop();
		
		// get the vm/reader state...
		FReader.State st = vm.CREAD.saveState();
		boolean compmode = vm.compilationMode;
		VM_CustomWord cdef = vm.CDEF;
		
        vm.CREAD.setDefaultSource((int)fileno, false);
        
        CompilationWords.interpretationMode(vm);
        
        // restore vm/reader state...
        vm.CDEF = cdef;
        vm.compilationMode = compmode;
        vm.CREAD.restoreState(st);
	}
}


// src dest len -- ()
class VM_Copy_OP extends OpCode {
	@Override
	public void run(VM vm) {
		int len = (int)vm.pop();
		long dest_addr = vm.pop();
		long src_addr = vm.pop();
		ByteBuffer src = vm.MM.addressToBuffer(src_addr);
		ByteBuffer dest = vm.MM.addressToBuffer(dest_addr);
		int src_offset = (int)src_addr;
		int dest_offset = (int)dest_addr;
		for(int i = 0; i < len; ++i) {
			dest.put(dest_offset++,src.get(src_offset++));
		}
		vm.push(dest_addr); // leave the destination on the stack
	}
}

public class StandardWords {

	public static void installStandardWords(VM vm) {
		MathLogicWords.__init__(vm);
				
		// lowest level ops...
		vm.DCT.addWord("__COMPNXT__", new VM_CompileNext_OP());
		vm.DCT.addWord("__TAIL__", new VM_TailCall_OP());
		vm.DCT.addWord("__JZ__", new VM_JZ_OP());
		vm.DCT.addWord("__JMP__", new VM_JMP_OP());

		StackWords.__init__(vm);
		MemoryWords.__init__(vm);
		CompilationWords.__init__(vm);
		
		vm.DCT.addWord("FILE-CHAR", new VM_Char_OP());
		vm.DCT.addWord("FILE-SKIP", new VM_Skip_OP());
				
		vm.DCT.addWord("EXECUTE", new VM_Execute_OP());
		vm.DCT.addWord("FILE-WORD", new VM_NextWord_OP());
		vm.DCT.addWord("FILE-PARSE", new VM_Parse_OP());
		vm.DCT.addWord("EXIT", new VM_Exit_OP());
		vm.DCT.addWord("FILE-EVALUATE", new VM_EVAL_FILE_OP());
		vm.DCT.addWord("MEM-FILE", new VM_StringFile_OP());
				
		vm.DCT.addWord("BUF-ALLOC", new VM_Buf_Alloc(true));
		vm.DCT.addWord("BUF-BORROW", new VM_Buf_Alloc(false));
		vm.DCT.addWord("BUF-CREATE", new VM_Buf_Create());
		vm.DCT.addWord("BUF-DELETE", new VM_Buf_Delete());
		vm.DCT.addWord("COPY", new VM_Copy_OP());
		
		IOWords.__init__(vm);
	}
	
}
