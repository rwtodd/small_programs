package rwt.forth;

public class CompilationWords {
	// some ops that these words rely on...
	private static OpCode __COMPNXT__ = null;
	private static OpCode swap = null;
	private static OpCode __JMP__ = null;
	private static OpCode __TAIL__ = null;
	
	// ( -- len )
	public static void complen(VM vm) {
		vm.push(vm.CDEF.words.size());
	}
	
	// ( -- addr )
	public static void compaddr(VM vm) {
		vm.push(vm.CDEF.address(vm));
	}
	
	// ( -- )
	public static void makeImmediate(VM vm) {
		vm.CDEF.immediate = true;
	}
	
	// ( addr . "name" -- addr )
	public static void createName(VM vm) {
		BufferString name = vm.CREAD.readWord(-1,(byte)' ');
		vm.DCT.addWord(name.toByteString(), new VM_Integer_OP(vm.peek(0)));
		// leave the address of the created thing on the stack
	}
	
	// ( x -- ) 
	public static void literal(VM vm) {
		long topush = vm.pop();
		OpCode op = null;
		if((topush >= -10) && (topush <= 10)) {
			op = vm.DCT.lookupWord(Long.toString(topush));			
		} else {
			op = new VM_Integer_OP(topush);
		}
		vm.CDEF.words.add(op);		
	}
	
	// ( xt loc -- <xt compiled into CDEF at loc> )
	// ( xt -1  -- <xt compiled to the end of CDEF> )
	public static void __compl__(VM vm) {
		int  loc = (int)vm.pop();
		long toComp = vm.pop();
		OpCode op = null;
		if(toComp > 0) op = vm.MM.addressToOpCode(toComp);
		if(op == null) {
			if((toComp >= -10) && (toComp <= 10)) {
				op = vm.DCT.lookupWord(Long.toString(toComp));
			} else {
				op = new VM_Integer_OP(toComp);
			}
		}
		if(loc < 0) {
			vm.CDEF.words.add(op);
		} else {
			vm.CDEF.words.set(loc, op);
		}		
	}
	
	// : COMPILE, -1 __COMPL__ ;  
	public static void compileComma(VM vm) {
		vm.push(-1);
		__compl__(vm);
	}
	
	// ( xt -- t|f ) 
	public static void isImmediate(VM vm) {
		OpCode op = vm.MM.addressToOpCode(vm.pop());
		// FIXME handle null...
		vm.push(op.immediate?-1L:0L);		
	}
	
	// ( -- ) semicolon
	public static void semicolon(VM vm) {
		// FIXME complain if not already true???
		vm.compilationMode = false;
	}

	// : CONSTANT   CREATE  DROP ;
	public static void constant(VM vm) {
		createName(vm);
		vm.pop();
	}
	
	// ( xt -- <xt referred to is postponed into current definition>
	public static void postponeComma(VM vm) {
		long addr = vm.pop();
		OpCode op = vm.MM.addressToOpCode(addr);
		
		if(op.immediate) {
			// compile it directly in
			vm.CDEF.words.add(op);
		} else {
			// compile^2 -- compile compiling code
			vm.CDEF.words.add(__COMPNXT__);
			vm.CDEF.words.add(op);
		}
	}
	
	// : POSTPONE   ( "word" -- ) ' POSTPONE, ; IMMEDIATE
	public static void postpone(VM vm) {
		BufferString bs = vm.CREAD.readWord(-1, (byte)' ');
		OpCode op = vm.DCT.lookupWord(bs);
		
		if(op.immediate) {
			// compile it directly in
			vm.CDEF.words.add(op);
		} else {
			// compile^2 -- compile compiling code
			vm.CDEF.words.add(__COMPNXT__);
			vm.CDEF.words.add(op);
		}
		
	}

	// The '[' operator, interpretation mode!
	public static void interpretationMode(VM vm) {
		OpCode op = null;

	   // FIXME complain if it wasn't true already??
	   vm.compilationMode = false;
	   
	   while(!vm.compilationMode) {
		   BufferString word = vm.CREAD.readWord(-1,(byte)' ');
		   if(word == null) break;
	   
		   // word is either a number or in the dictionary...?
		   op = vm.DCT.lookupWord(word);
		   op.run(vm);
	   }		   
     }	
	
	// The ']' operator, compilation mode!
	public static void compilationMode(VM vm) {
		   OpCode op = null;
		   if(!vm.compilationMode) {
			   vm.compilationMode = true;
			   return;	   
		   }

		   while(vm.compilationMode) {
			   BufferString word = vm.CREAD.readWord(-1,(byte)' ');
			   if(word == null) break;
			   
			   // word is either a number or in the dictionary...?
			   op = vm.DCT.lookupWord(word);
				   
			   if(op.immediate) {
				   op.run(vm);
				} else {
					vm.CDEF.words.add(op);
				}
		   }
	   }
		
	// :   -- initiates compilation of a new custom word	
	public static void colon(VM vm) {
		BufferString word = vm.CREAD.readWord(-1,(byte)' ');
		vm.newCustomWord(word, new VM_CustomWord());
		//System.out.println("Compiling: " + word.toString()); // TEMP!!!
		if(vm.compilationMode) {
			System.out.println("**** Probably a missing semicolon somewhere before : " + word);
		}
		vm.compilationMode = true;
		compilationMode(vm);		
	}
	
	// : '   BL WORD FIND DROP ; 
	public static void tick(VM vm) {
		BufferString word = vm.CREAD.readWord(-1,(byte)' ');
		OpCode op = vm.DCT.lookupWord(word);
		vm.push(op.address(vm));
	}
	
	// : [']    ' POSTPONE LITERAL ; IMMEDIATE
	public static void bracketTick(VM vm) {
		tick(vm);
		literal(vm);
	}
	
	// ]] is like ] only it also pushes a literal...
	// : ]]   ]  POSTPONE LITERAL ; IMMEDIATE
	public static void closeDoubleBracket(VM vm) {
		compilationMode(vm);
		literal(vm);
	}
	
	// ]]-2 is like ] only it pushes two literals...
	// : ]]-2  ] SWAP POSTPONE LITERAL POSTPONE LITERAL ; IMMEDIATE 
	public static void closeDoubleBracket_2(VM vm) {
		compilationMode(vm);
		swap.run(vm);
		literal(vm);
		literal(vm);
	}
	
//	\ If the tail-call is to the function being defined, just
//	\ jump back to 0. It's faster and makes the VM code simpler.
//	: TAIL-CALL   ' DUP __COMPADDR__ = 
//	              IF
//	                DROP
//	                POSTPONE __JMP__ 
//	                0 __COMPLEN__ -      COMPILE, 
//	              ELSE  
//	              	POSTPONE __TAIL__    COMPILE,
//	              THEN ; IMMEDIATE
	public static void tailCall(VM vm) {
		BufferString bs = vm.CREAD.readWord(-1, (byte)' ');
		OpCode op = vm.DCT.lookupWord(bs);
		if(op == vm.CDEF) {
			// tail-call to self... generate a jump!
			vm.CDEF.words.add(__JMP__);
			long amt = -((long)vm.CDEF.words.size());
			if((amt >= -10) && (amt <= 10)) {
				op = vm.DCT.lookupWord(Long.toString(amt));
			} else {
				op = new VM_Integer_OP(amt);
			}
			vm.CDEF.words.add(op);
		} else {
			// not a tail-call to self...
			vm.CDEF.words.add(__TAIL__);
			vm.CDEF.words.add(op);
		}
	}
	
	// : \ LINE 2DROP ; IMMEDIATE
	public static void backslash(VM vm) {
		vm.CREAD.parse(-1, (byte)'\n');
	}
	
	// : (    [ CHAR ) ] LITERAL PARSE 2DROP ; IMMEDIATE
    public static void openParen(VM vm) {
    	vm.CREAD.parse(-1, (byte)')');
    }
    
    // : \phelp LINE NIP 0 > IF   TAIL-CALL \phelp   THEN ;
    // : \p LINE 2DROP \phelp ; IMMEDIATE
    // ( a comment that lasts until the next blank line, )
    // ( and the first line can be blank... )
    public static void paraComment(VM vm) {
       BufferString bs = null;
 	  bs = vm.CREAD.parse(-1, (byte)'\n');	
      while(true) {
     	  bs = vm.CREAD.parse(-1, (byte)'\n');	
    	  if(bs == null || bs.length == 0) break;
       }
    }
    
    // FIND ( caddr # -- xt t|f )  the flag tells whether the found word
    // was IMMEDIATE or not.
    public static void find(VM vm) {
		int strlen = (int)vm.pop();
		long strid = vm.pop();
		OpCode op = vm.DCT.lookupWord(
				new BufferString(strid - (int)strid,vm.MM.addressToBuffer(strid),(int)strid,strlen));
		if(op != null) {
			vm.push(op.address(vm)); vm.push(op.immediate?-1L:0L);			
		} else {
			System.out.println(new BufferString(strid - (int)strid,vm.MM.addressToBuffer(strid),(int)strid,strlen));
			vm.push(0); vm.push(0);
		}    	
    }
    
	public static void __init__(VM vm) {
		Class<?> me = CompilationWords.class;

		__COMPNXT__ = vm.DCT.lookupWord("__COMPNXT__");
		swap = vm.DCT.lookupWord("SWAP");
		__TAIL__ = vm.DCT.lookupWord("__TAIL__");
		__JMP__ = vm.DCT.lookupWord("__JMP__");
		
		vm.DCT.addWord("__COMPLEN__", 
				VM_JavaMethodOp.generate(me,"complen",false));
		vm.DCT.addWord("__COMPADDR__",
				VM_JavaMethodOp.generate(me,"compaddr",false));
		vm.DCT.addWord("IMMEDIATE",
				VM_JavaMethodOp.generate(me,"makeImmediate",false));
		vm.DCT.addWord("CREATE",
				VM_JavaMethodOp.generate(me,"createName",false));
		vm.DCT.addWord("LITERAL",
				VM_JavaMethodOp.generate(me,"literal",true));
		vm.DCT.addWord("__COMPL__",
				VM_JavaMethodOp.generate(me,"__compl__",false));
		vm.DCT.addWord("COMPILE,",
				VM_JavaMethodOp.generate(me,"compileComma",false));
		vm.DCT.addWord("IMMEDIATE?",
				VM_JavaMethodOp.generate(me,"isImmediate",false));
		vm.DCT.addWord(";",
				VM_JavaMethodOp.generate(me,"semicolon",true));
		vm.DCT.addWord("CONSTANT",
				VM_JavaMethodOp.generate(me,"constant",false));
		vm.DCT.addWord("POSTPONE",
				VM_JavaMethodOp.generate(me,"postpone",true));
		vm.DCT.addWord("POSTPONE,", 
				VM_JavaMethodOp.generate(me,"postponeComma",false));
		vm.DCT.addWord(":", 
				VM_JavaMethodOp.generate(me,"colon",true));
		VM_JavaMethodOp openbrackets = VM_JavaMethodOp.generate(me, "interpretationMode", true);
		vm.DCT.addWord("[", openbrackets);
		vm.DCT.addWord("[[", openbrackets);
		vm.DCT.addWord("]", 
				VM_JavaMethodOp.generate(me,"compilationMode",false));
		vm.DCT.addWord("]]", 
				VM_JavaMethodOp.generate(me,"closeDoubleBracket",false));
		vm.DCT.addWord("]]-2", 
				VM_JavaMethodOp.generate(me,"closeDoubleBracket_2",false));
		vm.DCT.addWord("'", 
				VM_JavaMethodOp.generate(me,"tick",false));
		vm.DCT.addWord("[']", 
				VM_JavaMethodOp.generate(me,"bracketTick",true));
		vm.DCT.addWord("TAIL-CALL", 
				VM_JavaMethodOp.generate(me,"tailCall",true));
		vm.DCT.addWord("\\", 
				VM_JavaMethodOp.generate(me,"backslash",true));
		vm.DCT.addWord("(", 
				VM_JavaMethodOp.generate(me,"openParen",true));
		vm.DCT.addWord("\\p", 
				VM_JavaMethodOp.generate(me,"paraComment",true));
		vm.DCT.addWord("FIND", 
				VM_JavaMethodOp.generate(me,"find",false));
		
    }
	
}
