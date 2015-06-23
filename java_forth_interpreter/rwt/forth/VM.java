package rwt.forth;

import java.util.ArrayList;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.ReadableByteChannel;
import java.nio.charset.Charset;

/**
 * This is the main class for the VM. It contains the
 * apparatus for running forth code.
 * @author Richard
 *
 */
public class VM {
	
   public static Charset cset = Charset.forName("US-ASCII");
	
   // used for program counter and RS stack
   private class Location {
	  private int index;
	  private VM_CustomWord opcode;
	  public Location(VM_CustomWord op, int idx) {
		  opcode = op;
		  index = idx;
	  }
	  public void next() {
		  ++index;
	  }
	  
	  public void next(int dist) {
		index += dist;  
	  }
	 	  
	  public void end() { // jump to the end of the list...
		  index = opcode.words.size();
	  }
	  
	  public OpCode getNextOp() {
		  if(index < opcode.words.size())
  		     return opcode.words.get(index);
		  return null; // past the end...
	  }
   }
   
   private ArrayList<Object> RS; // return stack
   private Location PC; // Program Counter

   // various buffers...
   private long[] DS; // data stack
   private int DSTop; // top of stack index
   
   private ByteBuffer PAD; // scratch area
   
   public WordDictionary DCT; // Dictionary
   public PointerManagement MM; // Memory Map
   
   public VM_CustomWord CDEF; // current definition
   public FReader CREAD;
   
   public boolean compilationMode; // are we in compilation mode?
   
   public VM(int stackSize, int padSize, int tibSize, int strSize, 
		    ReadableByteChannel stdin) {

	   DS = new long[stackSize];
	   DSTop = -1;
	   
	   PAD = ByteBuffer.allocate(padSize);
	   PAD.order(ByteOrder.nativeOrder());
	   
	   DCT = new WordDictionary();
	   MM = new PointerManagement(strSize);
	   //MM.add(DS);         // index 1
	   long padBuffer = ((long)MM.add(PAD)) << 32;
	   long dictBuffer = ((long)MM.createBuffer(-1)) << 32;
	   DCT.addWord("PAD", new VM_Integer_OP(padBuffer));
	   DCT.addWord("DICT", new VM_Integer_OP(dictBuffer));
	   CREAD = new FReader(tibSize,this,stdin);
	   CDEF = null;
	   RS = null;
	   PC = null;
	   
	   compilationMode = false;
   }

   // DATA STACK STUFF -------------------------------
   public void push(long val) {
	   DS[++DSTop] = val;
   }
   
   public long pop() {
	   return DS[DSTop--];
   }
   
   public long peek(int idx) {
	   return DS[DSTop - idx];
   }
   
   public void stackSet(int idx, long val) {
	   DS[DSTop - idx] = val;
   }

   // floating point pushes and pops
   public void pushFloat(double val) {
	   DS[++DSTop] = Double.doubleToRawLongBits(val);
   }
   
   public double popFloat() {
	   return Double.longBitsToDouble(DS[DSTop--]);
   }
   
   // -------------------------------------------------------
   // Return stack ops...
   // -------------------------------------------------------
   public void RSPush(long val) {
	   RS.add(new Long(val));
   }

   public long RSPop() {
	   Long val = (Long)RS.remove(RS.size()-1);
	   return val;
   }
   
   public long RSPeek(int idx) {
	   Long val = (Long)RS.get(RS.size() - 1 - idx);
	   return val;
   }
   
   // ----------------------------------------------------------
   // CURRENT DEFINITION STUFF
   // ----------------------------------------------------------
   public void newCustomWord(BufferString name, VM_CustomWord vt) {
	   CDEF = vt;
	   DCT.addWord(name.toByteString(), vt);
   }
 
    
   // PROCESSING STUFF -------------------------------   
   public void executeNext(OpCode op) {
	   if(op instanceof VM_CustomWord) {
		   RS.add(PC);
		   PC = new Location((VM_CustomWord)op,0);
	   } else {
		   op.run(this);
	   }	   
   }
   
   public void tailCall(OpCode op) {
	  if(op instanceof VM_CustomWord) {
		  PC = new Location((VM_CustomWord)op,0);
	  } else {
		  // run the op if we have one, but then jump to the end of
		  // the current PC so that the RS will get popped
		  // next...
		  if(op != null) op.run(this);
		  PC.end();
	  }
   }
   
   public void relativeJump(int dist) {
	   PC.next(dist);
   }
   public OpCode peekAtPC() {
	   return PC.getNextOp();
   }
   
   // are we running ops right now?
   public boolean runningInVM() { return PC != null; }
   
   public void runCustomOP(VM_CustomWord op) {
       ArrayList<Object> oldRS = RS; // return stack
       Location oldPC = PC; // program counter
       
       RS = new ArrayList<Object>();
       PC = new Location(op,0);
       
       while(true) {
    	   OpCode nextOp = PC.getNextOp();
    	   if(nextOp == null) {
    	     if(RS.isEmpty()) break;
    	     Object tmp = RS.remove(RS.size()-1);
    	     if(tmp instanceof Location) {
      	       PC = (Location)tmp;
    	     } else {
    	    	 System.out.println("BAD RS!!!");
    	    	 PC = null;
    	     }
    	     continue;
    	   }
    	   PC.next();
    	   
    	   executeNext(nextOp);
       }
       
       //restore old regime...
       RS = oldRS; 
       PC = oldPC;
   }
}
