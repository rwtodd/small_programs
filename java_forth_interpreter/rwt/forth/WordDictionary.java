package rwt.forth;

import java.util.HashMap;

public class WordDictionary {
	private HashMap<ByteString,OpCode> dict;
	
	public WordDictionary() {
		dict = new HashMap<ByteString,OpCode>();
	}
	
	   // DICTIONARY STUFF -------------------------------
	   public void addWord(String name, OpCode vt) {
		   addWord(new ByteString(name.getBytes(VM.cset)),vt);
	   }

	   public void addWord(ByteString name, OpCode vt) {
		   dict.put(name, vt);
	   }
	   
	   private OpCode tryToIdentifyNumber(String strword) {
		   OpCode op = null;
		   if(strword.matches("^-?\\d+$")) {
			   op = new VM_Integer_OP(Long.parseLong(strword));
		   } else {
			   // FIXME complain through the VM...
			   System.out.println("Unknown word: <" + strword + ">");
		   }		 
		   return op;
	   }
	   
	   public OpCode lookupWord(String name) {
		   OpCode op = dict.get(new ByteString(name.getBytes(VM.cset)));
			if(op == null) {
				  op = tryToIdentifyNumber(name);
			} 
			return op;
	   }
	   
	   public OpCode lookupWord(ByteString name) {
		   OpCode op =  dict.get(name);
		   if(op == null) {
			      op = tryToIdentifyNumber(name.toString());
		   }
		   return op;
	   }
	   
	   public OpCode lookupWord(BufferString name) {
		   OpCode op = dict.get(name);
		   if(op == null) {
			      op = tryToIdentifyNumber(name.toString());
		   }
		   return op;
	   }
	   
}
