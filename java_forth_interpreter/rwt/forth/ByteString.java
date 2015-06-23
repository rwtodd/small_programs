package rwt.forth;

// a byte[] string for use in our
// dictionary class...
public class ByteString {
	private byte[] bytes;
    private int hash;
    
    public ByteString(byte[] bts) {
    	bytes = bts;
    	hash = 0;
    	byte b = 0;
    	for(int i = 0; i < bts.length; ++i) {
    	   b = bts[i];
 		   if((b >= 97) && (b <= 122)) bts[i] -= 32; // toupper
    	}
    }
    
    @Override
    public int hashCode() {
       if(hash == 0) {
    	   int h = 0;
    	   int endp = bytes.length;
    	   for(int i = 0; i < endp; ++i ) {
    		   h = (h*31) + bytes[i];
    	   }
    	   hash = h;
       }
       
       return hash;
    }
    
    @Override
    public boolean equals(Object o) {
    	if(o instanceof BufferString) {
    		// buffer string compare...
    		BufferString bs = (BufferString)o;
    		if(bytes.length != bs.length) return false;
    		int j = bs.startPosition;
    		byte b = 00;
    		for(int delta = 0; delta < bytes.length; ++delta) {
    			b = bs.buffer.get(j+delta);
     		    if((b >= 97) && (b <= 122)) b -= 32; // toupper   			
    			if(bytes[delta] != b) 
    				return false;
    		}
    		return true;
    	} else if(o instanceof ByteString) {
    		return java.util.Arrays.equals(bytes,((ByteString)o).bytes);    		
    	} 
    
    	return false;
    }

    @Override
    public String toString() {
    	return new String(bytes,VM.cset);
    }
    
    public byte[] getBytes() { return bytes; }
}
