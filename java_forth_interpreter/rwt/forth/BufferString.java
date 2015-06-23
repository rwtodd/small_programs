package rwt.forth;

import java.nio.ByteBuffer;

// a string from a forth buffer...
public class BufferString {
    public long baseAddr;
    public ByteBuffer buffer;
    public int startPosition;
    public int length;
    private int hash;
    
    public BufferString(long bAddr,ByteBuffer bb, int st, int len) {
    	baseAddr = bAddr;
    	buffer = bb;
    	startPosition = st;
    	length = len;
    	hash = 0;
    }
    
    @Override
    public int hashCode() {
       if(hash == 0) {
    	   int h = 0;
    	   byte b = 0;
    	   int endp = startPosition + length;
    	   for(int i = startPosition; i < endp; ++i ) {
   			   b = buffer.get(i);
    		   if((b >= 97) && (b <= 122)) b -= 32; // toupper
    		   h = (h*31) + b;
    	   }
    	   hash = h;
       }
       
       return hash;
    }
    
    @Override
    public boolean equals(Object o) {
    	
    	if(o instanceof ByteString) {
    		byte[] bs = ((ByteString)o).getBytes();
    		if(length != bs.length) return false;

    		int i = startPosition;
    		byte b = 0;
    		for(int delta = 0; delta < length; ++delta) {
    			b = buffer.get(i+delta);
    			if((b >= 97) && (b <= 122)) b -= 32; // toupper
    			if(b != bs[delta]) 
    				return false;
    		}
    		return true;
    		
    	} else if(o instanceof BufferString) {
    		// buffer string compare...
    		BufferString bs = (BufferString)o;
    		if(length != bs.length) return false;
    		int i = startPosition;
    		int j = bs.startPosition;
    		for(int delta = 0; delta < length; ++delta) {
    			if(buffer.get(i+delta) != bs.buffer.get(j+delta)) 
    				return false;
    		}
    		return true;
    	}
    	
    	return false;
    }
    
    public ByteString toByteString() {
    	int oldpos = buffer.position();
    	buffer.position(startPosition);
    	byte[] barr = new byte[length];
    	buffer.get(barr, 0, length);
    	buffer.position(oldpos); // restore the position it was in...
    	return new ByteString(barr);
    }
    
    @Override
    public String toString() {
    	int oldpos = buffer.position();
    	buffer.position(startPosition);
    	byte[] barr = new byte[length];
    	buffer.get(barr, 0, length);
    	buffer.position(oldpos); // restore the position it was in...
    	return new String(barr);
    }
    
}
