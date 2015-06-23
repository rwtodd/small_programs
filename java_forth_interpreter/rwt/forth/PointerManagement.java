package rwt.forth;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ArrayList;

public class PointerManagement {
	private ArrayList<Object> map;
	private int segmentSize; // default allocation size...
	
	public PointerManagement(int segSz) {
		map = new ArrayList<Object>();
		map.add(null); // the 0th entry will ALWAYS be null...
		segmentSize = segSz;
	}
	
	// put an object in the map,
	// return its index...
	public int add(Object o) {
		for(int i = map.size() -1; i > 0; --i) {
			if(map.get(i) == null) {
				map.set(i, o);
				return i; // found a null spot!
			}
		}
		
		// no null spots... just add to the end...
		map.add(o); 
		return map.size()-1;
	}
	
	public int createBuffer(int sz) {
		if(sz < 0) sz = segmentSize; 
		ByteBuffer bb = ByteBuffer.allocate(sz);
		bb.order(ByteOrder.nativeOrder());
		bb.putInt(0); // the next chained segment.
		return add(bb); // add it in and return it...
	}
	
	// delete all chained buffers
	public void deleteBuffer(long addr) {
		int nxt = (int)(addr >> 32);
		while(nxt != 0) {
			ByteBuffer bb = (ByteBuffer)map.get(nxt);			
			map.set(nxt, null);
			nxt = bb.getInt(0);
		}
	}
	
	public long allocFromBuffer(long base, int sz, boolean reserve) {
		ByteBuffer bb = addressToBuffer(base);
		while( (bb.limit() - bb.position()) < sz ) {
			int nxt = bb.getInt(0);
			if(nxt == 0) {
				nxt = createBuffer(Math.max(sz+8,bb.capacity()));
				bb.putInt(0,nxt); // add it to the chain...
			}
			bb = (ByteBuffer)map.get(nxt);
			base = ((long)nxt) << 32;
		}
		long ans = base - ((int)base) + bb.position();
		if(reserve) bb.position(bb.position() + sz);
		return ans;
	}
	
	   public OpCode addressToOpCode(long addr) {
		   Object ans = map.get((int)(addr >> 32));
		   if(ans instanceof OpCode) {
			   return (OpCode)ans;
		   }
		   return null;
	   }
	   
	   public ByteBuffer addressToBuffer(long addr) {
		   Object ans = map.get((int)(addr >> 32));
		   if(ans instanceof ByteBuffer) {
			   return (ByteBuffer)ans;
		   }
		   return null;
	   }
	   
	   // address points to a counted string...
	   public BufferString addressToBufferString(long addr) {
		   ByteBuffer bb = addressToBuffer(addr);
		   if(bb == null) return null;
		   int len = (int)bb.getShort((int)addr);
		   int start = ((int)addr) + 2;
		   return new BufferString( addr - ((int)addr), bb, start, len );
	   }
	   
		public BufferString addressCntToBufferString(long addr, int len) {
			   ByteBuffer bb = addressToBuffer(addr);
			   if(bb == null) return null;
			   int start = ((int)addr);
			   return new BufferString( addr - ((int)addr), bb, start, len );
		}
		
	   // counted string to java string
	   public String cstrToString(long addr) {
		   Object loc = map.get((int)(addr >> 32));
		   if(loc instanceof ByteBuffer) {
			   ByteBuffer bb = (ByteBuffer)loc;
			   int offs = (int)addr;
			   int size = bb.getShort(offs);
			   offs += 2;
			   return new String(bb.array(),offs,size,VM.cset);
		   }
		   return null;	  
	   }


	
}
