package rwt.forth;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.ReadableByteChannel;
import java.util.ArrayList;

/**
 * this is the class that knows how to read the
 * FORTH input...
 * @author Richard
 *
 */
public class FReader {
	
	private class Source {
		public ReadableByteChannel channel;
		public ByteBuffer tib;
		public long tibAddr;
		protected boolean eof_flag;
		
		public Source() { eof_flag = false; } // for use by StringSource...
		
		public Source(PointerManagement mm, ReadableByteChannel bc, int bufSz) {
			channel = bc;
			tib = ByteBuffer.allocate(bufSz);
			tib.order(ByteOrder.nativeOrder());
			tibAddr = ((long)mm.add(tib)) << 32;
			tib.limit(0); // force a reload on first read...
			eof_flag = false;
		}
		
		public void closeSource(PointerManagement mm) {
			// FIXME mm.remove(); // get rid of it in the address table!
			try {
				if(channel != null) {
					channel.close();
				}
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		
		private void refill(int preserveFrom) {			
			int restorePos = tib.position() - preserveFrom;
			tib.position(preserveFrom);
			tib.compact();
			
			try{
				channel.read(tib);
			} catch(IOException e) {
				e.printStackTrace();
			}

			tib.flip();
			tib.position(restorePos);
		}		
		
		//returns new pos, or -1 if we are out of chars!
		protected int prepareForRead(int pos) {
			if(!tib.hasRemaining()) {
				refill(pos);
				pos = 0;
		
				if(!tib.hasRemaining()) {
				   eof_flag = true;
			    }
			}
			return pos;
		}		
		
		public int readRawChar() {
			prepareForRead(tib.position());
			if(eof_flag) {
				return -1;
			}
			
			return tib.get();
		}
		
		public BufferString parse(byte delim) {
			if(delim == '\n') {
				return readRawLine(); // handle \n vs. \r\n etc...
			} else if(delim == ' ') {
				return readUntilWhiteSpace();
			}
			
			int restorePos = tib.position();
			byte ch = 0x00;
			while(ch != delim) {
				restorePos = prepareForRead(restorePos);
				if(eof_flag) { break; }
				ch = tib.get(); 
			}
			
			int len = tib.position() - restorePos - (ch==delim?1:0);
			if(len < 0) return null; // no string to read!
			return new BufferString(tibAddr,tib,restorePos,len);		
		}
		
		public BufferString readUntilWhiteSpace() {
			int restorePos = tib.position();
			byte ch = 0x00;
			while(!Character.isWhitespace((char)ch)) {
				restorePos = prepareForRead(restorePos);
				if(eof_flag) { break; }
				ch = tib.get(); 
			}
			
			int len = tib.position() - restorePos - 
					(Character.isWhitespace((char)ch)?1:0);
			if(len < 0) return null; // no string to read!
			if(ch == '\r') restorePos = swallowNewLine(restorePos);
			return new BufferString(tibAddr,tib,restorePos,len);		
		}
		
		public BufferString readRawLine() {
			int restorePos = tib.position();
			byte ch = 0x00;
			while(ch != '\n' && ch != '\r') {
				restorePos = prepareForRead(restorePos);
				if(eof_flag) { break; }
				ch = tib.get(); 
			}
			
			int len = tib.position() - restorePos - (ch=='\r'||ch=='\n'?1:0);
			if(len < 0) return null; // no string to read!
			if(ch == '\r') restorePos = swallowNewLine(restorePos);
			return new BufferString(tibAddr,tib,restorePos,len);
		}
			
		public void skip(byte delim) {
			int restorePos = tib.position();
			byte ch = 0x00;
			
			restorePos = prepareForRead(restorePos);
			if(eof_flag) return;
			ch = tib.get(); 
			if(Character.isWhitespace((char)ch)) {
				   ch = ' ';
			}
			
			while(ch == delim) {
				++restorePos;
				restorePos = prepareForRead(restorePos);
				if(eof_flag) { break; }
				ch = tib.get(); 
				if(Character.isWhitespace((char)ch)) {
					   ch = ' ';
				}
			}
			
			// now we found a character not matching DELIM...
			tib.position(restorePos); // so go back to that spot.
		}
		
		public BufferString readWord(byte delim) {
			skip(delim);
			
			int restorePos = tib.position();
			byte lastRead = 0x00;
			byte ch = 0x00;
			
			restorePos = prepareForRead(restorePos);
			if(eof_flag) return null;
			ch = tib.get(); 
			if(Character.isWhitespace((char)ch)) {
				   ch = ' ';
			}
			
			while(ch != delim) {
				restorePos = prepareForRead(restorePos);
				if(eof_flag) { break; }
				ch = tib.get(); 
				lastRead = ch;
				if(Character.isWhitespace((char)lastRead)) {
					   ch = ' ';
				}
			}
			
			int len = tib.position() - restorePos - (ch==delim?1:0);
			if(len < 0) return null; // no string to read!
			if(lastRead == '\r') restorePos = swallowNewLine(restorePos);
			return new BufferString(tibAddr,tib,restorePos,len);		
		}
			
		private int swallowNewLine(int pos) {
			pos = prepareForRead(pos);
			if(!eof_flag) {
				int ch = tib.get(tib.position());
				if(ch == '\n') tib.get();
			}
			return pos;
		}		
		
	} 

	private class StringSource extends Source {
		public StringSource(PointerManagement mm, BufferString bs) {
			tib = ByteBuffer.wrap(bs.buffer.array(), bs.startPosition, bs.length);
			tib.order(ByteOrder.nativeOrder());
			tibAddr = bs.baseAddr;
			channel = null;
		}
		
		@Override
		protected int prepareForRead(int pos) {
			if(!tib.hasRemaining()) { eof_flag = true; }
			return pos;
		}
		
	}
	
	
	
	private ArrayList<Source> sources;
	private int bufSz; // temporary input buffer
	private Source defaultSource; // when a source isn't specified,
	                              // use this one...
	private boolean rollToStdIn;  // roll over to stdin when the defaultsource
	                              // runs out of bytes...
	private PointerManagement mm;
	
	public FReader(int inbuf, VM vm, ReadableByteChannel stdin) {
		sources = new ArrayList<Source>();
		bufSz = inbuf;
		
		// allocate a buffer for STDIN...
		mm = vm.MM;
		defaultSource = new Source(mm,stdin,bufSz);
		sources.add(defaultSource); // index 0 is STDIN!
		rollToStdIn = false;
	}

	//==================================
	// state saving/restoring methods...
	// for use when EVALUATE-ing stuff and
	// INCLUDED-ing stuff
	//==================================
	class State {
		public Source defSource;
		public boolean rollOver;
		public State(Source s, boolean r) { defSource = s; rollOver = r; }
	}
	
	public State saveState() {
		return new State(defaultSource,rollToStdIn);
	}
	
	public void restoreState(State s) {
		defaultSource = s.defSource;
		rollToStdIn = s.rollOver;
	}
	//====================================================
	
	public int newSource(ReadableByteChannel rdr) {
		sources.add(new Source(mm,rdr,bufSz));
		return sources.size() - 1;
	}
	
	public int newSource(BufferString str) {
		sources.add(new StringSource(mm,str));
		return sources.size() - 1;
	}
	
	// set the default source for subsequent reads...
	public void setDefaultSource(int src, boolean rollOver) {
		defaultSource = sources.get(src); 
		rollToStdIn = rollOver;
	}

	public BufferString readWord(int fileno, byte delim) {
		BufferString ans = null;
	
		if(fileno < 0) {
			ans = defaultSource.readWord(delim);
			if(ans == null && rollToStdIn) {
				defaultSource.closeSource(mm);
				defaultSource = sources.get(0);
				rollToStdIn = false;
				ans = defaultSource.readWord(delim);
			}
		} else {
			ans = sources.get(fileno).readWord(delim);
		}
		return ans;
	}

	public void skip(int fileno, byte delim) {
		if(fileno < 0) {
			defaultSource.skip(delim);
		} else {
			sources.get(fileno).skip(delim);
		}
	}

	public int readRawChar(int fileno) {
		int ans = -1;
		
		if(fileno < 0) {
			ans = defaultSource.readRawChar();
			if(ans == -1 && rollToStdIn) {
				defaultSource.closeSource(mm);
				defaultSource = sources.get(0);
				rollToStdIn = false;
				ans = defaultSource.readRawChar();
			}
		} else {
			ans = sources.get(fileno).readRawChar();
		}
		
		return ans;
	}

	public BufferString parse(int fileno, byte delim) {
		BufferString ans = null;
		
		if(fileno < 0) {
			ans = defaultSource.parse(delim);
			if(ans == null && rollToStdIn) {
				defaultSource.closeSource(mm);
				defaultSource = sources.get(0);
				rollToStdIn = false;
				ans = defaultSource.parse(delim);
			}
		} else {
			ans = sources.get(fileno).parse(delim);
		}
		return ans;
	}
	

	
}
