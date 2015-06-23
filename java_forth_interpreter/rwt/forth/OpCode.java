package rwt.forth;

/**
 * This is the interface that Forth Values
 * will support
 * @author Richard
 *
 */
abstract public class OpCode {
   public boolean immediate;
   private int map_address; // where are we in VM's MM?
   
   public OpCode() {
	   immediate = false;
	   map_address = -1;
   }
   
   abstract public void run(VM vm);
   
   public long address(VM vm) {
	   long ans = map_address; 
	   if(ans < 1) {
		   map_address = vm.MM.add(this);
		   ans = map_address;
	   }
	   return (ans << 32);
   }
   
}
