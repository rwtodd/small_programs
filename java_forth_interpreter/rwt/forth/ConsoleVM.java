package rwt.forth;

import java.io.FileInputStream;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;

public class ConsoleVM {

	public void run(ReadableByteChannel toRun) {
		VM vm = new VM(1024 // stack
				      ,1024 // pad
				      ,1024 // TIB
				      ,2048 // STRINGS
				      , Channels.newChannel(System.in));
		
		// add in the standard words, then read the BASE.FTH library
		StandardWords.installStandardWords(vm);
		ReadableByteChannel base = Channels.newChannel(
				getClass().getResourceAsStream("/resources/BASE.FTH")
				);
		vm.CREAD.setDefaultSource(vm.CREAD.newSource(base),false);		
		OpCode interpreter = vm.DCT.lookupWord("[");
		interpreter.run(vm);

		if(toRun != null) vm.CREAD.setDefaultSource(vm.CREAD.newSource(toRun),true);
		interpreter.run(vm);
	}
	
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		System.out.println("Welcome to my Forth!");
		
		ReadableByteChannel sr = null;
		
		if(args.length > 0) {
			try{
				sr = new FileInputStream(args[0]).getChannel();
			} catch(Exception e) {
				e.printStackTrace();
				return;
			}
		} 

		ConsoleVM cvm = new ConsoleVM();
		cvm.run(sr);
		
		try { if(sr != null) sr.close(); } catch(Exception e) { e.printStackTrace(); }
		
		System.out.println("Thanks!");
	}

}
