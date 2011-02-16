package program;

import org.armedbear.lisp.Cons;
import org.armedbear.lisp.Fixnum;
import org.armedbear.lisp.Function;
import org.armedbear.lisp.Interpreter;
import org.armedbear.lisp.JavaObject;
import org.armedbear.lisp.LispObject;
import org.armedbear.lisp.MacroObject;
import org.armedbear.lisp.Packages;
import org.armedbear.lisp.Package;
import org.armedbear.lisp.Symbol;



public class Main {
	public static void main(String[] args){
		//testABCL();
		loadTmSparql();
	}
	
	
	public static void testABCL(){
		// load the file functions.lisp which also evaluates a let as last command 
		Interpreter interpreter = Interpreter.createInstance();
		interpreter.eval("(load \"lisp-code/test-code/functions.lisp\")");

		
		// use the lisp function print-line
		Package defaultPackage = Packages.findPackage("CL-USER");
		Symbol myFunctionSym = defaultPackage.findAccessibleSymbol("PRINT-LINE");
		Function printLineFun = (Function)myFunctionSym.getSymbolFunction();
		LispObject lispString = JavaObject.getInstance("This is a java string", true);
		printLineFun.execute(lispString);
		
		
		// use the lisp function add
		myFunctionSym = defaultPackage.findAccessibleSymbol("ADD");
		Function addFun = (Function)myFunctionSym.getSymbolFunction();
		LispObject lispInt1 = JavaObject.getInstance(6, true);
		LispObject lispInt2 = JavaObject.getInstance(2, true);
		LispObject result = addFun.execute(lispInt1, lispInt2);
		System.out.println(result.intValue());

		
		// use the build-i function cons
		myFunctionSym = defaultPackage.findAccessibleSymbol("CONS");
		Function consFun = (Function)myFunctionSym.getSymbolFunction();
		Cons list = (Cons) consFun.execute(Fixnum.getInstance(64), Fixnum.getInstance(65));
		System.out.println(list.car.intValue() + ", " + list.cdr.intValue());
	}
	
	
	public static void loadTmSparql(){
		// === load base-tools.lisp ===========================================
		Interpreter interpreter = Interpreter.createInstance();
		interpreter.eval("(load \"lisp-code/base-tools/base-tools.lisp\")");
		
		
		// === load sparql.lisp ===============================================
		//interpreter.eval("(load \"lisp-code/TM-SPARQL/sparql.lisp\")");
		//TODO: import datamodel => implement an abstract datamodel
		
		
		// === test the loaded files ==========================================
		Package defaultPackage = Packages.findPackage("BASE-TOOLS");
		Symbol myFunSym = defaultPackage.findAccessibleSymbol("separate-leading-digits".toUpperCase());
		Function strFun = (Function)myFunSym.getSymbolFunction();
		
		LispObject str1 = JavaObject.getInstance("no leading digits in this string", true);
		LispObject str2 = JavaObject.getInstance("123 string started with 3 digits", true);
		System.out.println(strFun.execute(str1));
		System.out.println(strFun.execute(str2));
	}
}
