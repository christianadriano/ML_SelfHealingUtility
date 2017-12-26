package rserve.client;

import org.rosuda.REngine.REXP;
import org.rosuda.REngine.Rserve.RConnection;



public class xboostTreeModel {

	private RConnection rconnect;
	private REXP x;
	
	public xboostTreeModel(RConnection c){
		this.rconnect = c;
	}
	
	public void showDataSummary(){
		try{
			x = rconnect.eval("summary(dataf)");
			x.getAttribute("a");
			System.out.println(x.toString());
			String outputStr = x.asString();
			System.out.println(outputStr);
		}
		catch(Exception e){
			System.out.println(e.toString());
		}
	}
	
	public static void main(String args[] ){
		try{
			RConnection c = new RConnection();
			/*REXP x = c.eval("R.version.string");
			String outputStr = x.asString();
			System.out.println(outputStr);*/
			
			xboostTreeModel model = new xboostTreeModel(c);
			model.showDataSummary();
			

		}
		catch(Exception e){
			System.out.println(e.toString());
		}

	}
}
