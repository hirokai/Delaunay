import ij.measure.ResultsTable
import scala.collection.mutable.ArrayBuffer
import scala.math._

trait SpacingResult {
	def addPair(coord: PairElem): Unit	//ax, ay, bx, by
	def getNextPair: PairElem //ax, ay, bx, by, dist
	def mean: Double
	def stdev: Double
	def getSummary: String
	def getAllDataString: String
	def removeDuplicate: Unit
	def sort: Unit
	def showResultsTable: Unit
}

case class PairElem(ax: Double,ay:Double,bx:Double,by:Double,var dist:Double){
  val E: Double = 0.001
  def equals(p: PairElem): Boolean = {
		if(
				abs(ax-p.ax)<E &&
				abs(ay-p.ay)<E &&
				abs(bx-p.bx)<E &&
				abs(by-p.by)<E)
			return true;
		else if(
				abs(ax-p.bx)<E &&
				abs(ay-p.by)<E &&
				abs(bx-p.ax)<E &&
				abs(by-p.ay)<E)
			return true;
		else
			return false;
  }
}

class SpacingResultCoord extends SpacingResult {
	
	var pair: ArrayBuffer[PairElem] = new ArrayBuffer[PairElem]
	var iterator_index: Int = _

	var meanNumAdjacent = Double.NaN;

	def removeDuplicate: Unit = {
		val ret = new ArrayBuffer[PairElem]
		for(p <- pair){
			val index = ret.indexWhere(_.equals(p))
			if(index == -1)
				ret += p
		}
		pair = ret
	}
	def count = pair.length
	def mean: Double = pair.map(_.dist).sum / pair.length
	def getNextPair: PairElem = {
	    var r: PairElem = null
		if(iterator_index<count){
			r = pair(iterator_index)
			iterator_index += 1
		}else
		  r = null
		return r
	}
	def addPair(p: PairElem): Unit = {
		pair += PairElem(ax=p.ax, ay=p.ay, bx=p.bx, by=p.by,
						dist=sqrt(pow(p.ax-p.bx,2)+pow(p.ay-p.by,2)))
	}
	def stdev = {
	  val m = mean
	  sqrt(pair.map({p:PairElem => pow(p.dist-m,2)}).sum / count)
	}
	def getSummary: String = 
		("%d pairs. Mean: %.2f Stdev: %.2f. "+
				"Average # of adjacent points: %.3f \n").format(count,mean,stdev,meanNumAdjacent);
	def getAllDataString: String = {
		val str = new StringBuffer
		str.append("AX\tAY\tBX\tBY\tdistance (in nm)\n");
		pair.foreach({p:PairElem=>
		  str.append("%.2f\t%.2f\t%.2f\t%.2f\t%.2f\n".format(p.ax, p.ay, p.bx, p.by, p.dist))
		})
		str.toString
	}
	def showResultsTable: Unit = {
		val rt = ResultsTable.getResultsTable
		rt.reset
		pair.foreach({p=>
			rt.incrementCounter();
			rt.addValue("AX", p.ax);
			rt.addValue("AY", p.ay);
			rt.addValue("BX", p.bx);
			rt.addValue("BY", p.by);
			rt.addValue("dist", p.dist);
		})
		rt.show("Results")
	}
	def sort: Unit = {
	  pair = pair.sortWith((a,b)=>a.dist<b.dist)
	  return
	}
}
