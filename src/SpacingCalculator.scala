import ij.IJ
import ij.measure.ResultsTable
import ij.gui.Roi
import ij.process.ImageStatistics
import ij.plugin.filter.ParticleAnalyzer
import java.awt.Point
import delaunay._
import scala.collection.mutable.ArrayBuffer

	class MyParticleAnalyzer(options:Int, measurements:Int, rt:ResultsTable,
	    minSize:Double, maxSize:Double) extends ParticleAnalyzer(options:Int, measurements:Int, rt:ResultsTable,
	    minSize:Double, maxSize:Double) {
		var particles: ArrayBuffer[Point2] = new ArrayBuffer[Point2]
		override def saveResults(stats: ImageStatistics, roi: Roi): Unit = {
			super.saveResults(stats,roi);
			val x = stats.xCenterOfMass;
			val y = stats.yCenterOfMass;
			particles += Point2(x,y)
		}
		def getPoints = particles
	}


case class Point2(x: Double, y: Double)

class SpacingCalculator {
	var imageWidth: Double = _	//in nm
	var imageHeight: Double = _
	var points: Array[Point2] = _
	var crop: Double = 0	// in nm
	var result: SpacingResult = _
	
	def this(width:Int, height: Int, ps: Array[Point2], factor: Double) = {
		this
		points = new Array[Point2](ps.length)
		scala.Array.copy(ps,0,points,0,ps.length)
		imageWidth = width*factor;
		imageHeight = height*factor;
		scaleWithFactor(factor);
	}
	def scaleWithFactor(factor: Double): Unit = {
		points = points.map({p=>Point2(p.x * factor,p.y * factor)})
	}

	def analyze(cr: Double): SpacingResult = {
		//Threshold in nm
		crop = cr;
		val delaunay = new Triangulation(
				new Triangle(new Pnt(0,imageHeight*(-0.577)),new Pnt(0,imageHeight*1.577)
				,new Pnt(imageWidth*1.866,imageHeight*0.5)));
		result = new SpacingResultCoord
		
		points.foreach({p=>
			delaunay.delaunayPlace(new Pnt(p.x,p.y));
		})
		val iter: java.util.Iterator[Triangle] = delaunay.iterator
		while(iter.hasNext){
			val t = iter.next
			def addIfInRegion(i:Int,j:Int) = {
				var p = new PairElem(ax=t.get(i).coord(0),ay=t.get(i).coord(1),
						bx=t.get(j).coord(0), by=t.get(j).coord(1),dist=Double.NaN)
				if(withinBound(p))
					result.addPair(p)			  
			}
			addIfInRegion(0,1)
			addIfInRegion(1,2)
			addIfInRegion(2,0)
		}
		result.removeDuplicate
		result.sort
		return result
	}
	def withinBound(p: PairElem): Boolean = {
		if(p.ax<crop || p.ax>=imageWidth-crop)	return false;
		else if(p.ay<crop || p.ay>=imageHeight-crop) return false;
		else if(p.bx<crop || p.bx>=imageWidth-crop)	return false;
		else if(p.by<crop || p.by>=imageHeight-crop) return false;
		else return true;
	}
	def getPoint(index: Int): Point2 = points(index)
	def farFromBorder(p: Point2): Boolean = 
		(p.x > crop && p.x < imageWidth - crop
				&& p.y > crop && p.y < imageHeight - crop)
}

