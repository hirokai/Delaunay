package delaunay;
import scala.math._

/*
 * Copyright (c) 2005, 2007 by L. Paul Chew.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 * Modified and Ported to Scala by HK, Nov 6, 2011
 */

/**
 * Points in Euclidean space, implemented as double[].
 *
 * Includes simple geometric operations.
 * Uses matrices; a matrix is represented as an array of Pnts.
 * Uses simplices; a simplex is represented as an array of Pnts.
 *
 * @author Paul Chew
 *
 * Created July 2005.  Derived from an earlier, messier version.
 *
 * Modified Novemeber 2007.  Minor clean up.
 */

object Pnt {
      /* Pnts as matrices */

    /**
     * Create a String for a matrix.
     * @param matrix the matrix (an array of Pnts)
     * @return a String represenation of the matrix
     */
    def toString (matrix: Array[String]): String = "{" + matrix.mkString(" ") + "}"

    /**
     * Compute the determinant of a matrix (array of Pnts).
     * This is not an efficient implementation, but should be adequate
     * for low dimension.
     * @param matrix the matrix as an array of Pnts
     * @return the determinnant of the input matrix
     * @throws IllegalArgumentException if dimensions are wrong
     */
    def determinant (matrix: Array[Pnt]): Double = {
        if (matrix.length != matrix(0).dimension)
            throw new IllegalArgumentException("Matrix is not square");
        var columns = new Array[Boolean](matrix.length)
        for (i <- 0 until matrix.length) columns(i) = true
        try {return determinant(matrix, 0, columns)}
        catch{case e:ArrayIndexOutOfBoundsException =>
            throw new IllegalArgumentException("Matrix is wrong shape")
        }
    }

    /**
     * Compute the determinant of a submatrix specified by starting row
     * and by "active" columns.
     * @param matrix the matrix as an array of Pnts
     * @param row the starting row
     * @param columns a boolean array indicating the "active" columns
     * @return the determinant of the specified submatrix
     * @throws ArrayIndexOutOfBoundsException if dimensions are wrong
     */
    private def determinant(matrix: Array[Pnt], row: Int, columns: Array[Boolean]): Double = {
        if (row == matrix.length) return 1
        var sum:Double = 0;
        var sign = 1;
        for (col <- 0 until columns.length if columns(col)) {
            columns(col) = false;
            sum += sign.toDouble * matrix(row).coordinates(col) *
                   determinant(matrix, row+1, columns);
            columns(col) = true;
            sign = -sign;
        }
        return sum;
    }

    /**
     * Compute generalized cross-product of the rows of a matrix.
     * The result is a Pnt perpendicular (as a vector) to each row of
     * the matrix.  This is not an efficient implementation, but should
     * be adequate for low dimension.
     * @param matrix the matrix of Pnts (one less row than the Pnt dimension)
     * @return a Pnt perpendicular to each row Pnt
     * @throws IllegalArgumentException if matrix is wrong shape
     */
    def cross(matrix: Array[Pnt]): Pnt = {
        val len = matrix.length + 1;
        if(len != matrix(0).dimension)
            throw new IllegalArgumentException("Dimension mismatch");
        val columns = new Array[Boolean](len)
        for (i <- 0 until len)
          columns(i) = true;
        val result = new Array[Double](len)
        var sign = 1;
        try {
            for (i <- 0 until len) {
                columns(i) = false;
                result(i) = sign * determinant(matrix, 0, columns);
                columns(i) = true;
                sign = -sign;
            }
        } catch{case e: ArrayIndexOutOfBoundsException =>
            throw new IllegalArgumentException("Matrix is wrong shape");
        }
        return new Pnt(result);
    }

    /* Pnts as simplices */

    /**
     * Determine the signed content (i.e., area or volume, etc.) of a simplex.
     * @param simplex the simplex (as an array of Pnts)
     * @return the signed content of the simplex
     */
    def content (simplex: Array[Pnt]): Double = {
        val matrix = new Array[Pnt](simplex.length)
        for(i <- 0 until matrix.length)
            matrix(i) = simplex(i).extend(1);
        var fact = (1 until matrix.length).foldLeft(1){(x,y)=>x*y}
        determinant(matrix) / fact
    }
    /**
     * Circumcenter of a simplex.
     * @param simplex the simplex (as an array of Pnts)
     * @return the circumcenter (a Pnt) of simplex
     */
    def circumcenter (simplex: Array[Pnt]):Pnt = {
        val dim = simplex(0).dimension
        if (simplex.length - 1 != dim)
            throw new IllegalArgumentException("Dimension mismatch");
        val matrix = new Array[Pnt](dim)
        for (i <- 0 until dim)
            matrix(i) = simplex(i).bisector(simplex(i+1))
        val hCenter = cross(matrix);      // Center in homogeneous coordinates
        val last = hCenter.coordinates(dim);
        val result = new Array[Double](dim);
        for (i <- 0 until dim) result(i) = hCenter.coordinates(i) / last;
        return new Pnt(result)
    }
    def apply(x: Double,y: Double){
      new Pnt(Array(x,y))
    }
}

class Pnt{

    var coordinates: Array[Double] = _          // The point's coordinates

    /**
     * Constructor.
     * @param coords the coordinates
     */
    def this(coords:Array[Double]) = {
        // Copying is done here to ensure that Pnt's coords cannot be altered.
        // This is necessary because the double... notation actually creates a
        // constructor with double[] as its argument.
		this
		coordinates = new Array[Double](coords.length)
        Array.copy(coords, 0, coordinates, 0, coords.length);
    }
    
    def this(x: Double,y: Double){
      this(Array(x,y))
    }

    override def toString: String = {
        if (coordinates.length == 0) {"Pnt()"}
        else "Pnt(" + coordinates.mkString(",") + ")"
    }
    
    def equals(p: Pnt): Boolean = {
      val E = 0.00001
        if (this.coordinates.length != p.coordinates.length) return false;
        for(i<-coordinates.indices)
            if ((this.coordinates(i) / p.coordinates(i)-1).abs<E) return false;
        return true;
    }

    override def hashCode: Int = {
        var hash = 0;
        for (c <- this.coordinates) {
            val bits = c.hashCode
            hash = (31*hash) ^ (bits ^ (bits >> 32)).toInt
        }
        return hash;
    }

    /* Pnts as vectors */

    /**
     * @return the specified coordinate of this Pnt
     * @throws ArrayIndexOutOfBoundsException for bad coordinate
     */
    def coord(i:Int):Double = this.coordinates(i)

    /**
     * @return this Pnt's dimension.
     */
    def dimension: Int = coordinates.length

    /**
     * Check that dimensions match.
     * @param p the Pnt to check (against this Pnt)
     * @return the dimension of the Pnts
     * @throws IllegalArgumentException if dimension fail to match
     */
    def dimCheck(p:Pnt): Int = {
        val len = this.coordinates.length;
        if (len != p.coordinates.length)
            throw new IllegalArgumentException("Dimension mismatch");
        len
    }

    /**
     * Create a new Pnt by adding additional coordinates to this Pnt.
     * @param coords the new coordinates (added on the right end)
     * @return a new Pnt with the additional coordinates
     */
    def extend (c: Double*): Pnt = {
        val coords: Array[Double] = c.toArray
        val result = new Array[Double](coordinates.length + coords.length)
        Array.copy(coordinates, 0, result, 0, coordinates.length);
        Array.copy(coords, 0, result, coordinates.length, coords.length);
        new Pnt(result)
    }

    /**
     * Dot product.
     * @param p the other Pnt
     * @return dot product of this Pnt and p
     */
    def dot (p: Pnt): Double = {
        val len = dimCheck(p);
        var sum = 0d;
       for(i <- 0 until len)
    	   sum += this.coordinates(i) * p.coordinates(i)
        return sum
    }

    /**
     * Norm (as a vector).
     * @return the Euclidean length of this vector
     */
    def norm: Double = sqrt(this.dot(this))

    /**
     * Subtract.
     * @param p the other Pnt
     * @return a new Pnt = this - p
     */
    def subtract (p:Pnt): Pnt = {
        val len = dimCheck(p);
        val coords = new Array[Double](len)
        for (i <- 0 until len){
            coords(i) = this.coordinates(i) - p.coordinates(i)
        }
        new Pnt(coords);
    }
    def -(p:Pnt):Pnt = subtract(p)

    /**
     * Add.
     * @param p the other Pnt
     * @return a new Pnt = this + p
     */
    def add (p: Pnt): Pnt = {
        val len = dimCheck(p);
        val coords = new Array[Double](len)
        for(i <- 0 until len)
            coords(i) = this.coordinates(i) + p.coordinates(i);
        new Pnt(coords)
    }
    def +(p:Pnt):Pnt = add(p)

    /**
     * Angle (in radians) between two Pnts (treated as vectors).
     * @param p the other Pnt
     * @return the angle (in radians) between the two Pnts
     */
    def angle(p: Pnt): Double = acos(this.dot(p) / (this.norm * p.norm))

    /**
     * Perpendicular bisector of two Pnts.
     * Works in any dimension.  The coefficients are returned as a Pnt of one
     * higher dimension (e.g., (A,B,C,D) for an equation of the form
     * Ax + By + Cz + D = 0).
     * @param point the other point
     * @return the coefficients of the perpendicular bisector
     */
    def bisector (point: Pnt): Pnt = {
        dimCheck(point)
        val diff = this.subtract(point);
        val sum = this.add(point);
        val dot = diff.dot(sum);
        return diff.extend(-dot / 2);
    }


    /**
     * Relation between this Pnt and a simplex (represented as an array of
     * Pnts). Result is an array of signs, one for each vertex of the simplex,
     * indicating the relation between the vertex, the vertex's opposite facet,
     * and this Pnt.
     *
     * <pre>
     *   -1 means Pnt is on same side of facet
     *    0 means Pnt is on the facet
     *   +1 means Pnt is on opposite side of facet
     * </pre>
     *
     * @param simplex an array of Pnts representing a simplex
     * @return an array of signs showing relation between this Pnt and simplex
     * @throws IllegalArgumentExcpetion if the simplex is degenerate
     */
    def relation(simplex: Array[Pnt]): Array[Int] = {
        /* In 2D, we compute the cross of this matrix:
         *    1   1   1   1
         *    p0  a0  b0  c0
         *    p1  a1  b1  c1
         * where (a, b, c) is the simplex and p is this Pnt. The result is a
         * vector in which the first coordinate is the signed area (all signed
         * areas are off by the same constant factor) of the simplex and the
         * remaining coordinates are the *negated* signed areas for the
         * simplices in which p is substituted for each of the vertices.
         * Analogous results occur in higher dimensions.
         */
        val dim = simplex.length - 1;
        if (this.dimension != dim)
            throw new IllegalArgumentException("Dimension mismatch");

        /* Create and load the matrix */
        val matrix = new Array[Pnt](dim+1)
        /* First row */
        var coords = new Array[Double](dim+2)
        coords.indices.foreach(coords(_) = 1)
        matrix(0) = new Pnt(coords)
        /* Other rows */
        for (i <- 0 until dim){
            coords(0) = this.coordinates(i)
            for (j <-0 until simplex.length)
                coords(j+1) = simplex(j).coordinates(i);
            matrix(i+1) = new Pnt(coords)
        }

        /* Compute and analyze the vector of areas/volumes/contents */
        val vector = Pnt.cross(matrix);
        val content: Double = vector.coordinates(0);
        var result = new Array[Int](dim+1)
        for (i <- 0 until result.length) {
            val value = vector.coordinates(i+1);
            result(i) =
              if (value.abs <= 1.0e-6 * content.abs) 0
              else if (value < 0) -1
              else 1
        }
        if (content < 0) result = result.map(-_)
        if (content == 0) result = result.map(_.abs)
        return result;
    }

    /**
     * Test if this Pnt is outside of simplex.
     * @param simplex the simplex (an array of Pnts)
     * @return simplex Pnt that "witnesses" outsideness (or null if not outside)
     */
    def isOutside (simplex: Array[Pnt]): Pnt = {
        def result: Array[Int] = this.relation(simplex)
        val index = result.indexWhere({a=>a>0})
        if(index != -1) simplex(index)
        else null
    }

    /**
     * Test if this Pnt is on a simplex.
     * @param simplex the simplex (an array of Pnts)
     * @return the simplex Pnt that "witnesses" on-ness (or null if not on)
     */
    def isOn (simplex: Array[Pnt]): Pnt = {
        val result = this.relation(simplex)
        var witness: Pnt = null;
        for (i <- 0 until result.length) {
            if (result(i) == 0) witness = simplex(i);
            else if (result(i) > 0) return null;
        }
        return witness
    }

    /**
     * Test if this Pnt is inside a simplex.
     * @param simplex the simplex (an arary of Pnts)
     * @return true iff this Pnt is inside simplex.
     */
    def isInside (simplex: Array[Pnt]): Boolean = {
        val result = this.relation(simplex)
        val index = result.indexWhere(_>=0)
        index != -1
    }

    /**
     * Test relation between this Pnt and circumcircle of a simplex.
     * @param simplex the simplex (as an array of Pnts)
     * @return -1, 0, or +1 for inside, on, or outside of circumcircle
     */
    def vsCircumcircle (simplex: Array[Pnt]): Int = {
        val matrix = new Array[Pnt](simplex.length + 1);
        for (i <- 0 until simplex.length)
            matrix(i) = simplex(i).extend(1, simplex(i).dot(simplex(i)))
        matrix(simplex.length) = this.extend(1, this.dot(this));
        val d = Pnt.determinant(matrix)
        var result = if (d < 0) -1 else (if(d > 0) +1 else 0)
        if (Pnt.content(simplex) < 0) result = - result
        return result;
    }


}