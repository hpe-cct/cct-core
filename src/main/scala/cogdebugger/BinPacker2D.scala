package cogdebugger

import java.awt.Rectangle

import scala.collection.mutable
import scala.swing._

/* A common approach to 2D bin packing is to use a binary tree to partition
 * the space as items are inserted, but this implementation goes about it
 * differently, in order to better handle a specific use case - window tiling
 * on a desktop.
 *
 * BinPacker2D maintains a regular grid of cells that represent portions of
 * the packing area. Cells have links to their neighbors, and know if they
 * represent free or occupied space. Packed items are not limited to a single
 * cell; they can span multiple cells. Cells can be subdivided even after they
 * have been occupied by an object. This allows the grid to remain regular -
 * at any given moment, all the cells in a column have the same width, and all
 * the cells in a row have the same height.
 *
 * When it comes time to insert a new item, the cells are scanned until one
 * representing free space is found. If the cell is too small to accommodate
 * the item, neighboring cells are checked to see if they are also free and can
 * collectively provide enough space to fit the item. If the space found ends
 * up being larger than the item (or if the initial cell was already larger),
 * then a column and/or row of cells are split to produce a region exactly the
 * same size as the inserted object. If no region of sufficient size can be
 * found by expanding from the cell, then the scan continues with the next
 * cell. In the event that there is no region that can accommodate the new
 * item, the insertion operation will signal failure (with a None option type).
 *
 * Packed objects can be removed from the grid. The cells that they span are
 * simply marked as free, at which point the insertion operation described
 * above will be able to use them again for newly inserted objects.
 *
 * Initially, the grid is simply a single cell covering the whole packable
 * area. As items are inserted, it will become increasingly fragmented. In the
 * degenerate case, it is fragmented to the point that each cell represents a
 * single pixel on screen. The reset operation will throw away the current grid
 * and start over with a new single, big cell.
 */

/** Packs rectangles of arbitrary sizes into a larger rectangular area.
  *
  * This implementation was primarily designed to help with automatic window
  * tiling on a desktop. It's assumed that the number and sizes of items to be
  * packed are not known in advance. Additionally, items may be removed from
  * the packed area at any time, in which case the freed space can be reused
  * by subsequently inserted items.
  *
  * @param initialSize dimensions of the area into which items are to be packed
  *
  * @author Tobin Gonzalez
  */
class BinPacker2D(initialSize: Dimension) {

  def this(width: Int, height: Int) = this(new Dimension(width, height))

  private var dim: Dimension = initialSize
  private var root = TilerCell(new Rectangle(dim))

  /** Insert an object with the given dimensions into the packable area, if
    * possible. If a place can be found that fits the object, a Rectangle
    * indicating the location and size is returned. A return value of None
    * indicates that no region could be found that can accommodate the object.
    *
    * @param dim The size (width and height) of the object to be inserted into
    *            the packable area
    * @param content The object being packed into the area
    */
  def insert(dim: Dimension, content: AnyRef): Option[Rectangle] = {
    for (col <- grid; cell <- col if cell.content.isEmpty) {
      val placement = insertAt(cell, dim, content)
      if (placement.isDefined) { return placement }
    }
    None
  }

  /** Throw away the existing packed area and start over with a new one of the
    * given size.
    *
    * @param dim Size of the new packed area.
    */
  def reset(dim: Dimension): Unit = {
    this.dim = dim
    root = TilerCell(new Rectangle(dim))
  }

  /** Remove the given object from the packable area, freeing the space it
    * occupied for use by other objects.
    *
    * @param content Object to be removed from packable area
    */
  def remove(content: AnyRef): Unit = {
    for (col <- grid; cell <- col; c <- cell.content if c eq content) {
      cell.content = None
    }
  }

  // TODO implement expand op
//  /** Grow the packable region by the given width and height. The extra space
//    * is added to the right and below the existing space. */
//  def expand(width: Int, height: Int): Unit = {
//    ???
//  }

  // Can we safely implement a shrink() method? In the event the shrink op
  // would cut occupied cells, what do we do? A shrink() followed by expand()
  // could be bad; any content that was sticking past the edge of the packable
  // area would need to be accounted for in the expand.

  /** Get all the existing cells as a 2D array (an array of columns) */
  private def grid: Seq[Seq[TilerCell]] = {
    // Prefer row major or column major?
    //val firstRow = mutable.Buffer(root)
    //while (firstRow.last.right.isDefined) { firstRow += firstRow.last.right.get }
    //val rows = mutable.Buffer(firstRow)
    //while(rows.last.head.down.isDefined) { rows += rows.last.map(_.down.get) }
    //rows
    val firstCol = mutable.Buffer(root)
    while (firstCol.last.down.isDefined) { firstCol += firstCol.last.down.get }
    val cols = mutable.Buffer(firstCol)
    while(cols.last.head.right.isDefined) { cols += cols.last.map(_.right.get) }
    cols
  }

  /** Try to insert the content with the given dimension into the given cell.
    * The initial cell will be expanded to include empty cells to the right and
    * below if necessary.
    */
  private def insertAt(cell: TilerCell, dim: Dimension, content: AnyRef): Option[Rectangle] = {
    val (x, y) = (cell.rect.x, cell.rect.y)

    var gatheredWidth  = cell.rect.width
    var gatheredHeight = cell.rect.height

    // Expand selection to the right (if necessary)
    val firstRow = mutable.Buffer(cell)
    while (gatheredWidth < dim.width) {
      firstRow.last.right match {
        case Some(right) if right.content.isEmpty =>
          firstRow += right
          gatheredWidth += right.rect.width
        case _ =>
          // Either we're at the edge of the grid with nothing to the right, or
          // the cells to the right are already occupied
          return None
      }
    }

    // Expand the selection down (if necessary)
    val rows = mutable.Buffer(firstRow)
    while (gatheredHeight < dim.height) {
      val curRow = rows.last
      if (curRow.head.down.isEmpty) {
        // On last row of grid; no space below
        return None
      }
      if (!curRow.forall(_.down.get.content.isEmpty)) {
        // Something already occupies some space in the row below
        return None
      }
      val nextRow = curRow.map(_.down.get)
      rows += nextRow
      gatheredHeight += nextRow.head.rect.height
    }

    splitVertical(x + dim.width)    // No-op if gatheredWidth == dim.width
    splitHorizontal(y + dim.height) // No-op if gatheredHeight == dim.height

    val actualWidth = firstRow.foldLeft(0)(_ + _.rect.width)
    val actualHeight = rows.map(_.head).foldLeft(0)(_ + _.rect.height)

    // sanity check
    require(actualWidth == dim.width && actualHeight == dim.getHeight,
      s"Size mismatch: got ${actualWidth}x${actualHeight} region for ${dim.width}x${dim.height} object")

    for (row <- rows; cell <- row) cell.content = Some(content)
    Some(new Rectangle(x, y, actualWidth, actualHeight))
  }

  /** Vertically split the grid of cells at the given x coordinate. Has no
    * effect if x lands on an existing border between columns. */
  private def splitVertical(x: Int): Unit = {
    val col = getColIdx(x)
    val oldCells = col match {
      case Some(c) => getCol(c)
      case None => return
    }
    val firstCell = oldCells.head
    val startX = firstCell.rect.x
    val endX = startX + firstCell.rect.width

    if (startX == x) {
      // We landed right at the start of an existing column, in which case
      // there's nothing to do
      return
    }

    require(startX <= x && endX > x, s"Split at x=$x; startX=$startX, endX=$endX") // sanity check

    // Create the new cells, one row at a time, hooking up the left and right
    // connections as we go
    val wLeft = x - firstCell.rect.x // width left of split
    val wRight = endX - x            // width right of split
    val newCells = oldCells.map { oldCell =>
        val leftRect  = new Rectangle(oldCell.rect.x, oldCell.rect.y,  wLeft, oldCell.rect.height)
        val rightRect = new Rectangle(x,              oldCell.rect.y, wRight, oldCell.rect.height)
        val newCell = TilerCell(rightRect, oldCell.content, None, None, Some(oldCell), oldCell.right)
        val oldRight = oldCell.right
        oldCell.rect = leftRect
        oldCell.right = Some(newCell)
        oldRight.foreach(_.left = Some(newCell))
        newCell
      }

    // Wire up the up/down connections
    for (i <- newCells.indices) {
      val above = i - 1
      val below = i + 1
      val cell = newCells(i)
      if (above >= 0) cell.up = Some(newCells(above))
      if (below < newCells.length) cell.down = Some(newCells(below))
    }
  }

  /** Horizontally split the grid of cells at the given y coordinate. Has no
    * effect if y lands on an existing border between rows. */
  private def splitHorizontal(y: Int): Unit = {
    val row = getRowIdx(y)
    val oldCells = row match {
      case Some(r) => getRow(r)
      case None => return
    }
    val firstCell = oldCells.head
    val startY = firstCell.rect.y
    val endY = startY + firstCell.rect.height

    if (startY == y) { return }

    require(startY <= y && endY > y, s"split at y=$y; startY=$startY, endY=$endY") // sanity check

    val hAbove = y - firstCell.rect.y
    val hBelow = endY - y
    val newCells = oldCells.map { oldCell =>
      val aboveRect = new Rectangle(oldCell.rect.x, oldCell.rect.y, oldCell.rect.width, hAbove)
      val belowRect = new Rectangle(oldCell.rect.x,              y, oldCell.rect.width, hBelow)
      val newCell = TilerCell(belowRect, oldCell.content, Some(oldCell), oldCell.down, None, None)
      val oldDown = oldCell.down
      oldCell.rect = aboveRect
      oldCell.down = Some(newCell)
      oldDown.foreach(_.up = Some(newCell))
      newCell
    }

    for (i <- newCells.indices) {
      val left = i - 1
      val right = i + 1
      val cell = newCells(i)
      if (left >= 0) cell.left = Some(newCells(left))
      if (right < newCells.length) cell.right = Some(newCells(right))
    }
  }

  /** Get the index of the column that starts at or spans the given x
    * coordinate */
  private def getColIdx(x: Int): Option[Int] = {
    val idx = getRow(0).indexWhere {
      cell => cell.rect.x <= x && (cell.rect.x + cell.rect.width) > x
    }
    if (idx < 0) None else Some(idx)
  }

  /** Get the index of the row that starts at or spans the given y
    * coordinate */
  private def getRowIdx(y: Int): Option[Int] = {
    val idx = getCol(0).indexWhere { cell =>
      cell.rect.y <= y && (cell.rect.y + cell.rect.height) > y
    }
    if (idx < 0) None else Some(idx)
  }

  /** Get the cells in the given column */
  private def getCol(idx: Int): Seq[TilerCell] = grid(idx)

  /** Get the cells in the given row */
  private def getRow(idx: Int): Seq[TilerCell] = grid.map(_(idx))

}

case class TilerCell(
  var rect: Rectangle,
  var content: Option[AnyRef],
  var up: Option[TilerCell],
  var down: Option[TilerCell],
  var left: Option[TilerCell],
  var right: Option[TilerCell])

object TilerCell {
  def apply(rect: Rectangle): TilerCell =
    new TilerCell(rect, None, None, None, None, None)
}

