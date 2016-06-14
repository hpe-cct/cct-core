package cogdebugger.coggui3ports

import org.interactivemesh.scala.swing.InternalFrame

/**
 * This event is used to signal that a ProbeFrame has sized itself and is
 * ready for display. This is important for the probe desktop's window tiler,
  * as it can't know where to place a ProbeFrame until the frame's final size
  * is determined.
 */
case class FramePackedEvent(source: InternalFrame) extends scala.swing.event.UIEvent
