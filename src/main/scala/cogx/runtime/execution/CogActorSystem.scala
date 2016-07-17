/*
 * (c) Copyright 2016 Hewlett Packard Enterprise Development LP
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cogx.runtime.execution

import akka.actor._
import cogx.parameters.Cog
import com.typesafe.config.ConfigFactory

/** Actor system for Cog which implements the computation.
  *
  * The typical Akka actor scenario uses static configuration in an
  * `application.conf` file which is not a good match for Cog.
  * So we need to do additional magic here to make Akka do what we
  * need. In particular, the `application.conf` file used by Akka is too rigid
  * for our needs, so we do all configuration programmatically within this
  * file.
  *
  * @author Greg Snider
  */
private[cogx]
object CogActorSystem {
  /** Maximum number of actors with dedicated threads in system. */
  private val MaxThreadedActors = 100

  /** Dispatcher configuration which allocates a thread for each actor, needed
    * for Cog actors which do I/O or otherwise call blocking operations.
    *
    * Some other attempts at creating a one-actor-per-thread dispatcher
    * didn't work.  Playing with the parameters of the default dispatcher
    * created a system with the correct number of threads, but the application
    * hung around for keep-alive-time before terminating.  Setting the
    * keep-alive-time to a low value resulted in threads that terminated
    * when their actor was idle for longer than that time.
    *
    * The solution that worked stayed close to the recommendations of the
    * "Dispatchers" section of the Akka documentation (section 5.9).  However,
    * the suggestion to set allow-core-timeout = off seemed ineffective, and
    * the threads timed out after the default 60 seconds of idleness.  Setting
    * keep-alive-time to a large value corrected this.
    *
    * The UnboundedDequeueBasedMailbox is a requirement to use the Stash trait.
    * Stash is useful when one wants an Actor to "become" another temporarily,
    * without losing the messages that belong to the previous context.  The
    * GPUSupervisor class used stash at one time, but no longer does so.
    */
  private val oneActorPerThreadDispatcher =
    s"""
      | CogActors {
      |   one-actor-per-thread-dispatcher {
      |      type = PinnedDispatcher
      |      executor = thread-pool-executor
      |      thread-pool-executor {
      |        core-pool-size-min = 1
      |        core-pool-size-factor = 1.0
      |        core-pool-size-max = $MaxThreadedActors
      |      }
      |      thread-pool-executor.allow-core-timeout = off
      |      thread-pool-executor.keep-alive-time = 365 days
      |      mailbox-type = akka.dispatch.UnboundedDequeBasedMailbox
      |   }
      | }
      |
    """.stripMargin

  /** Typed actors have a hardwired 5 second timeout for blocking (non-void)
    * methods. This is too rigid for our testing, so we override that here.
    */
  private val typedActorBlockingMethodsTimeout =
    """
      | akka {
      |   actor {
      |     typed {
      |       timeout = 1000s
      |     }
      |   }
      | }
      |
    """.stripMargin

  /** We don't try to run past a thrown exception from any of the Actors, thus
    * we want the supervisor to stop the actors, not restart them.
    */
  private val stoppingSupervisorStrategy =
    """
      | akka {
      |   actor {
      |     guardian-supervisor-strategy = "akka.actor.StoppingSupervisorStrategy"
      |   }
      | }
      |
    """.stripMargin

  /** We don't try to run past a thrown exception from any of the Actors, thus
    * we want the supervisor to stop the actors, not restart them.
    */
  private val debug =
    """
      | akka {
      |   loglevel = "DEBUG"
      |   actor {
      |     debug {
      |       lifecycle = on
      |       unhandled = on
      |       autoreceive = on
      |     }
      |   }
      | }
      |
    """.stripMargin

  /** Exceptions thrown in the CPU kernel actors are messaged to the CircuitEvaluator,
    * which throws the exception.  The CiruitEvaluator is a TypedActor that will be
    * stopped.  There are often some "dead letter" messages still floating around (e.g.
    * from the Debugger GUI) that can be ignored.
    */
  private val disableDeadLetterLogging =
    """
      | akka {
      |   log-dead-letters = 0
      |   log-dead-letters-during-shutdown = false
      | }
      |
    """.stripMargin

  /** Create a system of Cog actors, one per application. */
  def apply(): ActorSystem = {
      ActorSystem("CogActors", createConfig)
  }

  /** Create an Akka configuration for Cog. */
  private def createConfig =  {
    val regularConfig = ConfigFactory.load()
    val configString =
      oneActorPerThreadDispatcher +
        typedActorBlockingMethodsTimeout +
          stoppingSupervisorStrategy +
            disableDeadLetterLogging  // + debug
    val myConfig = ConfigFactory.parseString(configString)
    val combined = myConfig.withFallback(regularConfig)
    val completeConfig = ConfigFactory.load(combined)
    completeConfig
  }

  /** Creating Akka actors through this single path allows the user to experiment
    * with creating the actors by the default dispatcher (which allows the actors
    * to share their threads) or by using a Cog-customized dispatcher (which assigns
    * each actor to its own thread).
    *
    * @param context The ActorContext within which to create the Actor
    * @param props The Props to use to create the Actor (which includes the dispatcher info)
    * @param name The name of the Actor
    */
  def createActor(context: ActorContext, props: Props, name: String) = {

    // Having actors share threads is the default Akka dispatcher's behavior
    val customProps =
      if (Cog.threadSharing)
        props
      else
        props.withDispatcher("CogActors.one-actor-per-thread-dispatcher")

    context.actorOf(customProps, name)
  }

  /** Creating cpu kernel Akka actors through this single path allows the user to experiment
    * with creating the actors by the default dispatcher (which allows the actors
    * to share their threads) or by using a Cog-customized dispatcher (which assigns
    * each actor to its own thread).
    *
    * This was broken off from the core Akka creation mechanism because the Sensors should run
    * in parallel with the GPUSupervisor.  One might experiment with having core actors share
    * threads, but not the CPU kernels (or vice versa).
    *
    * @param context The ActorContext within which to create the Actor
    * @param props The Props to use to create the Actor (which includes the dispatcher info)
    * @param name The name of the Actor
    */
  def createCpuKernelActor(context: ActorContext, props: Props, name: String) = {

    // Having actors share threads is the default Akka dispatcher's behavior
    val customProps =
      if (Cog.cpuKernelThreadSharing)
        props
      else
        props.withDispatcher("CogActors.one-actor-per-thread-dispatcher")

    context.actorOf(customProps, name)
  }
}