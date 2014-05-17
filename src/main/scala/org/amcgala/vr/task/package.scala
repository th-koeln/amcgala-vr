package org.amcgala.vr

import scala.concurrent.{Promise, Future}
import org.amcgala.vr.BrainModes.BrainMode


package object task {


}

object BrainModes{
  sealed trait BrainMode
  case object NeedMode extends BrainMode
  case object JobMode extends BrainMode
  case object IdleMode extends BrainMode
}


trait BrainModule{s: BotAgent =>
  val brain = new Brain(Bot(s.self))
  def registerNeed(need: Need) = brain.registerNeed(need)
  def registerJob(job: Behavior) = brain.registerJob(job)
}

trait Behavior{
  type Return
  val bot: Bot
  def start(): Future[Return]
}


trait Task{
  val bot: Bot
  type Return
  def execute(): Future[Return]
}

trait MultiStepTask extends Task{
  import scala.concurrent._

  var result: Promise[Return] = promise[Return]

  var isDone = false
  val id = System.nanoTime().toString
  println(s"ID is: $id")
  bot.registerOnTickAction(id, onTick)

  def onTick(): Unit

  def done(): Unit = {
    bot.removeOnTickAction(id)
    isDone = true
  }
}

class Brain(bot: Bot){
  import scala.concurrent.ExecutionContext.Implicits.global

  private var job: Option[Behavior] = None
  private var idle: Option[Behavior] = None
  private var activeBehavior: Option[Behavior] = None

  private var mode: BrainMode = BrainModes.NeedMode
  private val needManager = new NeedManager

  def executeTask(task: Task): Future[task.Return] = {
    val f = task.execute()
    f.onSuccess{case t => println(s"brain: $t")}
    f
  }
  def executeBehavior(behavior: Behavior): Future[behavior.Return] = {
    activeBehavior = Some(behavior)
    behavior.start()
  }

  def registerJob(jobBehavior: Behavior) = job = Some(jobBehavior)
  def registerIdleBehavior(behavior: Behavior) =
  def registerNeed(need: Need) = needManager.registerNeed(need)

  def update(): Unit = {
    if (activeBehavior == None) {
      for(time <- bot.currentTime) {
        println(time)
        mode match {
          case BrainModes.JobMode if time.hours > 16 =>
            // Done with work?! Let's...uhm...EAT!
            mode = BrainModes.IdleMode
          case BrainModes.NeedMode if time.hours > 7 =>
            // Switch to job if we don't have anything else to do. This ensures that we wait until the last SatisfactionBehavior is finished.
            for (j <- job) activeBehavior = Some(j)
            mode = BrainModes.JobMode
          case BrainModes.JobMode =>
            // If it's still time for work and the last job is done, we start over again.
            for (j <- job) activeBehavior = Some(j)
          case BrainModes.NeedMode =>
            needManager.update()
            val suggestions = needManager.needSuggestion
            val strategy = needManager.getSatisfactionStrategyFor(suggestions.head)
            activeBehavior = Some(strategy)
          case BrainModes.IdleMode =>
            needManager.update()
            // TODO IDLE Mode!
        }
      }
    }else{
      // There is an active behavior. Guess we need to update its state and execute the next task.
    }
  }
}