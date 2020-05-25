import io.getquill.PostgresAsyncContext
import io.spill._
import io.spill.{Literal, ZStreamContext}

{

  // naming is only necessary if one is transpiling
  //   - streaming sql (siddhi)
  //   - esper's eql

  // keep the syntax open so that we can compile to a cep engine like entity that we can add operators to, join on streams, etc
  // the akka graph dsl is probably a good guide to follow that way

  val ctx = new ZStreamContext()   // default or no config, no naming scheme
  import ctx._

  case class Metrics(name:String, value:Int, ts:Int)

  val c = quote {

    val button1 /*: Parser[MouseClick] */ = filter(_.button == 1)

    stream[Metrics].matching(button1)
  }

  val result /* ZTransducer[Any, Nothing, Metrics, Boolean] */= ctx.compile(c)

}


{
  val ctx = new PipelineDBContext(Literal, "")
  import ctx._

  case class Metrics(name:String, value:Int, ts:Int)

  val c = quote {
    select[Metrics].filter(_.value > 10)
  }

  val result = ctx.run(c)

}

{
  // esper: https://dzone.com/articles/complex-event-processing-made
  val config = ???
  val engine = EPServiceProviderManager.getDefaultProvider(config)

  case class TemperatureEvent(value:Int)
  config.addEventTypeAutoName("TemperatureEvent")

  val eplStatement = "select avg(value) as avg_val from TemperatureEvent.win:time_batch(10 sec)"
  val epAdmin = epService.getEPAdministrator()
  //  Statements have 3 states: STARTED, STOPPED and DESTROYED.
  val statementHandle: EPStatement = epAdmin.createEPL(eplStatement)
  statementHandle.setSubscriber(eventProcessor)
}


{
  // flink: https://ci.apache.org/projects/flink/flink-docs-release-1.5/dev/libs/cep.html
  val stream: DataStream[Event] = ...

  val pattern = Pattern.begin("start").where(_.getId == 42)
    .next("middle").subtype(classOf[SubEvent]).where(_.getVolume >= 10.0)
    .followedBy("end").where(_.getName == "end")

  val patternStream = CEP.pattern(stream, pattern)

  val result: DataStream[Alert] = patternStream.select(createAlert(_))
}
