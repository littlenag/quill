import io.getquill.{Literal, PipelineDBDialect, PostgresAsyncContext, SqlMirrorContext}

{
  val ctx = new PostgresAsyncContext(Literal, "")
  import ctx._

  case class Metrics(name:String, value:Int, ts:Int)

  val c = quote {
    query[Metrics].filter(_.value > 10)
    query[Metrics].groupBy(_.name).map {
      case (n, q) => (n, q.avg)
    }.filter {
      case (n,a) =>
    }
  }

  val result /* Future[List[Metrics]] */= ctx.run(c)

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
