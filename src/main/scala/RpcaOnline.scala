import java.util.{Arrays, Properties, TimeZone}
import org.apache.kafka.clients.consumer.KafkaConsumer
import org.apache.kafka.clients.producer.{KafkaProducer, ProducerRecord}
import rpca._
import scala.jdk.CollectionConverters._
import scala.collection.mutable.ArrayBuffer
import scala.io.Source


class DataBus(input_topic:String, output_topic:String){
	private val src_props = new Properties
	src_props.setProperty("bootstrap.servers", "localhost:9092")
	src_props.setProperty("group.id", "rpca_consumer")
	src_props.setProperty("enable.auto.commit", "false")
	src_props.setProperty("auto.commit.interval.ms", "1000")
	src_props.setProperty("key.deserializer", "org.apache.kafka.common.serialization.StringDeserializer")
	src_props.setProperty("value.deserializer", "org.apache.kafka.common.serialization.StringDeserializer")
	private val consumer = new KafkaConsumer[String, String](src_props)
	consumer.subscribe(Arrays.asList(input_topic))
	consumer.poll(0) //start poll
	consumer.assignment.asScala.foreach(partition => consumer.seekToEnd(partition))
	val source = Iterator.continually(consumer.poll(Long.MaxValue).asScala).flatten.map(_.value)

	private val dst_props = new Properties
	dst_props.put("bootstrap.servers", "localhost:9092")
	dst_props.put("key.serializer", "org.apache.kafka.common.serialization.StringSerializer")
	dst_props.put("value.serializer", "org.apache.kafka.common.serialization.StringSerializer")
	private val sink = new KafkaProducer[String, Double](dst_props)

	def write(msg:Double) = sink.send(new ProducerRecord(output_topic, msg))

}

object DataBus{
	def create(input_topic:String, output_topic:String) = new DataBus(input_topic, output_topic)
}


object RpcaOnline {
	def main(args: Array[String])={
		val tz = TimeZone.getTimeZone("America/Sao_Paulo")
		val f = 1440 //every minute of a day
		val T = 28
		val cycle = 1440*7 //seven days of a week
		val window = 60 //sixty seconds
		val spenalty = (30 / math.sqrt(math.max(f,T)))
		//val lpenalty = 1;//val spenalty = 1.4 / sqrt(max(f, T));//val spenalty = 1.4 / sqrt(max(X.size, X.head.size))
		val rpca = new Rpca
		val mf_tags = Array("X","L","S","E")
		val history = Source.fromInputStream(this.getClass.getResourceAsStream("history.txt")).mkString.split(',').map(_.toDouble).toVector
		val bus = DataBus.create("input_topic", "output_topic")
		val buf = ArrayBuffer[String]()
		val clock = new Chronometer(window)
		clock.reset
		bus.source.foreach{js =>
			if (clock.overlap) {
				val qt = buf.length.toDouble//quantidade por min
				val t0 = (System.currentTimeMillis + tz.getRawOffset)/1000
				val i0 = (t0/60 % cycle).toInt
				val X = Rpca.reshape(history.updated(i0, qt).toArray, f, T, "F")
				val res = rpca.fit(X, Spenalty=spenalty)
				val pts = res.map(x => x.get(i0%f, i0/f))
				val mf_elem = (mf_tags zip pts).toMap
				bus.write(mf_elem("X"))
				buf.clear
			}
			buf.append(js)
		}
	}
}
