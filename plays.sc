import scala.language.higherKinds

trait Record[This <: Record[This]] {
  self =>
  val data: String
}
case class ExampleRecord(data : String) extends Record[ExampleRecord]

trait Store[This <: Store[This, RecordType], RecordType <: Record[RecordType]] {
  self: This =>
  val content: RecordType
  def database: DatabaseService[Store, Record]  // What is the correct type?
  def queryBuilder: QueryService[This, Record] // What is the correct type?
}
trait DatabaseService[StoreKind[St <: StoreKind[St, RecordKind[Rec]], Rec <: RecordKind[Rec]], RecordKind[Rec <: RecordKind[Rec]]]

trait QueryService[StoreKind[St <: Record[St]], RecordKind[Rec <: Record[Rec]]]

case class MyStore[RecordType <: Record[RecordType]](content: RecordType) extends Store[MyStore[RecordType], RecordType] {
  def database = MyStore
  def queryBuilder = MyQueryBuilder
}

object MyStore extends DatabaseService[Store, Record]
object MyQueryBuilder extends QueryService[MyStore,Record]