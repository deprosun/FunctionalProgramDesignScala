import scala.util.Random

trait Generator[+T] {
  self =>

  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate).generate
  }
}

val integers = new Generator[Int] {
  val random = new Random()

  override def generate: Int = random.nextInt()
}

val booleans = integers map (x => x > 0)


val pairs = integers flatMap { x =>
  integers map { y =>
    (x, y)
  }
}

def lists = for {
  isEmpty <- booleans
  list <- if (isEmpty)
}
