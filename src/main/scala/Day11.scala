object Day11 {

  case class Cache(cache: Map[(Long, Int), Long] = Map.empty, rule: (Long, Int) => Boolean) {

    def get(key: (Long, Int))      = cache.get(key)
    def merge(other: Cache): Cache = this.copy(cache = cache ++ other.cache)
    def update(key: (Long, Int), value: Long): Cache =
      if (rule(key._1, key._2)) {
        this.copy(cache = cache + (key -> value))
      } else {
        this
      }

  }

  def depthRecursion(digit: Long, depth: Int, cache: Cache): (Long, Cache) = {
    if (depth == 0) (1L, cache)
    else {
      val cached = cache.get((digit, depth))
      cached.map(x => (x, cache)).getOrElse {
        val (result, subCache) = if (digit == 0) {
          depthRecursion(1L, depth - 1, cache)
        } else {
          val str = digit.toString
          val len = str.length
          if (len % 2 == 0) {
            val (as, bs)             = str.splitAt(len / 2)
            val (a, b)               = (as.toLong, bs.toLong)
            val (resultA, subCacheA) = depthRecursion(a, depth - 1, cache)
            val (resultB, subCacheB) = depthRecursion(b, depth - 1, subCacheA)
            val totalResult          = resultA + resultB
            (totalResult, subCacheB)
          } else {
            depthRecursion(digit * 2024L, depth - 1, cache)
          }
        }
        (result, subCache.update((digit, depth), result))
      }
    }
  }

  def calcWithCache(input: List[Long], depth: Int, cache: Cache): (Long, Cache) = {
    input.foldLeft((0L, cache)) { case ((acc, cache), i) =>
      val (result, newCache) = depthRecursion(i, depth, cache)
      println("done1")
      (acc + result, newCache)
    }
  }

  @main
  def Day11Main(): Unit = {
    val testIn             = List(125L, 17L)
    val cacheRule          = (v: Long, d: Int) => v < 100000 & d > 3
    val cacheTest          = Cache(rule = cacheRule)
    val (result, newCache) = calcWithCache(testIn, 25, cacheTest)
    println(result)

    val input                = List(7725L, 185L, 2L, 132869L, 0L, 1840437L, 62L, 26310L)
    val (resultA, newCache2) = calcWithCache(input, 25, newCache)
    println(resultA)

    val (resultB, _) = calcWithCache(input, 75, newCache2)
    println(resultB)

  }

}
