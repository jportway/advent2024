object Day11 {

  type Depth      = Int
  type Stone      = Long
  type StoneCount = Long

  case class Cache(depthSize: Int, stoneSize: Int, private val depthCaches: Vector[Vector[StoneCount]]) {

    def getOrUpdate(depth: Depth, stone: Stone)(updateFunc: (Cache) => (StoneCount, Cache)): (StoneCount, Cache) = {
      if depth == 0 then (1L, this)
      else {
        val stoneIndex = stone.toInt
        if (stone.isValidInt & stoneIndex < stoneSize) {
          val cached = depthCaches(depth)(stoneIndex)
          if cached > 0 then (cached, this)
          else {
            val (calculatedVal, newCache) = updateFunc(this)
            val depthRow                  = depthCaches(depth).updated(stoneIndex, calculatedVal)
            val newDepthCache             = newCache.depthCaches.updated(depth, depthRow)
            (calculatedVal, Cache(depthSize, stoneSize, newDepthCache))
          }
        } else { // out of range for caching
          updateFunc(this)
        }
      }
    }

  }

  object Cache {

    def apply(depthSize: Int, stoneSize: Int): Cache = {
      val depthCaches = Vector.fill(depthSize + 1)(Vector.fill(stoneSize + 1)(0L))
      new Cache(depthSize, stoneSize, depthCaches)
    }

  }

  def depthRecursion(stone: Stone, depth: Depth, cache: Cache): (StoneCount, Cache) = {
    cache.getOrUpdate(depth, stone) { cache =>
      if stone == 0 then depthRecursion(1L, depth - 1, cache)
      else if stone.toString.length % 2 == 0 then stringSplit(stone, depth, cache)
      else depthRecursion(stone * 2024L, depth - 1, cache)
    }
  }

  inline def stringSplit(stone: Stone, depth: Depth, cache: Cache): (StoneCount, Cache) = {
    val str                  = stone.toString
    val (as, bs)             = str.splitAt(str.length / 2)
    val (a, b)               = (as.toLong, bs.toLong)
    val (resultA, subCacheA) = depthRecursion(a, depth - 1, cache)
    val (resultB, subCacheB) = depthRecursion(b, depth - 1, subCacheA)
    val totalResult          = resultA + resultB
    (totalResult, subCacheB)
  }

  def calcWithCache(input: List[Stone], depth: Depth, cache: Cache): (StoneCount, Cache) = {
    input.foldLeft((0L, cache)) { case ((acc, cache), i) =>
      val (result, newCache) = depthRecursion(i, depth, cache)
      (acc + result, newCache)
    }
  }

  @main
  def Day11Main(): Unit = {
    val testIn             = List(125L, 17L)
    val (result, newCache) = calcWithCache(testIn, 25, Cache.apply(75, 6000))
    println(result)

    val input = List(7725L, 185L, 2L, 132869L, 0L, 1840437L, 62L, 26310L)

    val (resultA, newCache2) = calcWithCache(input, 25, newCache)
    println(resultA)

    val start        = System.currentTimeMillis()
    val (resultB, _) = calcWithCache(input, 75, newCache2)
    val end          = System.currentTimeMillis()
    println(resultB)
    println(s"time : ${end - start}")

  }

}
