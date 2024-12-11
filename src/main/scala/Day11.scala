object Day11 {

  type Depth      = Int
  type Stone      = Long
  type StoneCount = Long

  type Cache = Map[(Stone, Depth), StoneCount]

  def depthRecursion(number: Stone, depth: Depth, cache: Cache): (StoneCount, Cache) = {
    if (depth == 0) (1L, cache)
    else {
      val cached = cache.get((number, depth))
      cached match {
        case Some(count) => (count, cache)
        case None =>
          val (result, subCache) = number match {
            case 0 => depthRecursion(1L, depth - 1, cache)
            case n if n.toString.length % 2 == 0 => stringSplit(n, depth, cache)
            case _ => depthRecursion(number * 2024L, depth - 1, cache)
          }
          val updatedCache = updateCache(subCache, number, depth, result)
          (result, updatedCache)
      }
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

  inline def updateCache(cache: Cache, stone: Stone, depth: Depth, count: StoneCount) = {
    if (stone < 20000 && depth > 4)
      cache + ((stone, depth) -> count)
    else {
      cache
    }
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
    val (result, newCache) = calcWithCache(testIn, 25, Map.empty)
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
