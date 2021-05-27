
object Hello extends App {
  //Valid Anagram
  def isAnagram(s: String, t: String): Boolean = {
    var string2 = t
    var string = ""
    var output = false
    //proverka length
    if(s.length == t.length) {
      //сменить одинаковые буквы
      for(c <- s) {
        string = string + "1"
        var i = s.indexOf(c)
        string2 = string2.replaceFirst(c.toString,"1")
      }

      //проверка на схожост
      if(string == string2){
        output = true
      }

    }
    else{
      output = false
    }

    output
  }
//  var t = isAnagram("anagram","nagaram")

//  Can Make Arithmetic Progression From Sequence
  def canMakeArithmeticProgression(arr: Array[Int]): Boolean = {
    var output = true
    var i = 1
    // сортируем массив
    var sortedArr = arr.sorted
    //  разнится между первым и вторым элементом
    val a = sortedArr(1) - sortedArr(0)
    while(output && i<arr.length-1){
      if(sortedArr(i+1) - sortedArr(i) == a) {
        output = true
        i = i + 1
      }
      else {
        output = false
      }
    }
    output
  }
//  val t = canMakeArithmeticProgression(Array(3,5,1))

  //Largest Perimeter Triangle
  def largestPerimeter(nums: Array[Int]): Int = {
    //сортировка для того чтобы найти максимальную периметр
    var sortedNums = nums.sorted
    var maxPerimeter = 0
    for (num <- 0 to sortedNums.length-3) {
      //проверка привильный триугольник и больше чем предедущий максимум
      if(sortedNums(num) + sortedNums(num+1)>sortedNums(num+2) &&
        (sortedNums(num) + sortedNums(num+1) +sortedNums(num+2) > maxPerimeter)){
        maxPerimeter = sortedNums(num) + sortedNums(num+1) +sortedNums(num+2)
      }
    }
    maxPerimeter
  }
//  val t = largestPerimeter(Array(3,2,3,4))

  //Intersection of Two Arrays
  def intersection(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
    //create new list
    val builder = List.newBuilder[Int]
    //take short array
    if(nums2.length < nums1.length) {
      //sorting array with unique nums
      var sortedNums = nums2.sorted.distinct
      for(i <-sortedNums) {
        //if it has on the next array we add it to list
        if(nums1.contains(i)) {
          builder += i
        }
      }
    }
    else{
      val sortedNums1 = nums1.sorted.distinct
      for(i <-sortedNums1) {
        if(nums2.contains(i)) {
          builder += i
        }
      }
    }
    //converting list to array
    val output = builder.result().toArray
    output
  }
//  var t = intersection(Array(1,2,2,3), Array(1,2,2,4,6,63))

  //Maximum Units on a Truck
  def maximumUnits(boxTypes: Array[Array[Int]], truckSize: Int): Int = {
    var output = 0
    var count = 0
    var isOk = true
    //sorting to add the big boxes
    var sortedBox = boxTypes.sortBy(_(1))
    var i = sortedBox.length-1

    //adding while is ok
    while (isOk && i>=0){
      count = count + sortedBox(i)(0)
      if(count<truckSize){
        var temp = sortedBox(i)(0)*sortedBox(i)(1)
        output = output + temp
        i = i - 1
      }
      else{
        var dif = count - truckSize
        var temp = (sortedBox(i)(0)-dif) * sortedBox(i)(1)
        output = output + temp
        i = i - 1
        isOk = false
      }
    }
    output
  }
//  var t = maximumUnits(Array(Array(2,5),Array(5,10),Array(4,7),Array(3,9)),10)


  //increasing Decreasing String
  def sortString(s: String): String = {
    var sortedString = s.sorted
    val builder = List.newBuilder[Int]
    var string = s.sorted.distinct
    //counting each element
    for(i <- string) {
      var count = 0
      for(t<- sortedString) {
        if(i == t) {
          count = count + 1
        }
      }
      builder += count
    }
//    println(string)
    var counts = builder.result().toArray

    var output = ""
    var isOk = true
    var reverse = false
    //adding until it has equal length with s
    while (output.length  < s.length) {
      //add in abs
      if(!reverse) {
        //adding each element by count if he has count more than 0
        for( i <- 0 to string.length-1) {
          if(counts(i) > 0) {
            output += string(i)
            counts.update(i,(counts(i)-1))
            println(string(i) + " " + counts(i))
          }
        }
        println("reverseIsnot" + output)
        reverse = true
      }
      //add in desc
      else {
        //adding each element by count if he has count more than 0
        for( i <- string.length-1 to 0 by -1) {
          if(counts(i) > 0) {
            output += string(i)
            counts.update(i,(counts(i)-1))
          }
        }
        println("reverse" + output)
        reverse = false
      }
    }

    output
  }

  var s = sortString("leetcode")
  println(s)

//  leetcode
//  cdelottoled
//  leetcode
//  cdelotee
}
