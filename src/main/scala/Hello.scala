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



}
