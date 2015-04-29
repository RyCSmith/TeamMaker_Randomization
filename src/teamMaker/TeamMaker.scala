package teamMaker

import scala.util.control.Breaks._
import scala.util.Random
import scala.collection.mutable.Buffer
import scala.collection.mutable

object TeamMaker {
  
  //sets aliases for types uses in this program
  type Team = Tuple2[Int, Int]
  type Teams = List[Team]
  
  /**
   * Main method. Runs the program.
   */
  def main(args: Array[String]): Unit = {
    //get num of students and assignments
    val numStudents = getNumStudents();
    val numAssignments = getNumAssignments();
    
    //generate teams and print
    val teams = generateTeams(numStudents, numAssignments)
    var assignment = 1
    for (x <- teams){
      printf("Assignment %d: ", assignment)
      assignment = assignment + 1
      for (y <- x){
        printf("%10s", y)
      }
      println("")
    }  
    
    //print the check to show that there are no duplicates
    println(check(teams, numStudents))
  }
  
  /**
   * Asks the user for the number of students and repeats until an acceptable number is received. Returns the number.
   * @return numStudents - the number of students
   */
  def getNumStudents(): Int = {
    var numStudents = -1
    breakable {
      while(true){
        try{
         println("Please enter the number of students in this course: ") 
         numStudents = readInt()
         if (numStudents < 20 || numStudents > 40)
           println("Class capacity must be between 20 and 40.")
         else if (numStudents % 2 != 0)
           println("The number of students must be even.");
         else
           break
        }catch {
          case e: NumberFormatException => println("Improper Format")
        }
      }
    }
    numStudents
  }
  
  /**
   * Asks the user for the number of assignments and repeats until an acceptable number is received.
   * @return numAssignments - the number of assignments 
   */
  def getNumAssignments(): Int = {
    var numAssignments = -1
    breakable {
      while(true){
        try{
         println("Please enter the number of assignments this semester: ") 
         numAssignments = readInt()
         if (numAssignments < 8 || numAssignments > 12)
           println("Number of assignments must be between 8 and 12.")
         else
           break
         }catch {
          case e: NumberFormatException => println("Improper Format")
        }
      }
    }
    numAssignments
  }
  
  /**
   * Generates pairs of students for each assignment using random pairing scheme.
   * @param numStudents - the number of students in the class
   * @param numAssignments - the number of assignments this semester
   * @return List[Teams] - list of teams for each assignment 
   */
  def generateTeams(numStudents: Int, numAssignments: Int): List[Teams] = {
    var teamList = List[Teams]()
    var assignment = 0
    for (assignment <- 0 to numAssignments - 1){ //loop through to make a team for each assignment
      var singleTeam = makeRandomTeam(numStudents) // make a randomly generated team and add to list
      teamList = singleTeam::teamList
      
      while (!check(teamList, numStudents).isEmpty){ //if list fails check with new team, enter loop
        val returnList = check(teamList, numStudents).toList(0) // get list of bad pairs and remove the team from the list
        val badTeam = teamList.head 
        teamList = teamList.drop(1)
        val newTeam = makeSwaps(badTeam, returnList) //make swaps and add the returned team to the list. loop to check again
        teamList = newTeam::teamList
      }
    }
    teamList
  }

  /**
   * Takes a list with invalid pairs and returns a List with those pairs replaced with randomly chosen swaps.
   * @param headList - the List containing pairs that have worked together previously
   * @param returnList - the List indicating which pairs are invalid
   * @return headArray - List[Team] with the invalid pairing randomly replaced
   */
  def makeSwaps(headList: List[Team], returnList: List[Team]): List[Team] ={
    var headArray = headList.toArray
    for (badPair <- returnList){
      var index = headArray.indexOf(badPair) //get index of the bad pair in the array, if -1, switch order and try again
      if (index == -1)
        index = headArray.indexOf((badPair._2, badPair._1))
      if (index != -1){ //if index == -1 at this point the pair was already eliminated in a previous swap
        val random = Random.nextInt(headArray.length)
        val newPair1 = (headArray(index)._1, headArray(random)._2)
        val newPair2 = (headArray(index)._2, headArray(random)._1)
        headArray(index) = newPair1
        headArray(random) = newPair2
      }
    }
    headArray.toList
  }

  /**
   * Creates a list of randomly selected pairs for the given number of students.
   * @param - the number of students in the class
   * @return teams - List[Team] of randomly generated sets of partners
   */
  def makeRandomTeam(numStudents: Int): List[Team] = {
    var teams = List[Team]()
    //creates a Buffer containing all an ID for each student
    val nums = 0 to numStudents - 1
    var students: Buffer[Int] = Buffer()
    students.appendAll(nums)

    while (teams.length < (numStudents / 2)){
      //create random pairs removing from the Buffer to avoid self pairs and repeats
      val randomIndex1 = Random.nextInt(students.length)
      val student1 = students(randomIndex1)
      students.remove(randomIndex1)
      val randomIndex2 = Random.nextInt(students.length)
      val student2 = students(randomIndex2)
      students.remove(randomIndex2)
      //add to the list of teams
      teams = (student1, student2)::teams
    }
    teams
  }
  
  /**
   * Verifies that no student pair occurs more than once over the course of all assignments. Returns a list of issues if so.
   * @param teams - List[Teams] the list of teams for all assignments
   * @param numStudents - the number of students in the course
   * @return returnList - list of teams that occur more than once over the duration of the class 
   */
  def check(teams: List[Teams], numStudents: Int): Option[List[Team]] = {
     var pairsMap = scala.collection.mutable.Map[Int,List[Int]]() //create map to hold tuples of pairs that have been assigned
     var studentNum = 0
     for (studentNum <- 0 to numStudents - 1){
       pairsMap += (studentNum -> List[Int]())
     }
     pairsMap += (1 -> List[Int]())
    var returnList = List[Team]() //list to hold pairs that have been assigned to eachother more than once
    
    for (assignment <- teams){ //loop through all assignments
      var z = 0
      for (z <- 0 to assignment.length - 1){ //loop through all pairs for the current assignment
        
        val tuple = assignment(z) //this is the pair we are currently viewing
        val pastPartners = pairsMap(tuple._1) //list all all partners member 1 of the pair has had previously
        
        var repeat = false
        for (partner <- pastPartners){ //loop through all past partners, report a hit if encountered
          if (partner == tuple._2)
            repeat = true
        }
        
        if (repeat){ //if hit was encountered, all the pair to the returnList
          returnList = (tuple._1, tuple._2)::returnList
        }
        else{ //otherwise update the list of past partners for each
          pairsMap(tuple._1) = tuple._2::pairsMap(tuple._1)
          pairsMap(tuple._2) = tuple._1::pairsMap(tuple._2)
        }
      }
    }
    if (returnList.size > 0)
      Option(returnList) 
    else
      None   
  }
}