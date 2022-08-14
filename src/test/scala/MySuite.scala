import MyList._
import MyList.*

class MySuite extends munit.FunSuite {
  test("Distinct of MyNil") {
    val expected = MyNil
    val actual = distinct(MyNil)
    assertEquals(actual, expected)
  }
  test("Distinct of same items") {
    val expected = MyList(0)
    val actual = distinct(MyList(0,0,0,0,0,0,0,0))
    assertEquals(actual, expected)
  }
  test("Distinct default numbers, items don't have duplicates") {
    val expected = MyList(1,2,3,4,5,6,7,8)
    val actual = distinct(MyList(1,2,3,4,5,6,7,8))
    assertEquals(actual, expected)
  } 
   test("Distinct default numbers, items have duplicates") {
    val expected = MyList(1,2,3,4,5,6,7,8)
    val actual = distinct(MyList(1,1,2,3,3,4,5,6,7,8))
    assertEquals(actual, expected)
  } 
   test("mkString MyNil") {
    val expected = ""
    val actual = mkString(MyNil)
    assertEquals(actual, expected)
  }
   test("mkString MyNil+pref+sep+postf") {
    val expected = "[]"
    val actual = mkString(MyNil,"[",",","]")
    assertEquals(actual, expected)
  }
   test("mkString MyList(1)+pref+sep+postf") {
    val expected = "[1]"
    val actual = mkString(MyList(1),"[",",","]")
    assertEquals(actual, expected)
  }
   test("mkString MyList(1,2,3,4,5)+pref+sep+postf") {
    val expected = "[1,2,3,4,5]"
    val actual = mkString(MyList(1,2,3,4,5),"[",",","]")
    assertEquals(actual, expected)
  }
   test("elementAt MyNil at pos 0") {
    val expected = None
    val actual = elementAt(MyNil,0)
    assertEquals(actual, expected)
  }
   test("elementAt MyList(1,2,3,4,5) at pos 0") {
    val expected = Some(1)
    val actual = elementAt(MyList(1,2,3,4,5),0)
    assertEquals(actual, expected)
  }    
    test("elementAt MyList(1,2,3,4,5) at pos 4") {
    val expected = Some(5)
    val actual = elementAt(MyList(1,2,3,4,5),4)
    assertEquals(actual, expected)
  }  
   test("elementAt MyList(1,2,3,4,5) at pos 10") {
    val expected = None
    val actual = elementAt(MyList(1,2,3,4,5),10)
    assertEquals(actual, expected)
  }   
}