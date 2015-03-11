package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times is good"){
    assert(times(List('a','a','b','c','d','b','a','b')).contains(('a',3)))
    assert(times(List('a','a','b','c','d','b','a','b')).contains(('b',3)))
    assert(times(List('a','a','b','c','d','b','a','b')).contains(('c',1)))
    assert(times(List('a','a','b','c','d','b','a','b')).contains(('d',1)))
    assert(times(List()).size === 0)
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }


  test("french code"){
    assert(decodedSecret === string2Chars("huffmanestcool"))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      assert(decode(t2, encode(t2)("adb".toList)) === "adb".toList)
      assert(decode(frenchCode, encode(frenchCode)("huffmanestcool".toList)) === "huffmanestcool".toList)
    }
  }

  test("codetable"){
    val table = convert(createCodeTree(string2Chars("ABBBACDEFGHAAAAAA".toLowerCase())))
    assert(table.contains('a',List(0)))
    assert(table.contains('d',List(1,0,0,0)))
    assert(table.contains('c',List(1,0,0,1)))
    assert(table.contains('f',List(1,0,1,0)))
    assert(table.contains('e',List(1,0,1,1)))
    assert(table.contains('h',List(1,1,0,0)))
    assert(table.contains('g',List(1,1,0,1)))
    assert(table.contains('b',List(1,1,1)))
  }


  test("decode and encode french code with quickEncode"){
    assert(quickEncode(frenchCode)(string2Chars("huffmanestcool")) === secret)
  }
}
