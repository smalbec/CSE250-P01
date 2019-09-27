/**
 * DataEntryStore.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT:smalbec
 * Person#:50280232
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa1

import cse250.objects.EmbeddedListNode
import util.control.Breaks._

class DataEntryStore[A >: Null <: AnyRef](private val capacity: Int = 1000)
  extends collection.mutable.Seq[A] {

  //array of type  populated by 100 emb list
  private val dataArray: _root_.scala.Array[_root_.cse250.objects.EmbeddedListNode[A]] = Array.fill[EmbeddedListNode[A]](capacity)(new EmbeddedListNode[A])
  private var headIndex = -1
  private var tailIndex = -1
  private var numStored = 0
  var count = 0


  /** Inserts element to tail of list. */
  def insert(elem: A): Unit = {

    //if there are no things stored, add it to the list

    //headIndex is where in the array is the first/oldest element of the list
    //tailIndex is where in the array is the last/newest element of the list

    //numstored is always gonna point

    //first element of the array to be populated
    if (numStored == 0) {
      dataArray(numStored).value = elem
      headIndex = 0
      tailIndex = 0
      numStored += 1
    }

    else {
      // if there's an empty space its gonna populate it
      if (numStored != capacity) {
        // check in every node where NULL is
        breakable(for (i <- 0 until capacity) {
          if (dataArray(i).value == null) {
            //changes null to new value
            dataArray(i).value = elem
            //current node value previous becomes the last tail index
            dataArray(i).prev = tailIndex
            //last tailIndex node next value becomes index of current tail
            dataArray(tailIndex).next = i
            //tail becomes current node index
            tailIndex = i
            numStored += 1
            break
          }
        })

      }
      //now if every node in the array is occupied
      //its going to replace the head value with new node
      //new node is going to be tailIndex and newest element
      //it should update the second oldest entry as the oldest
      else {
        //checks where in the array the head is, since the head.prev is always -1
        breakable(for (j <- 0 until capacity) {


          if (dataArray(j).prev == -1) {
            //once we find head we are going to replace its value with the given elem value
            dataArray(j).value = elem
            //the node.next is the second oldest
            headIndex = dataArray(j).next
            //second oldest (now oldest) has .prev -1
            dataArray(headIndex).prev = -1

            //prev is gonna be the lastIndex before lastIndex becomes current node
            dataArray(j).prev = tailIndex

            dataArray(tailIndex).next = j

            tailIndex = j
            //node will have no next since its newest node
            dataArray(j).next = -1
            //node prev should be

            //and the tail index will become the index where this new node is
            break
          }
          // check if the node had the headIndex as its prev, so we can make it the new head

        })
      }

    }

  }

  /** Removes all copies of the given element. */
  def remove(elem: A): Boolean = {
    // remember to update numstored

    var switch = false

    for (i <- 0 until capacity) {
      if (dataArray(i).value == elem) {
        switch = true
      }
    }
    for (i <- 0 until capacity) {
      if (dataArray(i).value == elem) {
        //changes node value to null
        dataArray(i).value = null

        //if the node is the head
        //change the next node to be new head and headIndex
        if (dataArray(i).prev == -1 && dataArray(i).next != -1) {
          dataArray(dataArray(i).next).prev = -1
          headIndex = dataArray(i).next
        }
        //if the node is the tail
        //change the next node to be the new tail and tailIndex
        else if (dataArray(i).next == -1 && dataArray(i).prev != -1) {

          headIndex = dataArray(i).prev
        }
        else if (dataArray(i).next == -1 && dataArray(i).prev == -1) {
          headIndex = -1
          tailIndex = -1

        }
        //if the node is neither head or tail
        //modify the nodes before and after
        else {
          //the node.next of the .prev of the deleted node is going to become the node index of the deleted node.next
          dataArray(dataArray(i).prev).next = dataArray(i).next
          //the node.prev of the .next of the deleted node is going to become the node index of the deleted node.prev
          dataArray(dataArray(i).next).prev = dataArray(i).prev

        }

        //change the values of the node after we use them to find prev and next to change those too
        dataArray(i).prev = -1
        dataArray(i).next = -1

        //remember to change numstored
        numStored -= 1

      }
    }

    for (i <- 0 until capacity) {
      if (dataArray(i).value == elem) {
        return false
      }
    }
    if (!switch) {
      return false
    }

    return true
  }


  /** Returns the count of nodes containing given entry. */
  def countEntry(entry: A): Int ={

    var count = 0

    for (i <- 0 until capacity) {
      if (dataArray(i).value == entry) {
        count += 1
      }
    }
    count
  }



  /** Gets the element at the specified index. */
  override def apply(idx: Int): A ={
    var returnVal: A = dataArray(0).value
    var head = headIndex
//    for (elem <- iterator) {
//      if (elem == idx) {
//         returnVal = iterator.next
//      }
//    }
    if(iterator == idx){
      returnVal = iterator.next
    }
    returnVal
  }

  /** Replaces element at given index with a new value. */
  override def update(idx: Int, elem: A): Unit = {

      dataArray(idx).value = elem
    }


  /** Returns an Iterator that can be used only once. */
  def iterator: Iterator[A] = new Iterator[A] {
    private var current = headIndex

    override def hasNext: Boolean = current != -1

    override def next(): A = {
      val prev: Int = current
      current = dataArray(current).next
      dataArray(prev).value
    }


  }

  /** Returns the length of the stored list. */
  override def length: Int = numStored

  override def toString: String = if (numStored == 0) "" else this.iterator.addString(new StringBuilder, "\n").result()
}
