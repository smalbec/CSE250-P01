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

class DataEntryStore[A >: Null <: AnyRef](private val capacity: Int = 100)
  extends collection.mutable.Seq[A] {

  //array of type  populated by 100 emb list
  private val dataArray = Array.fill[EmbeddedListNode[A]](capacity)(new EmbeddedListNode[A])
  private var headIndex = -1
  private var tailIndex = -1
  private var numStored = 0
  var count = 0

  /** Inserts element to tail of list. */
  def insert(elem: A): Unit = {

    //TODO: null stuff, iterator maybe,

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
      if (dataArray(numStored).value == null) {
        dataArray(numStored).value = elem
        //node.prev becomes the index of the latest pushed node
        dataArray(numStored).prev = tailIndex

        //checks where in the array is the current element and marks it it newest index
        for (i <- 0 until capacity - 1) {
          if (dataArray(i).value == elem) {
            tailIndex = i

          }
        }
        //previous node.next will become the newest tail index (it points where in the array is the current node)

        dataArray(numStored - 1).next = tailIndex

        /** think about modifying before and after nodes */

        numStored += 1
      }
      //now if the node in the array is not occupied
      //its going to replace the head value with new elem
      //new elem is going to be tailIndex and newest element
      //it should update the second oldest entry as the oldest

      //TODO: check if its updating the right values
      else {
        //checks where in the array the head is, since the head.prev is always -1
        for (j <- 0 until capacity - 1) {
          if (dataArray(j).prev == -1) {
            //once we find head we are going to replace its value with the given elem value
            dataArray(j).value = elem
            //the node.prev will become the latest
            dataArray(j).prev = tailIndex
            tailIndex = j

            //how to find second oldest?
            // e3 -> e4 -> e5
            // remove e3 and e4 and how do you know e5 is the oldest?
          }
          //second oldest is going to be where the node.prev is the head index
          //headIndex is going to become index of second oldest
          if(dataArray(j).prev == headIndex){
            headIndex = j
          }
        }
      }

    }

  }

  /** Removes all copies of the given element. */
  def remove(elem: A): Boolean = {
    //when null the prev and next should always be -1
    // remember to update numstored
    return false
  }

  /** Returns the count of nodes containing given entry. */
  def countEntry(entry: A): Int = return 0

  /** Gets the element at the specified index. */
  override def apply(idx: Int): A ={}

  /** Replaces element at given index with a new value. */
  override def update(idx: Int, elem: A): Unit = {}

  /** Returns an Iterator that can be used only once. */
  def iterator: Iterator[A] = new Iterator[A] {
    private var current = headIndex

    override def hasNext: Boolean = current != -1

    override def next(): A = {
      val prev = current
      current = dataArray(current).next
      dataArray(prev).value
    }


  }

  /** Returns the length of the stored list. */
  override def length: Int = numStored

  override def toString: String = if (numStored == 0) "" else this.iterator.addString(new StringBuilder, "\n").result()
}
