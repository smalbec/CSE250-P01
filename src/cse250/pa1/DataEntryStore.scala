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

  //array of type emb list of capacity 100 populated by emb list
  private val dataArray = Array.fill[EmbeddedListNode[A]](capacity)(new EmbeddedListNode[A])
  private var headIndex = -1
  private var tailIndex = -1
  private var numStored = 0
  var count = 0

  /** Inserts element to tail of list. */
  def insert(elem: A): Unit = {

    //TODO: null stuff, iterator maybe,

      //if there are no things stored, add it to the list

    //headIndex is where in the array is the first element of the list
    //listIndex is where in the array is the last element of the list

    if(numStored == 0){
      dataArray(numStored).value = elem
      headIndex = 0
      tailIndex = 0
    }
    else{

      if(dataArray(numStored).value == null){
        dataArray(numStored).value = elem
      }
    }
    numStored += 1
  }

    for(i <- 0 to capacity - 1){
      //
      //      if(dataArray(i).value == null){
      //        // if empty gonna change the value
      //        dataArray(i).value = elem
      //        if(dataArray(i-1) != null){
      //          dataArray(i).prev = i
      //        }
      //      }
      //
      //    }

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
  override def apply(idx: Int): A ={ }

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
