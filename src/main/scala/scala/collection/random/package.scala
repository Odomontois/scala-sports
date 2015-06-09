/**
 * Author: Odomontois
 * Date  : 22-Apr-15
 * Time  : 19:41
 */
package scala.collection

import scala.collection.immutable.HashSet
import scala.util.Random

package object random {
  implicit class HashSetRandom[T](set: HashSet[T]) {
    def randomElem: Option[T] = set match {
      case trie: HashSet.HashTrieSet[T] => {
        trie.elems(Random.nextInt(trie.elems.length)).randomElem
      }
      case _ => Some(set.size) collect {
        case size if size > 0 => set.iterator.drop(Random.nextInt(size)).next
      }
    }
  }
}
