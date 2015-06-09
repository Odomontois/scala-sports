/**
 * Author: Oleg Nizhnik
 * Date  : 08.06.2015
 * Time  : 10:18
 */
package stackoverflow

object CompReflection {
  class CC {
    def CC () = {
    }
  }
  object CC {
    def getC(name : String) : CC = {
      return new CC();
    }
  }

}
