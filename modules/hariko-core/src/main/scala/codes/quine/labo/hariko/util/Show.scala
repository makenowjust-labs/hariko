package codes.quine.labo.hariko
package util

import data.Tree

/** Utility functions for showing values.
  */
object Show {

  /** Shows generated tree value for debugging.
    *
    * {{{
    * scala> import codes.quine.labo.hariko._
    * scala> val t = Gen.int(Range.linear(0, 10)).run(Random(0), Param(0), 100)._2
    * scala> Show.tree(t, 3)
    * res0: String =
    * 9
    * - 5
    *   - 3
    *     - ...
    *   - 4
    *     - ...
    * - 7
    *   - 4
    *     - ...
    *   - 6
    *     - ...
    * - 8
    *   - 4
    *     - ...
    *   - 6
    *     - ...
    *   - 7
    *     - ...
    * }}}
    */
  def tree[T](tree: Tree[Option[T]], maxDepth: Int): String = {
    def option(value: Option[T]): String =
      value.fold("<discard>")(any(_))

    def prefix(depth: Int): String =
      depth match {
        case 0 => ""
        case 1 => "- "
        case n => "  " * (n - 1) + "- "
      }

    def loop(tree: Tree[Option[T]], depth: Int): List[String] = {
      val value = prefix(depth) + option(tree.value)
      val children =
        if (depth + 1 >= maxDepth) if (tree.children.isEmpty) List.empty else List(prefix(depth + 1) + "...")
        else tree.children.flatMap(loop(_, depth + 1)).toList
      List(value) ++ children
    }

    loop(tree, 0).mkString("\n")
  }

  /** Shows the value as human-readable format as possible.
    *
    * Currently it makes string and char values format better than `toString`.
    */
  def any(value: Any): String =
    value match {
      case s: String => string(s)
      case c: Char   => char(c)
      case v         => v.toString()
    }

  private def string(s: String): String =
    "\"" + s.toList.map(charInternal).mkString + "\""

  private def char(c: Char): String =
    "\'" + charInternal(c) + "\'"

  private def charInternal(c: Char): String =
    c match {
      case '\b'             => "\\b"
      case '\f'             => "\\f"
      case '\n'             => "\\n"
      case '\r'             => "\\r"
      case '\t'             => "\\t"
      case '\\'             => "\\\\"
      case '\"'             => "\\\""
      case '\''             => "\\'"
      case c if c.isControl => f"\\u$c%04x"
      case c                => c.toString
    }
}
