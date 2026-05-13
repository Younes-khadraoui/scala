package tp01.multiSet

/**
 * Une classe modélisant les multi-ensembles.
 * Un élément peut apparaitre plusieurs fois dans un multi-ensemble. On représentera
 *  donc ceux-ci à l'aide d'associations (Maps) dont les clés sont les éléments et
 *  les valeurs, leur nombre d'occurrences.
 * Par exemple, le multiset {e1, e2, e1, e3, e1, e3} sera représenté par
 *  l'association {e1 -> 3, e2 -> 1, e3 -> 2}.
 */
class MultiSet[E](val elems: Map[E, Int]):
  /**
   * Le nombre d'éléments 2 à 2 distincts dans le multiset, i.e. le nombre de clés
   *  de l'association.
   */
  def size = elems.size

  /**
   * Le nombre total d'éléments du multiset.
   */
  def card: Int = elems.values.sum

  /**
   * "e" est-il présent dans le multiset ?
   */
  def mem(e: E) = elems.contains(e)

  /**
   * Le nombre d'occurrences de "e" dans le multiset.
   */
  def count(e: E) = elems.getOrElse(e, 0)

  /**
   * "this" est-il sous-ensemble de "that" ?
   */
  def subsetOf(that: MultiSet[E]) =
    elems.forall((e, n) => n <= that.count(e))

  /**
   * Produit un nouveau multi-ensemble à partir de "this" auquel on ajoute "n"
   *  occurrences de l'élément "e".
   */
  def add(e: E, n: Int) = MultiSet(elems + (e -> (count(e) + n)))

  /**
   * Produit un nouveau multi-ensemble à partir de "this" dont on supprime "n"
   *  occurrences de l'élément "e".
   * Il ne reste aucune occurrence de "e" si "n" est supérieur ou égal à
   *  this.count(e)
   */
  def remove(e: E, n: Int) =
    val remaining = count(e) - n
    if remaining <= 0 then MultiSet(elems - e)
    else MultiSet(elems + (e -> remaining))

  /**
   * Produit un nouveau multi-ensemble union de "this" et "that".
   */
  def union(that: MultiSet[E]) =
    val keys = elems.keySet ++ that.elems.keySet
    MultiSet(keys.map(e => e -> (count(e) + that.count(e))).toMap)

  /**
   * Produit un nouveau multi-ensemble soustraction de "that" à "this".
   */
  def diff(that: MultiSet[E]) =
    elems.foldLeft(MultiSet(Map.empty[E, Int])):
      (acc, kv) =>
        val remaining = kv._2 - that.count(kv._1)
        if remaining > 0 then acc.add(kv._1, remaining)
        else acc

  /**
   * Produit un nouveau multi-ensemble maximum de "this" et "that".
   * Le nombre d'occurrences d'un élément du maximum est le maximum des nombres
   *  d'occurrences de cet élément dans "this" et "that".
   */
  def maximum(that: MultiSet[E]) =
    val keys = elems.keySet ++ that.elems.keySet
    MultiSet(keys.map(e => e -> math.max(count(e), that.count(e))).toMap)

  /**
   * Produit un nouveau multi-ensemble intersection de "this" et "that".
   */
  def inter(that: MultiSet[E]) =
    val keys = elems.keySet.intersect(that.elems.keySet)
    MultiSet(keys.map(e => e -> math.min(count(e), that.count(e))).toMap)

  /**
   * L'égalité de multi-ensembles basée sur l'égalité ensembliste.
   */
  override def equals(that: Any) = that match
    case m: MultiSet[?] => elems == m.elems
    case _              => false

  /**
   * result == elems.hashCode()
   */
  override def hashCode() = elems.hashCode()

  /**
   * result == "MultiSet" + "(" + e1 + "->" + n1 + ", " + ... + ek + "->" + nk + ")"
   */
  override def toString =
    val content = elems.map((e, n) => s"$e -> $n").mkString(", ")
    s"MultiSet($content)"
end MultiSet

object MultiSet:
  def apply[E](elems: Map[E, Int]): MultiSet[E] = new MultiSet(elems)
