package tp01.multiSet

/**
 * Une classe modélisant les multi-ensembles.
 * Un élément peut apparaitre plusieurs fois dans un multi-ensemble. On représentera
 * donc ceux-ci à l'aide d'associations (Maps) dont les clés sont les éléments et
 * les valeurs, leur nombre d'occurrences.
 * Par exemple, le multiset {e1, e2, e1, e3, e1, e3} sera représenté par
 * l'association {e1 -> 3, e2 -> 1, e3 -> 2}.
 */
class MultiSet[E](val elems: Map[E, Int]):

  /**
   * Le nombre d'éléments 2 à 2 distincts dans le multiset.
   */
  def size: Int = elems.size

  /**
   * La cardinalité totale (somme des occurrences).
   */
  def card: Int = elems.values.sum

  /**
   * "e" est-il présent dans le multiset ?
   */
  def mem(e: E): Boolean = elems.contains(e)

  /**
   * Le nombre d'occurrences de "e" dans le multiset.
   */
  def count(e: E): Int = elems.getOrElse(e, 0)

  /**
   * "this" est-il sous-ensemble de "that" ?
   */
  def subsetOf(that: MultiSet[E]): Boolean =
    elems.forall { case (e, n) => that.count(e) >= n }

  /**
   * Produit un nouveau multi-ensemble à partir de "this"
   * auquel on ajoute "n" occurrences de l'élément "e".
   */
  def add(e: E, n: Int): MultiSet[E] =
    new MultiSet(elems.updated(e, count(e) + n))

  /**
   * Produit un nouveau multi-ensemble à partir de "this"
   * dont on supprime "n" occurrences de l'élément "e".
   */
  def remove(e: E, n: Int): MultiSet[E] =
    val remaining = count(e) - n
    if remaining > 0 then
      new MultiSet(elems.updated(e, remaining))
    else
      new MultiSet(elems - e)

  /**
   * Produit un nouveau multi-ensemble union de "this" et "that".
   * (addition des occurrences)
   */
  def union(that: MultiSet[E]): MultiSet[E] =
    new MultiSet(
      that.elems.foldLeft(elems) {
        case (acc, (e, n)) =>
          acc.updated(e, acc.getOrElse(e, 0) + n)
      }
    )

  /**
   * Produit un nouveau multi-ensemble soustraction de "that" à "this".
   */
  def diff(that: MultiSet[E]): MultiSet[E] =
    that.elems.foldLeft(this) {
      case (acc, (e, n)) => acc.remove(e, n)
    }

  /**
   * Produit un nouveau multi-ensemble maximum de "this" et "that".
   */
  def maximum(that: MultiSet[E]): MultiSet[E] =
    new MultiSet(
      (elems.keySet ++ that.elems.keySet).map { e =>
        e -> math.max(this.count(e), that.count(e))
      }.toMap
    )

  /**
   * Produit un nouveau multi-ensemble intersection de "this" et "that".
   */
  def inter(that: MultiSet[E]): MultiSet[E] =
    new MultiSet(
      elems.collect {
        case (e, n) if that.mem(e) =>
          e -> math.min(n, that.count(e))
      }
    )

  /**
   * L'égalité de multi-ensembles basée sur l'égalité ensembliste.
   */
  override def equals(that: Any): Boolean = that match
    case ms: MultiSet[?] => this.elems == ms.elems
    case _               => false

  /**
   * result == elems.hashCode()
   */
  override def hashCode(): Int =
    elems.hashCode()

  /**
   * result == "MultiSet(e1->n1, ..., ek->nk)"
   */
  override def toString: String =
    "MultiSet(" +
      elems.map { case (e, n) => s"$e -> $n" }.mkString(", ") +
      ")"

end MultiSet
