/*
The last element of `tails` is always the empty `Stream`, so we handle this as a special case, by appending it to the output.
*/
def tails: LazyList[LazyList[A]] =
  unfold(this) {
    case Empty => None
    case s => Some((s, s drop 1))
  } append LazyList(empty)
