def takeWhile_1(f: A => Boolean): LazyList[A] =
  foldRight(empty[A])((h,t) => 
    if (f(h)) cons(h,t)
    else      empty)