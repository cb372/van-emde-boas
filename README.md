This is an implementation of a [van Emde Boas
tree](https://en.wikipedia.org/wiki/Van_Emde_Boas_tree) in Scala.

vEB is a magical tree that supports a variety of operations in `O(lg lg n)` time
or better.

In practice, however, the performance is not as good as you would expect, partly
because the data structure's poor spatial locality of reference means it doesn't
play well with CPU caches.

This implementation is based on chapter 20 of Introduction to Algorithms, third
edition.
