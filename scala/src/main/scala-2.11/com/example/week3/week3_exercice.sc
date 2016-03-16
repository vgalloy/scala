/**
  * @author Vincent Galloy
  *         Created by Vincent Galloy on 28/12/15.
  */

class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "(User: " + user + "\n" +
      "Text: " + text + " [" + retweets + "])"
}

trait TweetList {
  def isEmpty: Boolean

  def head: Tweet

  def tail: TweetList
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty: Boolean = false

  override def toString: String = "[" + head + "," + tail + "]"
}

class NilList extends TweetList {
  override def isEmpty: Boolean = true

  override def head: Tweet = throw new ArrayIndexOutOfBoundsException("Nil.head")

  override def tail: TweetList = throw new ArrayIndexOutOfBoundsException("Nil.tail")

  override def toString: String = "[Nil]"
}

abstract class TweetSet {
  def contains(x: Tweet): Boolean

  def incl(x: Tweet): TweetSet

  def merge(other: TweetSet): TweetSet

  def filter(p: Tweet => Boolean): TweetSet

  def union(that: TweetSet): TweetSet

  def remove(tweet: Tweet): TweetSet

  def max(): Tweet

  def descendingByRetweet: TweetList
}

class Empty extends TweetSet {
  def contains(x: Tweet): Boolean = false

  def incl(x: Tweet): TweetSet = new NonEmpty(x, new Empty, new Empty)

  def merge(other: TweetSet): TweetSet = other

  def filter(p: Tweet => Boolean): TweetSet = new Empty

  def union(that: TweetSet): TweetSet = new Empty

  def remove(tweet: Tweet): TweetSet = new Empty

  def max(): Tweet = null

  def descendingByRetweet: TweetList = new NilList

  override def toString = "."
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {
  override def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left contains x
    else if (x.text > elem.text) right contains x
    else true

  override def incl(x: Tweet): TweetSet =
    if (x.text < elem.text) new NonEmpty(elem, left incl x, right)
    else if (x.text > elem.text) new NonEmpty(elem, left, right incl x)
    else this

  def merge(other: TweetSet): TweetSet = ((left merge right) merge other) incl elem

  def filter(p: Tweet => Boolean): TweetSet = {
    if (p(elem)) (left filter p) merge (right filter p) incl elem
    else (left filter p) merge (right filter p)
  }

  def union(that: TweetSet): TweetSet = {
    this merge that
    filter((p: Tweet) => this contains p)
    filter((p: Tweet) => that contains p)
  }

  def remove(tweet: Tweet): TweetSet = {
    if (elem.text == tweet.text) left merge right
    else left.remove(tweet) merge right.remove(tweet) incl elem
  }

  def max(): Tweet = {
    val leftMax = left.max()
    val rightMax = right.max()
    if (leftMax == null && rightMax == null) elem
    else if (leftMax == null || elem.retweets >= leftMax.retweets) {
      if (rightMax == null || elem.retweets >= rightMax.retweets) elem else rightMax
    } else if (rightMax == null || leftMax.retweets >= rightMax.retweets)
      leftMax
    else rightMax
  }

  def descendingByRetweet: TweetList = new Cons(this max, this remove (this max) descendingByRetweet)

  override def toString = "{" + left + elem + right + "}"

}

val t1 = new Tweet("vgalloy", "Hello", 1)
val t2 = new Tweet("vgalloy", "Bonjour", 2)
assert(!(new Empty contains t1))
assert((new Empty incl t1) contains t1)
assert(!new Empty().incl(t1).contains(t2))
assert(new Empty().incl(t1).incl(t2).contains(t1))
assert(new Empty().incl(t1).incl(t2).contains(t2))
// 1. Filtering
assert(new Empty().incl(t1).incl(t2).filter((t: Tweet) => t.retweets == 2).contains(t2))
assert(!new Empty().incl(t1).incl(t2).filter((t: Tweet) => t.retweets == 2).contains(t1))
// 2. Taking Unions
val t3 = new Tweet("vgalloy", "Bye bye", 3)
assert((new Empty().incl(t1).incl(t2) union new Empty().incl(t1).incl(t3)).contains(t1))
assert(!(new Empty().incl(t1).incl(t2) union new Empty().incl(t1).incl(t3)).contains(t2))
assert(!(new Empty().incl(t1).incl(t2) union new Empty().incl(t1).incl(t3)).contains(t3))
// 3. Sorting Tweets by their influence
// Remove
assert(!new Empty().incl(t1).remove(t1).contains(t1))
assert(new Empty().incl(t1).remove(t2).contains(t1))
assert(new Empty().incl(t1).incl(t2).remove(t2).contains(t1))
// Max
assert(new Empty().incl(t1).incl(t1).incl(t2).max().retweets == new Empty().incl(t1).incl(t2).incl(t2).max().retweets)

// descendingByRetweet
new Empty().incl(t1).descendingByRetweet
new Empty().incl(t1).incl(t3).incl(t3).incl(t2).descendingByRetweet

// 4. Trying everything together
val allTweet = new Empty().incl(new Tweet("Title", "void", 1))
  .incl(new Tweet("Title", "google", 2))
  .incl(new Tweet("Title", "An apple", 3))
  .incl(new Tweet("Title", "google and apple", 4))
  .incl(new Tweet("Title", "void", 5))
  .incl(new Tweet("Title", "void", 6))
  .incl(new Tweet("Title", "google 2", 7))
  .incl(new Tweet("Title", "apple", 8))
  .incl(new Tweet("Title", "This is google", 9))
  .incl(new Tweet("Title", "This is apple", 10))
  .incl(new Tweet("Title", "The world without apple", 11))
  .incl(new Tweet("Title", "The world vs google", 11))

val googleTweets: TweetSet = allTweet.filter((t: Tweet) => t.text.contains("google"))
val appleTweets: TweetSet = allTweet.filter((t: Tweet) => t.text.contains("apple"))


val trending = (googleTweets merge appleTweets) descendingByRetweet