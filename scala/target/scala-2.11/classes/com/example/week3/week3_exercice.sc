import com.example.week3.TweetReader
import com.example.week3.{Empty, Tweet}

val t1 = new Tweet("vgalloy", "Hello", 1)
val t2 = new Tweet("vgalloy", "Bonjour", 2)
assert(! (new Empty contains t1))
assert((new Empty incl t1) contains t1)
assert(! new Empty().incl(t1).contains(t2))
assert(new Empty().incl(t1).incl(t2).contains(t1))
assert(new Empty().incl(t1).incl(t2).contains(t2))

// 1. Filtering
assert(new Empty().incl(t1).incl(t2).filter((t: Tweet) => t.retweets == 2).contains(t2))
assert(! new Empty().incl(t1).incl(t2).filter((t: Tweet) => t.retweets == 2).contains(t1))

// 2. Taking Unions
val t3 = new Tweet("vgalloy", "Bye bye", 3)
assert((new Empty().incl(t1).incl(t2) union new Empty().incl(t1).incl(t3)).contains(t1))
assert(!(new Empty().incl(t1).incl(t2) union new Empty().incl(t1).incl(t3)).contains(t2))
assert(!(new Empty().incl(t1).incl(t2) union new Empty().incl(t1).incl(t3)).contains(t3))

// 3. Sorting Tweets by their influence
// Remove
assert(new Empty().incl(t1).remove(t1) == new Empty())
assert(new Empty().incl(t1).remove(t2).contains(t1))
assert(new Empty().incl(t1).incl(t2).remove(t2).contains(t1))

// Max
assert(new Empty().incl(t1).incl(t1).incl(t2).max().retweets == new Empty().incl(t1).incl(t2).incl(t2).max().retweets)

// descendingByRetweet
new Empty().incl(t1).descendingByRetweet
new Empty().incl(t1).incl(t3).incl(t3).incl(t2).descendingByRetweet

// 4. Trying everything together

TweetReader.allTweets.contains(new Tweet("", "", 1))