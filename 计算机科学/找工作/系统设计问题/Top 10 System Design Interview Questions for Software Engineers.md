[TOC]

### Top 10 System Design Interview Questions for Software Engineers

Designing Large Scale Distributed Systems has become the standard part of the software engineering interviews. Engineers struggle with System Design Interviews (SDIs), primarily because of the following two reasons:

1. Their lack of experience in developing large scale systems.
2. Unstructured nature of SDIs. Even engineers who’ve some experience building large systems aren’t comfortable with these interviews, mainly due to the open-ended nature of design problems that don’t have a standard answer.

A great performance in SDIs is highly rewarding since it reflects upon your ability to work with complex systems and translates into the position and compensation (salary & stocks) that the interviewing company will offer you.

***Check out the following resources to prepare for software engineering interviews:\***

> System Design Interviews: [**Grokking the System Design Interview**](https://www.educative.io/collection/5668639101419520/5649050225344512)
>
> Object-Oriented Design Interviews: [**Grokking the Object-Oriented Design Interview**](https://www.educative.io/collection/5668639101419520/5692201761767424)
>
> Coding interviews preparation: [**Grokking the Coding Interview**](https://www.educative.io/courses/grokking-the-coding-interview)
>
> Data Structures: [**Mastering Data Structures: An Interview Refresher**](https://www.educative.io/m/data-structures)
>
> In-depth guides to interviews at different tech companies: [**CodingInterview**](https://www.codinginterview.com/)

At [Educative.io](https://www.educative.io/), we’ve talked to hundreds of candidates who went through design interviews. As part of the process, we’ve compiled a list of most frequently asked System Design Interview Questions.

Following are the most frequently asked questions along with a few pointers to the things that interviewers want you to consider while designing the system.

## 1. Design TinyURL or bitly (a URL shortening service)

![img](https://miro.medium.com/max/60/1*0tUSMwh9fU7vD3ekxP5H2w.png?q=20)

![img](https://tva1.sinaimg.cn/large/008i3skNly1gwnxnltw13j30b403mmx5.jpg)

Given a (typically) long URL, how would how would you design service that would generate a shorter and unique alias for it.

Discuss things like:

- How to generate a unique ID for each URL?
- How would you generate unique IDs at scale (thousands of URL shortening requests coming every second)?
- How would your service handle redirects?
- How would you support custom short URLs?
- How to delete expired URLs etc?
- How to track click stats?

## **2. Design YouTube, Netflix or Twitch (a** global video streaming service)

![img](https://miro.medium.com/max/60/1*Yo4vob7HDFDhCJ9hhLaCTg.png?q=20)

![img](https://tva1.sinaimg.cn/large/008i3skNly1gwnxnm8r98j30b403maa2.jpg)

Videos mean that your service will be storing and transmitting petabytes and petabytes of data.You should discuss how to efficiently store and distribute data in away that a huge number of users can watch and share them simultaneously (e.g. imagine streaming the latest episode of a hit TV show like Games of Thrones).

In addition, discuss:

- How would you record stats about videos e.g the total number of views, up-votes/down-votes, etc.
- How would a user add comments on videos (in realtime).

## **3. Design Facebook Messenger or WhatsApp (a global chat service)**

![img](https://miro.medium.com/max/60/1*YJGHWs5gzSpMj4aVaexfmQ.png?q=20)

![img](https://tva1.sinaimg.cn/large/008i3skNly1gwnxnoln6uj30b403m74a.jpg)

Interviewers are interested in knowing:

- How would you design one-on-one conversations between users?
- How would you extend your design to support group chats?
- What to do when the user is not connected to the internet?
- When to send push notifications?
- Can you provide end-to-end encryption. How?

## 4. Designing Quora or Reddit or HackerNews (a social network + message board service)

![img](https://miro.medium.com/max/60/1*zBB6MN3N4e4J4HtM2ITfzg.png?q=20)

![img](https://tva1.sinaimg.cn/large/008i3skNly1gwnxnpsahpj30b403mmx5.jpg)

Users of the services can post questions or share links. Other users can answer questions or comment on the shared links. The service should be able to:

- Records stats for each answer e.g. the total number of views, upvotes/downvotes, etc.
- Users should be able to follow other users or topics
- Their timeline will consist of top questions from all the users and topics they follow (similar to newsfeed generation).

## **5. Design Dropbox or Google Drive or Google Photos (a global file storage & sharing service)**

![img](https://miro.medium.com/max/60/1*ya6zGBWWCmlm-YxeT8uKkA.png?q=20)

![img](https://tva1.sinaimg.cn/large/008i3skNly1gwnxnrh2fyj30b403m3yi.jpg)

Discuss things like:

- How would users be able to upload/view/search/share files or photos?
- How would you track persmissions for file sharing
- How would you allow multiple users to edit the same document

## 6. Design Facebook, Twitter or Instagram (a social media service with hundreds of millions of users)

![img](https://miro.medium.com/max/60/1*UNZEJGQ6INP3TNTpzRgX-Q.png?q=20)

![img](https://tva1.sinaimg.cn/large/008i3skNly1gwnxnstfsrj30b403m74c.jpg)

When designing a social medial service with hundreds of million (or billions of users), interviewers are interested in knowing how would you design the following components

- Efficient storage and search for posts or tweets.
- Newsfeed generation
- Social Graph (who befriends whom or who follows whom — specially when millions of users are following a celebrity)

A lot of times, interviewers spend the whole interview discussing the design of the newsfeed.

## **7.** Design Uber or Lyft (**a ride sharing service)**

![img](https://miro.medium.com/max/60/1*nabu7NFLKRSuWXwu5NMQrw.png?q=20)

![img](https://tva1.sinaimg.cn/large/008i3skNly1gwnxntv89yj30b403mglj.jpg)

While designing a ride-sharing service, discuss things like:

- The most critical use case — when a customer requests a ride and how to efficiently match them with the nearby drivers?
- How to store millions of geographical locations for drivers and riders who are always moving.
- How to handle updates to driver/rider locations (millions of updates every second)?

## **8. Design a Web Crawler or Type-Ahead (search engine related services)**

![img](https://tva1.sinaimg.cn/large/008i3skNly1gwnxnvnszyj301o00j0m4.jpg)

![img](https://miro.medium.com/max/800/1*GehHeMZ6B2XR3kZpfZrrkA.png)

For Type-Ahead, as the user types in their query, you need to design a service which would suggest top 10 searched terms starting with whatever the user has typed. Discuss things like:

- How to store previous search queries?
- How to keep the data fresh?
- How to find the best matches to the already typed string?
- How to handle updates and the user is typing too fast?

For Web Crawler, we have to design a scalable service that can crawl the entire Web, and can fetch hundreds of millions of Web documents. Discuss things like:

- How to find new web pages?
- How to prioritize web pages that change dynamically?
- How to ensure that your crawler is not infinitely stuck on the same domain?

## **9.** **Design an API Rate Limiter (e.g. for Firebase or Github)**

![img](https://miro.medium.com/max/60/1*tqgQQZHbV0QfI0bwkPUFlQ.png?q=20)

![img](https://tva1.sinaimg.cn/large/008i3skNly1gwnxnxosdpj30b403mglg.jpg)

You are expected to develop a Rate Limiter services that can:

- Limit the number of requests an entity can send to an API within a time window e.g., 15 requests per second.
- The rate limiting should work for a distributed setup, as the APIs are accessible through a cluster of servers.
- How would you handle throttling (soft and hard throttling etc.).

## **10. Design Yelp or** Nearby Places/Friends **(**a proximity server)

![img](https://miro.medium.com/max/60/1*BKFAcfpwmY3KvmYgD8R5sg.png?q=20)

![img](https://tva1.sinaimg.cn/large/008i3skNly1gwnxnyxovbj30b403mdg2.jpg)

This service would need to store locations for millions of people/places. Discuss things like:

- How would the users of the service be able to search nearby friends or places
- How to rank places (based on the distance, user reviews).
- How to efficiently store location data according to the population density (e.g. a block in New York City might have more places/people than a small city).

# Software engineer Interview Preparation Resources

Following are some resources that can help you prepare for software engineering interviews.

1. **System Design Interviews:** [Grokking the System Design Interview](https://www.educative.io/collection/5668639101419520/5649050225344512).
2. **Coding Interviews:** [Grokking the Coding Interview](https://www.educative.io/courses/grokking-the-coding-interview)
3. **Data Structures:** [Mastering Data Structures: An Interview Refresher](https://www.educative.io/m/data-structures)
4. **In-depth guides/roadmaps for interviews:** [CodingInterview.com](http://codinginterview/)

Happy interviewing!

**If you found this post helpful, please click the** 👏 **sign and follow me for more posts. If you have any feedback, reach out to me on** [**Twitter**](https://twitter.com/fahimulhaq)**.**

*Fahim is the co-founder of* [*Educative*](https://www.educative.io/)*. We are building the next generation interactive learning platform for software engineers and instructors. Learners learn by going through interactive courses. Instructors can quickly create and publish interactive courses using our course builder. If you are interested in publishing courses or knowing more, feel free to reach out.*

 