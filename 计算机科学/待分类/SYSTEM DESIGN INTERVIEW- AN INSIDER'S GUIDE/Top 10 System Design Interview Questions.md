[TOC]

# Top 10 System Design Interview Questions

The system design interview is considered to be the most complex and most difficult technical job interview by many. Below are the top 10 frequently asked design questions to help you get started. The architecture diagrams listed below are high level designs or parts of a big system. To see the entire design and deep dive of each component, please check out the course

### [Take the Course](http://courses.systeminterview.com/)

## 1. Design a Chat System (Facebook messenger, Whatsapp)

It is very likely that you have used at least one chat app. Consider this question an invitation to do a deep dive of your favorite chat system. This question can be solved in multiple directions. The design diagram below is part of one solution and illustrates the high level design for 1-on-1 chat.

<img src="https://tva1.sinaimg.cn/large/008i3skNly1gwp6n7kvmaj30u012t77w.jpg" alt="Design a chat system" style="zoom:50%;" />

## 2. Design a Video Streaming Service (YouTube, Netflix)

Video streaming services such as YouTube and Netflix have gained a lot of popularity. Besides watching a video, you can do a lot more on YouTube or Netflix. For example, comment, share, or like a video, save a video to playlists, subscribe to a channel, etc. It's important to narrow down the scope. The following design diagram focuses on video uploading flow.

<img src="https://tva1.sinaimg.cn/large/008i3skNly1gwp6n4qle1j30u0133tc1.jpg" alt="Design a video streaming service" style="zoom:50%;" />

## 3. Design a Key-value Store

Key-value store design is one of the most common interview questions. A key-value store, also referred to as a key-value database, is a non-relational database. Each unique identifier is stored as a key with its associated value. This data pairing is known as a “key-value” pair. Most popular key-value stores are DynamoDB, Memcached, Redis, Cassandra, BigTable, etc. The following design emphasizes on the read path.

![Design a key-value store](https://tva1.sinaimg.cn/large/008i3skNly1gwp6n2df91j31bm0medi5.jpg)

## 4. Design a URL shortener

Design a URL shortening service like tinyurl. For example, https://www.systeminterview.com/q=chatsystem&c=loggedin&v=v3&l=long is the original URL. Your service creates an alias with shorter length: https://tinyurl.com/ y7keocwj. If you click the alias, it redirects you to the original URL. The following diagram explains how URL shortening flow works.

<img src="https://tva1.sinaimg.cn/large/008i3skNly1gwp6mwn30aj30tk0mimyg.jpg" alt="Design a URL shortener" style="zoom:50%;" />

## 5. Design a Web Crawler

Web crawler design is a tough interview question. A web crawler is known as a robot or spider. It is widely used by search engines to discover new or updated content on the web. Content can be a web page, an image, a video, a PDF file, etc. A web crawler starts by collecting a few web pages and then follows links on those pages to collect new content. The high-level design for web crawler is shown below.

![Design a web crawler](https://tva1.sinaimg.cn/large/008i3skNly1gwp6ms36zkj319q0u0aby.jpg)

## 6. Design a Shared Cloud Drive (Google Drive, Dropbox)

Cloud storage services such as Google Drive, Dropbox, Microsoft OneDrive, and Apple iCloud have become very popular. You can be asked to design a Shared Cloud Drive. Below is a sneak peak of a small part of the system. The diagram shows what happens when a file is added/dragged to a shared drive.

<img src="https://tva1.sinaimg.cn/large/008i3skNly1gwp6mpp24aj314f0u0q60.jpg" alt="Design cloud storage system such as google drive or dropbox" style="zoom:50%;" />

## 7. Design a News feed System

News feed design is a popular interview question, including Facebook new feed, Instagram feed, Twitter timeline etc. The diagram below illustrates the feed publishing flow, one of the core components of the news feed system.

<img src="https://tva1.sinaimg.cn/large/008i3skNly1gwp6mj5x4tj30qy17agom.jpg" alt="Design a news feed system" style="zoom:50%;" />

## 8. Design a Search Autocomplete System

When searching on Google or shopping at Amazon, as you type in the search box, one or more matches for the search term are presented to you. This feature is known as autocomplete, typeahead, search-as-you-type, or incremental search. The design diagram below explains the data collection flow. It is part of the comprehensive autocomplete system.

![Design a search autocomplete system](https://tva1.sinaimg.cn/large/008i3skNly1gwp6mgd3c1j31gc0hm76t.jpg)

## 9. Design a Notification System

A notification system is a very popular feature for many applications. A notification alerts a user with important information and has become an indispensable part of our daily life. A notification is more than just a mobile push notification. Three types of notification formats are: mobile push notification, SMS message, and Email. The high level design for a notification system is shown below:

![Design a notification system](https://tva1.sinaimg.cn/large/008i3skNly1gwp6mey7zaj31b20sg0w2.jpg)

## 10. Design a scalable system that supports millions of users

How to scale a system to support millions of users lies at the heart of almost all the system design interviews. No matter what interview question is asked, you will find this knowledge handy and useful. The high level design is shown below:

<img src="https://tva1.sinaimg.cn/large/008i3skNly1gwp6md9b2qj30u011wwij.jpg" alt="Design a scalable system that supports millions of users" style="zoom:50%;" />

It is completely understandable that if you feel overwhelmed looking at this list. Just know this: Nobody expects you to have all the answers on top of your head—that's what we are here for. You can either take the text-based course or buy the book to have complete access. Both options have the same content.

This website only contains 4 chapters. If you like the content and would like to read more, you can either take the text-based course or buy the book on Amazon. Both options have the same content.