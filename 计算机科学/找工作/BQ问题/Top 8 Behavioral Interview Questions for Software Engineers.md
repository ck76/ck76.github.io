[TOC]

- https://betterprogramming.pub/top-8-behavioral-interview-questions-for-software-engineers-6dcc404bb7b3

While you’re grinding away at LeetCode and HackerRank for your next coding interview, make sure you don’t forget to prepare for the behavioral interview as well.

Behavioral interviews can look a lot different depending on your background, experience, and the company you’re interviewing with.

These interviews are designed to get to know your personality, understand your resume more deeply, and do a deep dive into your background.

One of the most important ways to prepare for this interview is to ensure you can talk about each section of your resume in detail. Both the technical and non-technical aspects of it.

I am sure most of you know about this book, I highly recommend checking out *Cracking the Coding Interview* by Gayle Laakmann McDowell if you haven’t already. The book does a good job of describing the behavioral interview process at a high level.

For this article, I’ll give the top 8 behavioral interview questions that I have personally come across and real answers that I have given which helped me nail the interview.

## 1. Tell me about a project that you worked on recently. Can you describe the technical challenges you faced and how you overcame those challenges?

This is where knowing the ins and outs of the technical details of each piece of your resume really comes in handy.

The interviewer may ask the question generically like in this example, or they may point out a project or role on your resume specifically and ask about that. So be prepared to talk about this in either scenario.

**Example:** *“I recently worked on a project that had some very specific security requirements the application had to follow. At a high level, we had to build a server that could function as a reverse proxy and a forward proxy and allow us to add in custom logic during the TLS handshake.*

*My first obstacle was trying to find an off-the-shelf product that we could use for this. We have normally used NGINX for all of our proxying and load balancing throughout the system, however, NGINX did not support a way for us to add in a custom hook to the TLS logic. We looked into other OTS tools like Apache HTTP Server and HAProxy, but nothing supported what we needed out of the box.*

*Eventually, I found that Node.js has a TLS module that provided exactly what we needed. The TLS module allowed us to easily add in custom code during the TLS handshake, and it contained a very lightweight, easy to set up HTTP proxy server.*

*This was my first time developing a production application from the ground up in Node.js. Going from mainly Java development to Node.js provided a bit of a challenge due to the asynchronous behavior of Node.js. The custom logic we were writing required us to make an HTTP request during the TLS handshake, however in Node.js this request is asynchronous, and I realized that I wasn’t able to wait for the response before the TLS handshake was completed.*

*Eventually, I found a way to overcome this by not putting the HTTP request into the TLS logic. Since we were caching the request, I made the request at the startup of the server, cached it, and then set a timeout function to make the request again after the value expired. That way we avoided making an async request in the TLS handshake and we just grabbed the value from the cache.*

*The project ended up being a huge success and passed all the requirements. These security requirements were not only within our program, but other programs being built for the customer as well. We actually had other programs within the company reach out to us for the server that I had built so they could integrate it with their system as well which saved the company weeks of development time in other programs.”*

## 2. Tell me about a time you had a disagreement with a coworker and how you resolved it.

Here is where the interviewer wants to see how you handle challenging situations with your coworkers, so you want to focus on an area where you resolved an issue with them in the past.

Even small disagreements that you have had with coworkers can be used in questions like this, but if you truly can’t think of anything, you could try to shape your answer into a more general answer about how you deal, or would deal, with disagreements between your coworkers.

**Example:** *“I’ll tell you a situation in which I had a disagreement with my engineering lead. The team I was on never adopted a good branching strategy. We were constantly dealing with issues where we would have half-baked features mixed with other features that were ready to go, and we would have to cherry-pick the certain commits we wanted to take into the next build. This took up a lot of time.*

*My manager put together a new branching strategy and presented it to us as the new path we would follow. The others on the call seemed fine with it, however, I did see a flaw in the strategy, and I spoke up about it. My manager saw the flaw as well but thought that it was still better than what we are currently doing and we should present this to the rest of the team to use for now since we have a lot of other important tasks to accomplish.*

*I said that I didn’t think it was a good idea to have the team start adopting a new branching strategy that we already know has a flaw and will need updating again soon. Although I understood the priority to finish our other tasks first, I offered to come up with a solution that fixes the issue here and put together some slides without impacting my other milestones. He agreed and we set up a meeting for the next day to go over my design.*

*I spent some time putting together the branching strategy, which was a more common GIT branching strategy that is used in the industry, and presented it to him and the other leads the next day. Everyone agreed on the new strategy and my manager thanked me for speaking up and putting this together in such a short time. Since this new strategy has been adopted, our team has cut down significant time on dealing with version control issues.*

*Overall, when I deal with disagreements, I really try to ensure that everyone understands both points of view and why each person came to the decision they did. I find that normally once you dig deep into both sides, we can usually come to an agreement. However, if that’s not the case and each person is still adamant about their decision, I like to get other engineers involved and get their input on it.”*

## 3. Why do you want to work at <company-name>?

For this question, it helps to research the company beforehand. Look into the companies values on their website, projects the company is working on (if available) and the technologies the company is using.

**Example:** *“What first excited me about the position, in particular, was the technologies that the company is using. I have been working with React and Node.js in my side project and I have really fallen in love with the entire JavaScript ecosystem, so I really want to steer my career more to that tech stack. I also want to continue working with cloud services and saw that the team is using AWS which excited me as well.*

*Aside from the technical side of things, I looked more into the company culture and saw that you really value creating an environment in which engineers can learn and continue to grow. Continuous growth is really important to me, and I want the next company I join to share that importance as well.*

*Finally, the product itself looks amazing and seems to have the ability to impact people across the world. I would love to be part of that initiative.”*

## 4. Why are you leaving your current company?

You want to give legit reasons for wanting to leave, but don’t make it sound like you are miserable at your current job. That could hurt you in the negotiating process, and also could make you seem like a negative person in their eyes.

Another angle to go at this question is to word it in a way where it doesn’t sound like you really want to leave your job, but you would just rather be with the company you are interviewing with. Then give reasons as to why you are excited to work there over your current company.

**Example:** *“Although overall I am happy in my current role, there are a few reasons I am looking for a new opportunity.*

*I feel that I have gotten to a point in my career where I am not learning as much as I want to be. I noticed that your company is using AWS and that is a technology that I have been wanting to get some experience in. Cloud platforms in general are something I am excited to work with.*

*This kind of leads into my next reason, which is that I am looking for more challenging work. As I mentioned I feel that I am not learning as much as I want to be, and the work that I have been doing lately hasn’t really provided any new challenges. I am excited to work with the newer technologies that your company is using as I know they will provide new and unique challenges.”*

## 5. What would you say is a weakness of yours?

You want to point out a legit weakness, but really hone in on how you are improving, or have improved yourself to overcome that weakness.

**Example:** *“I would say my biggest weakness is public speaking. Sometimes I have trouble speaking in front of other people and I get extremely nervous speaking in a large group. When I was younger, I used to try to avoid situations in which I would have to speak in a group any way I could, however as I got into college I started to realize that I need to face this fear instead of running from it.*

*As I’ve grown in my career, I have become a lot more comfortable with speaking in groups. I present in a lot of different meetings now daily like design reviews or demos to the customer, so I’ve gotten a lot of experience speaking in front of groups and leading discussions with a lot of attendees. While I still get nervous speaking in front of large groups, I’ve grown more accustomed to it and realize that those nerves go away as I start getting into the meeting and am comfortable with what I am presenting.”*

## 6. Describe a time when you received tough feedback or a time when you gave tough feedback. How did you handle the situation?

Constructive criticism can be helpful for your overall professional growth, so consider answering this question with a piece of criticism you received and how it helped you improve your work. You can show your ability to react positively to constructive criticism and demonstrate that you’re willing to learn and progress.

On the flip side, being able to give good feedback to your coworkers is important as well. This really shows you are a team player and that you care about improving others around you.

**Example:** *“I feel that constructive feedback from your peers is one of the best ways for an engineer to grow. I also feel that an important part of giving, or receiving feedback, is to give or ask for steps the engineer or you can take to improve.*

*An example that comes to mind when I gave feedback to someone was with a software engineer that is on my current team. I noticed that quite often in our group meetings, this engineer would jump into the conversation to give their thoughts and opinions, however, it felt like they didn’t really prepare their thoughts on what they were trying to say. They would seemingly start to ramble a bit and appear to get lost in their own thoughts trying to figure out what they’re trying to say.*

*Our company has semi-yearly performance reviews; however, I believe in giving feedback right away when I see something that I feel my colleague could improve on. So, one day I spoke with them and told them what I noticed, and they even agreed with what I had to say and said they noticed it themselves too, but they weren’t aware that it was obvious to others. I offered some advice in that they shouldn’t feel like they need to rush in order to get their points into the conversation. We’re all one team and we want to make sure everyone has an opportunity to speak up and agree on decisions we make as a team.*

*They told me that they really appreciated me talking to them about this so that they have the opportunity to improve themselves. Since that conversation, I noticed a continuous improvement in their ability to engage in conversations at the right time and be able to get their points across to the team efficiently.”*

## 7. Tell me about yourself and your background.

I learned a great way to structure this generic question from *Cracking the Coding Interview*. It is broken down like this:

1. Current Role [Just the Headline]
2. College [Include Internships]
3. Post College and Onwards
4. Current Role [Detailed]
5. Outside of Work
6. Wrap Up

**Example:** *“I’m currently a Software Engineer at <company-name>where I’ve been leading a small agile team of senior software engineers. I graduated from <school-name> in 2018 with a bachelor’s degree in Computer Science and Engineering.*

*I put myself through college working as a waiter for a while and also working as a Java Consultant for the university. As a Java Consultant, I would help students with their course work and help them understand Java concepts and best coding practices.*

*The summer before my senior year I replaced my job as a waiter with an internship at <company-name> which was an amazing experience for me. Over that summer I built a new time tracking application from the ground up using Java and the Spring framework that ended up replacing the current system used by the organization. My success at the internship led them to keep me on part-time while I finished up my senior year.*

*After college, I was recruited by <company-name>and was brought on as a level one software engineer. The team I was brought onto is responsible for the security, deployment, and the monitoring and control interface for the system. After about a year I became the technical lead of the team I was on. I actually became the lead at a critical time, as the program I was on was in danger of being shut down. Since that time, however, we were able to recover the schedule, fix all the critical requirement failures and get the program back on track for a successful operation deployment. This success restored faith in the team and secured future business with the customer.*

*Outside of work I’m currently working on a side project that I intend to finish and deploy to the web and app store this year using Node.js, React and React Native. I also just recently got into writing articles on Medium and have written a few articles on software development tips that have been published.*

*So, now I’m looking for something new and the job posting for this role really caught my eye. I love everything I’ve learned about the company so far and I’m really excited for this opportunity.”*

## 8. Tell me about a mistake you made and what you learned from it.

Everyone makes mistakes, but not everyone learns from them. The interviewer wants to see that you learn from your mistakes and grow from them.

Think about common mistakes that engineers make. Ever ran a query that wiped out an important part of the database? How about messing up a merge conflict, or force pushing something to the master branch?

**Example:** *“Earlier in my career, I was tasked with designing a solution to a feature that was needed for a customer project. I put together the design and met with the systems engineering team and my lead to present the design, and everyone agreed to go forward with it.*

*However, once I presented the design to the customer, they pointed out some flaws in the design and we realized there was a misunderstanding of the requirements. The big issue was that we had already used up all of the time that was estimated for the design task, so we were in danger of getting behind schedule with the development.*

*To remedy the situation, I ended up working some overtime hours to put together a new design that would meet the requirements and resolve the issues pointed out by the customer. I was able to quickly put together a new design over the weekend and present it to the customer the following week. The new design was approved, and we were able to get started on the development work on schedule.*

*I learned a valuable lesson here in that I should have gotten the customer involved in the design early on and iteratively as needed. If I had done that in this situation, I would have been able to catch the issues earlier and not at the last minute, which led me to overwork myself that week.”*

# Conclusion

Thank you for reading! I hope this will help some of you with your interview preparation and wish you all the luck with your job search.

If you liked the article, please consider following me as I plan on writing more content like this in the future. Also, feel free to connect with me below!