***Object-Oriented Design…***the most important topic to learn in programming.

If you’re a fresher or a software engineer working in a company then surely you might be aiming to reach some big companies such as Facebook, Microsoft, Google, or Amazon. These companies conduct the design round to check the ability of the candidate whether he/she is able to build a complex system or not. This round can be a system design round or an object-oriented design round. We have already discussed the topic of [cracking the system design round in interviews](https://www.geeksforgeeks.org/how-to-crack-system-design-round-in-interviews/).

![How-to-Approach-Object-Oriented-Design-Questions-in-Interview](https://tva1.sinaimg.cn/large/008i3skNly1gw86xsg0ndj30rs0dp3z5.jpg)

[![GeeksforGeeks LIVE courses](https://tva1.sinaimg.cn/large/008i3skNly1gw86xvjljdj30xc08g74p.jpg)](https://practice.geeksforgeeks.org/courses/live?utm_source=article&utm_medium=post-ads&utm_campaign=live-post-ads&utm_term=live-courses-geeksforgeeks)

In this blog, we will discuss how to approach an object-oriented design round in interviews. If you’re preparing for the entry-level developer role or senior developer role then this blog is certainly for you.





Let’s start with identifying the main objective of this round…

- A generic problem statement will be given to the candidate, and the candidate has to come up with a technical solution using his object-oriented designing skill. 
- The candidate needs to identify the different objects and entities in a given problem statement.
- The interviewer will check how much a candidate is comfortable with OOPs concepts and principles and how well a candidate can design a system, using these concepts. 

### Preparation Before the Interviews

**1.** You should have a good command of one object-oriented programming language such as Java/C++/Python etc. Have some experience in it and learn how the OOPs concepts work in these languages.

- Abstraction
- Encapsulation
- Inheritance
- Polymorphism
- Association, Aggregation, Composition
- Class
- Object
- Method

**2.** Understand the various [object-oriented design principles](https://www.geeksforgeeks.org/solid-principle-in-programming-understand-with-real-life-examples/) such as SOLID/DRY principles etc. 

**3.** If you have a good command of various design patterns such as MVC, singleton, factory method, etc then it will be a bonus point for you as an entry-level developer or senior-level developer. 

Let’s come to the next step that is the Interview process…

### Step-by-Step Process to Approach The Problem

### 1. Requirement Gathering

Firstly identify what the interviewer is looking for? Is it a system-level design or object-level design round in interviews? Both of the rounds will have a different approach to follow. Starts with the requirement gatherings. 

Here the interviewer will give you a high-level description of the system and you will have to identify the exact scope of the problem. Dig deeper and clarify everything he is looking for in the system. Discuss with the interviewer about the features and ask questions to clear out the confusion. Try to figure out the output interviewer is looking for. 

- Gather the top requirements or the feature you want to include in your design. Write down all the requirements and discuss them with the interviewer. Your interviewer will mention if he/she wants to include some other details or features.
- Once the requirement is discussed talk about the use cases on a high level.
- Now define your assumptions clearly to the interviewer.
- Mention and clarify the scope you need to address in the interviews.

Let’s take the example of an online shopping system. In this example, we can consider a few requirements given below…





- Customer can search/view/buy products
- A shopping cart for the customer to put all the items they want to buy.
- Customer should be able to pay through the credit cards/debit cards/net banking
- The customer should be able to mention the shipping address where his/her orders can be delivered.

### 2. Use Case Diagram

While discussing the requirement you must have identified all the use cases that you need to include in your design. List all the use cases your system is supposed to design. You will be able to identify the different components and actors you need to include in your system. 

- Identify all the actors in the Use Case Diagram
- Identify the roles of different actors.

Now you need to transform that into the Use Case Diagram. Below is the use case for the online shopping system.

![Use-Case-Diagram](https://tva1.sinaimg.cn/large/008i3skNly1gw86xyeb1dj30rs0dw3zu.jpg)

- Search product on the website
- View Product on website
- Add/Remove Product from the shopping cart
- Checkout
- Make Payment and specify a shipping address

### 3. Activity Diagram

Once you identified the actors and the role of each actor, focus on the logic, complete structure, and the overall flow of the system. Write down the flow diagram. This will help you to understand the problem clearly and you will have the chance to rectify your mistakes. Below is the activity diagram for the online shopping system.

![Activity-Diagram](https://tva1.sinaimg.cn/large/008i3skNly1gw86y0b9aej30rs0dwta1.jpg)

### 4. Class Diagram

This is the core part of the interview process. Your interviewer will be interested in this part more. Till now, you have all the materials such as different system objects and actors to place them in a class diagram.

Start with identifying the core objects. How to identify the core objects? Below is the rule to identify the core objects…

- Nouns in requirements are possible candidates for objects.
- Verbs in the requirements are possible methods.

**A.** Your main objective is to identify the relationship between the objects.

**B.** Identify whether to use an abstract class or interface for the abstractions

**C.** Check if you can use some design patterns in it or not.





**D.** Design a class diagram to represent everything to the interviewer. 

Keep in mind one thing that the interviewer may ask additional questions to you. He/she may ask a few questions regarding your design choices. You will have to give the justification for the choice you made. How would you handle the system if in the future users grow on your website? The interviewer may ask these kinds of questions to you.

Design questions are open-ended and different candidates will have different answers. There are no wrong answers….so defend your design.  

Make sure that you keep in mind basic object-oriented design principles. These principles will help you in class diagrams and it will help you in handling the questions effectively. 

![Class-Diagram](https://tva1.sinaimg.cn/large/008i3skNly1gw86y24r54j30rs0dw75q.jpg)

### 5. Describe Top Use Cases

Use the class defined in the above step and run the top use cases. This is similar to the activity diagram. This will help you if you have missed out on any components or scenarios. You need to explain this verbally to the interviewer. 

### 6. Code

The coding part is optional in this round but the interviewer may ask you to write the code for a specific feature or component. In case if you’re asked to write the code prioritize the things given below.

- Start with the Interfaces or abstract classes
- Write the code for the core objects and the internal structure. 
- Write the implementation part.
- Write JUnit

At a high level, define all the required properties, functions, interfaces, etc. 





- https://www.geeksforgeeks.org/6-steps-to-approach-object-oriented-design-questions-in-interview/