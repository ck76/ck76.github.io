# LinkedList

The **Linked List** in Rust is a sequence of nodes where each node consists of a value and a pointer/reference to the next node in the sequence. Linked Lists are primarily used for implementing higher level data structures such as stacks, queues and associative arrays. Rust's `std::collections` provides a `LinkedList` struct that is a doubly-linked list with O(1) insertion and removal at both the front and back, but with O(n) indexing.
