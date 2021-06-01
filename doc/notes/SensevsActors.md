## SenseVM vs Actors

1. asynchronous send implies an unbounded mailbox; asynchronous send resorts to synchronous sends anyways
2. explicit acknowledgement and related cruft arises in actor languages
3. specific SenseVM vs Medusa - medusa has to tag interrupt handlers and uses those tags to differentiate between messages in the mailbox. no such cost incurred because of the per driver channel in sense.
