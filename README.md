# NLP-Filter

An app to filter offensive comments from social media feeds.


## GOAL
Create an app that can be used, in the same way as a spam filter, by individual users
to filter out offensive comments from their social media feeds.  Working language: German.

# The Thought Behind the App

1. Empower the individual: in order to maintain a balance between freedom of speech, and the right to dignity,
it should be the individual who decides what is appropriate language for them to receive in their online presences.
1. As much as possible, the collection of data to train the algorithm should also be in the power of the individual end-user.
1. At no point during the creation or deployment of the app, should offensive behavior be encouraged.
1. Centralized collection of data on individual end-users will be restricted to the absolute minimum necessary for the functioning of the app.  Such data will not be used for any other purpose.



# Properties

1. The filter should be customizable:  what is offensive to one person is not necessarily offensive to another.
1. The filter should contain a learning algorithm that is either
    * Fast enough to keep updating its learning (online learning)
    * Linked to a central unit that can occasionally update
1. The filter should work on incoming as well as outgoing comments.

