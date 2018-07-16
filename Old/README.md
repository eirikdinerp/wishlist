# Wishlist

The plan is to make a wishlist application in elm..

## How to add make to windows

https://gist.github.com/evanwill/0207876c3243bbb6863e65ec5dc3f058

## What I am using to build this project

- Using elm boilerplate from [here](https://github.com/guillaumearm/elm-boilerplate)

## Resources going forward

Todo app

- https://github.com/evancz/elm-todomvc/blob/master/Todo.elm

Awesome elm

- https://github.com/isRuslan/awesome-elm

Application structure

- https://becoming-functional.com/nine-guidelines-for-modular-elm-development-fe18d2f7885e

- http://blog.jenkster.com/2016/04/how-i-structure-elm-apps.html

- https://dev.to/rtfeldman/tour-of-an-open-source-elm-spa

- https://github.com/rtfeldman/elm-spa-example

## WIP

- Create two modules, one for Wish and one for User

## Feature list

### Wish management

- Read, Create , Update , Delete

### User management

- Read, Create, Update , Delete

### Router

- Need to support routing (client side vs server side )
- /User routes, Basically all protected client side routing is in the context of the user
- / -> landing page
- /about -> who created the service
- /faq -> how to use it

### Authentication

- Need to support jwt auth

### Serverside

- Can build it with node.js (what about go ?)
- Serve data from server (wishes and users - REST api)
  - /Users (GET and POST)
  - /Wishes (GET and POST)
- Persist data to db

### Data model

|| User || 1 - \* || Wish ||

## Flows

- Add a user by specifying username and email
  - Then navigate to user profile page
