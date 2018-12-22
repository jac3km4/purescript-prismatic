# purescript-spaz
Optical UI component library for Purescript.

# Motivation
This library uses a lens-based approach to compose UI elements, which heavily borrows from combinators defined in [refract](https://github.com/pkamenarsky/purescript-refract).
The main difference between this library and refract is that it also enables you to use actions in response to events to handle more complex scenarios, which I found difficult to be solved with refract.
It uses a subscription model inspired by a Scala library, [diode](https://github.com/suzaku-io/diode). Individual components can listen to updates in parts of the state and also dispatch actions.
Actions are handled asynchronously at the top-level of your application and they can produce state changes and other actions. Actions are meant to be maximally decoupled from the components, components ought to use lens composition only.
