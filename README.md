Just a crazy idea to implement immutable persistent browser-based text editor with reagent.
Main idea is to use persistent tree to store document text and highlighting model.
Now the main goal is to make react reconcile only lines that have changed since last render.
