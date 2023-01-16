<h3 align="center">Hypothesis Machine in Shiny</h3>

  <p align="center">
    Here is a project to build a dashboard for doing basic psychology tests with R with nice publishable quality visuals
    <br />
    <a href="[https://github.com/pauLiou/R-Shiny-App]"><strong>Explore the project »</strong></a>
</div>

<!-- ABOUT THE PROJECT -->
## About The Project

![accuracy.png](https://github.com/pauLiou/Chess-Piece-Classifier/blob/main/download.jpg?raw=true)

Classifying chess pieces has an interesting number of factors to consider:

* How is the piece shown to the algorithm? From above? Directly in front?
* Different chess-sets use different models for pieces, for example, some kings have very intricate styling.
* Not all chess sets use black and white pieces, many use different colours.

Of course, not all of these problems are solvable. But it should be possible for a Computer Vision CNN to do a pretty good job of classifying the majority of pieces.

My model managed a very high accuracy after training:

![accuracy.png](https://github.com/pauLiou/Chess-Piece-Classifier/blob/main/accuracy.png?raw=true)

Testing on a validation set also showed promising results. Ultimately ending with around 98% accuracy.

A confusion matrix also showed pretty reliable predictions by the model on an outer sample test:

![output](https://github.com/pauLiou/Chess-Piece-Classifier/blob/main/output.png?raw=true)

All in all it was a successful project. I will use this model now to attempt to train a chess-board detection algorithm.
