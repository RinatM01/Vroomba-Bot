# Vroomba: The Magical Cleaning Robot

In this project, I developed a series of algorithms for navigating a Vroomba robot across arbitrary spaces so it could clean them. I strived to minimise the number of “moves” the Vroomba needs to make it to do its job.

A room is represented by a two-dimensional rectilinear polygon with all coordinates being integer values. Vroomba occupies one square 1x1, and its position is formally defined to be the bottom left corner of this square. Vroomba instantly cleans the space in the square it is located. In addition to that, its mechanical brushes can clean the eight squares adjacent to its current position.
