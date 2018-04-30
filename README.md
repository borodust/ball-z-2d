# Ball Z: Second Dimension

Game with balls. Even more of them.

[Lisp Game Jam 2018](https://itch.io/jam/lisp-game-jam-2018) entry.

# How To Play

Hold `spacebar` to power up and release it to unleash the force, pushing Master Bawl in the
direction of your cursor.

Bump into Red Balls with high velocity to increase Master Bawl's patience and give you more time
to finish the level.


# Running

## Binaries

* On [itch.io](https://borodust.itch.io/ball-z-second-dimension#download)
* GitHub [Releases](https://github.com/borodust/ball-z-2d/releases)


## From sources via Quicklisp

```lisp
(ql-dist:install-dist "http://bodge.borodust.org/dist/org.borodust.bodge.txt" :prompt nil :replace t)

(ql:quickload :ball-z-2d)

(ball-z-2d:run)

```
