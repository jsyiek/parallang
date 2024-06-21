# The Parallang Simulator

## What is this?

The *Parallang Simulator* is a scalable platform-independent solution to simulate parallel message-passing algorithms and evaluate their relative costs. 

*Key features*:
- **Parallang DSL**: The simulator is equipped with a strongly typed interpreted programming language with a C-like syntax called Parallang. Parallang is a portmanteau of "parallel" and "language."
- **Messages**: Through simple `send x -> worker i, j` and `recv x <- worker i, j` instructions, parallel algorithms on the simulator can send messages between processors. Message forwarding is implicitly performed, and the costs are modeled.
- **Data cache model**: The simulator can use an arbitrary user-specified data cache hierarchy. Programmed replacement strategies include LRU, tree-PLRU, and NLU.
- **Latency and memory estimates**: The system estimates running time and tracks memory usage throughout program execution, providing the user with this information at the end.
- **Communication environments**: A modular parameter set system can define communication costs, modeling anything from a multicore system-on-chip to a platform distributed over the internet.
- **Events system**: Latency and memory are tracked through *events* emitted throughout program execution. Users can add real-time event listeners to process these and gauge metrics of interest.
- **Output visualization and graph compression**: Several Jupyter Notebooks provide *graph compression* and *evaluation graph* functionality to visualize the simulator's output.

## What is a parallel algorithm?

To answer this, let's define the *parallel computer abstraction*. Conceptually, a parallel computer is a group of *processing elements* (PEs) linked together in a *message-passing topology*. Each PE executes a sequential algorithm independently and may send *data* directly to any connected PEs during execution: we call this a message. 

A *parallel algorithm* is any algorithm that uses some coordination technique, such as message-passing, to distribute computation tasks over PEs and achieve higher aggregate performance than its sequential counterpart. 

## Tutorials

These tutorials assume you have a *Parallang program* you want to run. If you do not, write one! See the sample code below and in `src/main/parallang`.

### Defining a simple simulation: a bus topology

A simulation is defined in Scala using a straightforward API. Before any simulation, you must set the number of PEs and define the connections. PEs are assumed to be arranged in a 2D grid for ease of use.

```scala
import uk.ac.cam.crsid.Parallang.Interpreter.CommunicationModel.InterconnectionNetwork

object TutorialSimulation {
    def main(args: Array[String]): Unit = {
        val pathToProgram = ...

        // Set up a 10x1 topology: a bus!
        InterconnectionNetwork.resetLengthWidth(10, 1)
        for (i <- 0 until 9) {
            InterconnectionNetwork.addLink((i, 1), (i+1, 1))
            InterconnectionNetwork.addLink((i+1, 1), (i, 1))
        }

        InterconnectionNetwork.launchWith(pathToProgram)
    }
}
```

### Changing the parameter set

By default, the Parallang Simulator uses the `MulticoreComputer` parameter set. Other parameter sets, such as `Datacenter` or `HighPowerInternet`, can be used. Users may also define their own by extending the `LatencyParameterSet` trait.

```scala
import uk.ac.cam.crsid.Parallang.Interpreter.CommunicationModel.{InterconnectionNetwork, HighPowerInternet}

InterconnectionNetwork.setLatencyParameterSet(HighPowerInternet)
```

### Defining an event listener

TBD

### Defining a cache hierarchy

TBD

### Communicating with a running program

TBD

## Sample code in Parallang

Here is an implementation of the Fox-Otto algorithm for solving APSP.

```
fn foxs_general(a: array[array[int]], b: array[array[int]], c: array[array[int]], maxIt: int) -> array[array[int]] {
    var southNeighbor: int = mod(myX+1, q);
    var northNeighbor: int = mod(myX+q-1, q);

    var p: array[array[int]] = array[array[int]](len(a), array[int](len(a[0]), 0));
    var pPrev: array[array[int]] = array[array[int]](len(a), array[int](len(a[0]), 0));

    for (var r: int = 0; r < len(a); r = r + 1) {
        for (var c: int = 0; c < len(a[0]); c = c + 1) {
            p[r][c] = c+scaleFactor_global*myY;
            pPrev[r][c] = c+scaleFactor_global*myY;
        }
    }

    for (var r: int = 0; r < len(a); r = r + 1) {
        for (var col: int = 0; col < len(a[0]); col = col + 1) {
            c[r][col] = a[r][col];
            if (a[r][col] != inf) {
                p[r][col] = r+scaleFactor_global*myX;
                pPrev[r][col] = r+scaleFactor_global*myX;
            }
        }
    }

    for (var it: int = 0; it < maxIt; it = it + 1) {
        printIfMain(-1*it);
        for (var k: int = 0; k < q; k = k + 1) {
            printIfMain(k);
            var bCastProc: int = mod(myX+k, q);
            if (myX == 0) {
            }
            if (bCastProc == myY) {
                send a -> broadcast_row;
                matSquareWithPredecessor(a, b, c, p, pPrev);
            } else {
                recv[array[array[int]]] tempBlock <- worker myX, bCastProc;
                matSquareWithPredecessor(tempBlock, b, c, p, pPrev);
            }
            send b -> worker northNeighbor, myY;
            send pPrev -> worker northNeighbor, myY;
            recv b <- worker southNeighbor, myY;
            recv pPrev <- worker southNeighbor, myY;
        }

        var temp: array[array[int]] = pPrev;
        pPrev = p;
        p = temp;

        for (var r: int = 0; r < len(a); r = r + 1) {
            for (var col: int = 0; col < len(a[0]); col = col + 1) {
                p[r][col] = pPrev[r][col];
                a[r][col] = c[r][col];
            }
        }
        b = a;
    }
```

## Origins

This code was produced for my undergraduate dissertation for the *Computer Science* tripos at the *University of Cambridge*: "Evaluation of parallel routing algorithms." 

I examined five parallelizations of solutions to the *all-pairs shortest paths* problem: Cannon's algorithm and the Fox-Otto algorithm for min-plus matrix exponentiation, the Floyd-Warshall algorithm, the distance vector algorithm, and the Bellman-Ford algorithm. The simulator was created to accomplish analysis at scale and be free of the variability in the underlying host computer. My results used road-network datasets and demonstrated that *superlinear speedup* could be achieved for the first three, that the Bellman-Ford algorithm was the fastest, and that the distance vector algorithm is abysmally inefficient. 

My supervisor was *Dr Jagdish Modi*, who suggested the project. I achieved a high class I mark on this project.
