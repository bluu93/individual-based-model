# individual-based-model
This project is adapted from my final project submission for [BIOL 563 - Computer Modeling in Biology](http://catalog.csulb.edu/preview_course.php?catoid=8&coid=68741&print) at California State University, Long Beach, taken in Spring 2024 with Dr. Darren Johnson.
A simulation of predator-prey dynamics of hornets and bees is used to explore the concepts of individual-based modeling.
It is worth noting that although the simulation is not based upon experimental data, there is still reliance on reasonable assumptions that encourage and discourage interactions for: bees-flowers and bees-hornets.

Objects are defined with attributes, actions, and rules that determine behavior over time.

### Initial Attributes:

<ins>Bees</ins>
- Location
- Action (status and activity)
- Health
- Detection radius
- Harvesting time (nectar collected from flowers)

<ins>Hornets</ins>
- Location
- Action (status and activity)
- Health
- Detection radius

<ins>Flowers</ins>
- Location
- Nectar

### Actions:

1. **Fly**: Movement of bees and hornets, defined by a step length and turning angle
2. **Harvest**: Movement of bees around a flower
3. **Swarm**: Movement of bees to a hornet
4. **Surround**: Movement of bees around hornet
5. **Latch**: Movement of hornet around bee
6. **Caught**: Indicates stationary status of hornet/bee
7. **Stay**: Indicates stationary status and death or hornet/bee


### Rules:

<ins>Bees</ins>
1. Bees move around randomly until detecting flower(s).
2. Bees harvest nectar from nearby flowers that are not empty.
3. Bees will swarm and surround a hornet if a bee is caught.
4. Bees will repeat this process while alive.

<ins>Hornets</ins>
1. Hornets move around randomly until detecting bee(s).
2. Hornets will catch a nearby bee.
3. Hornets will repeat this process while alive.



The simulation will run for a length of time and is driven by the bees’ foraging process and interaction(s) with surrounding hornet(s).
