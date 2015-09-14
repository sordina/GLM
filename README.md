# GLM

A small [GridLab-D](http://gridlab-d.sourceforge.net) markup parser.

Currently incomplete, but does work for simple files.

Includes two executables:

* glm2props - List properties found in file
* glm2dot - Output a Dot file for use with GraphViz

Library functions are currently not exposed.

Usage:

		glm2dot [-h] [-e] [FILE]*

Also accepts GLM format on STDIN.


Example:

		bash-3.2$ head IEEE_4_node.glm
		// Exercise 4.1.1

		clock {
			timestamp '2000-01-01 0:00:00';
			timezone EST+5EDT;
		}

		module powerflow {
			solver_method NR;
		}

		bash-3.2$ glm2dot IEEE_4_node.glm

		digraph {
			// Missed entry ["clock"] - noname
			// Missed entry ["module","powerflow"] - powerflow
			"460bc4159" [label="overhead_line_conductor100"];
			"f98a57e6e" [label="overhead_line_conductor101"];
			"3ae695a1a" [label="line_spacing200"];
			"125c9f13f" [label="line_configuration300"];
			"6666bc20c" [label="transformer_configuration400"];
			"164546f60" [label="node1"];
			"89b8519c3" [label="overhead_line"];
			"164546f60" -> "78882aaeb"; // node1 -> node2
			"78882aaeb" [label="node2"];
			"72f1da2f1" [label="transformer23"];
			"78882aaeb" -> "1315e07dc"; // node2 -> node3
			"1315e07dc" [label="node3"];
			"ff93d9775" [label="overhead_line:34"];
			"1315e07dc" -> "6bf7ab42e"; // node3 -> load4
			"6bf7ab42e" [label="load4"];
		}

Graphical output:

		glm2dot -e IEEE_37node.glm | dot -Tpng -o blah.png && open blah.png

![Rendered GLM](http://i.imgur.com/OgqAYTl.png)
