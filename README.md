
<p>
<strong>Email</strong>: 	   l.taylor@bowdoin.edu<br>
<strong>Bluesky</strong>:   @liamtaylor.bsky.social<br>
<strong>Website</strong>:   https://ltaylor.mmm.page/<br>
<br>
Computational model system for analyzing the behavior and reproduction of seabird pair-bonds. Currently hosts code analyzing the energetics and schedules of two storm-petrel parents and an egg across the incubation season. Currently parameterized for Leach's Storm-Petrels (<i>Hydrobates leucorhous</i>) in the Northwest Atlantic.

Note - the C++ code includes a sweep for influences of SD foraging conditions on hatching success, etc. These results are not analyzed in the current version of the manuscript, because the foraging intake distribution is censored at 0 (i.e., no negative draws allowed) -- as a result, high SDs and low means become equivalent to increasing means. There are multiple ways to disentangle those effects, which are themselves interesting, but require changing the underlying foraging distribution in the code, and are therefore left for future work.
</p>

---

<h3>Instructions</h3>
<p>
The C++ source code is compiled in <code>src/</code> with <code>make</code>
<br><br>
In the <code>src/</code> directory, run the compiled program with <code>./lhsp</code>
<br>
Important user/testing settings are found at the top of <code>src/main.cpp</code>
<br>
Simulation output (big file) is written to <code>Output/</code> directory
<br>
An example slurm script (for running simulations on HPC) is provided in <code>lhsp.sh</code>
<br><br>
Process the simulation output in chunks with <code>R/process_simulation_results.r</code>
<br>
Processed output is written as a separate file (<code>Output/processed_results.csv</code>)
<br><br>
Analyze results with <code>R/analysis.r</code>
<br>
Descriptive statistics logged in <code>Output/</code>
<br>
Plot images saved to <code>Plots</code>