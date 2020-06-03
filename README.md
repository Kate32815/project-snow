# project-snow
PL Research Report Code

We are exploring random access lists in SML for a class project based on:
Standard ML (version 0.93) source code accompanying
	Chris Okasaki
	"Purely Functional Random-Access Lists"
	FPCA '95

New functions were added in "ralist.sml", under the comment "PL reserach report functions/values". The corresponding entries were added in "ralist.sig".

"benchmarks/benchmark.sml" has been modified to not take in the BENCHMARK
signature, and the "run" function at the end of the file has been commented out,
as the time function it calls does not compile properly.

For now, to use the added functions, run:

- use "ralist.sig";
- use "ralist.sml";
- use "benchmarks/benchmark1.sml";


QUICKSORT FOR RALISTS
To access quicksort for ralists, we need to instantiate the functor, run:
- structure RAL = Benchmark1 (structure List = RandomAccessList);

Now you can use RAL.quicksort to access the quicksort function for ralists.
To generate a random ralist, call
- RAL.randlist n; (n is the length of the list you want)
To convert a random ralist to a standard list, call
- RAL.convert ralst; (where ralst is the one you're converting from)

QUICKSORT FOR STANDARD LISTS
- use "standard.sml";

Here, several errors may pop up - going to "ralist.sig" and commenting out the lines the compiler is raising errors about works as a temporary fix. After commenting out, run this again to update benchmark:
- use "benchmarks/benchmark1.sml";

To access quicksort for standard lists, need to again instantiate the functor:
- structure L = Benchmark1 (structure List = StandardList);

Now you can use L.quicksort to access the quicksort function for standard lists.


(Will definitely be updating this/linking the files so it's not as troublesome)