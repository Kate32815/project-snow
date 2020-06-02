signature BENCHMARKS =
sig
  val run : int list * int list * int list * int list * int list -> unit
end

functor Benchmarks (structure List : RANDOM_ACCESS_LIST) : BENCHMARKS =
struct
  structure B1 = Benchmark1(structure List = List)
  structure B2 = Benchmark2(structure List = List)
  structure B3 = Benchmark3(structure List = List)
  structure B4 = Benchmark4(structure List = List)
  structure B5 = Benchmark5(structure List = List)

  val reps = 30

  fun test f (n:int) =
    let val {user_time:real,sys_time:real,gc_time:real,total_time:real,clock_time:real} = f n reps
    in
	print "\t";
	print n;
	print " ";
	print user_time;
	print " ";
	print sys_time;
	print " ";
	print gc_time;
	print " ";
	print total_time;
	print " ";
	print clock_time;
	print "\n"
    end

  fun tests f ns = app (test f) ns

  fun run (ns1,ns2,ns3,ns4,ns5) =
      (print "quicksort\n";
       tests B1.run ns1;
       print "sum\n";
       tests B2.run ns2;
       print "lookup\n";
       tests B3.run ns3;
       print "update\n";
       tests B4.run ns4;
       print "histogram\n";
       tests B5.run ns5)
end

structure S = Benchmarks (structure List = StandardList)
structure M = Benchmarks (structure List = MyersList)
structure R = Benchmarks (structure List = RandomAccessList)
structure F = Benchmarks (structure List = OptRandomAccessList)
structure B = Benchmarks (structure List = BraunList)
structure A = Benchmarks (structure List = AVLList)
structure K = Benchmarks (structure List = KaplanTarjanList)
