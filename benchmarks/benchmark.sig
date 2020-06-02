signature BENCHMARK =
sig
    val run : int (* size *) -> int (* reps *) ->
	        {user_time : real,
		 sys_time : real,
		 gc_time : real,
		 total_time : real,
		 clock_time : real}
end
