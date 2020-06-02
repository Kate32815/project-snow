signature TIME =
   sig
     val time : (unit -> 'b) * int -> {user_time : real,
				       sys_time : real,
				       gc_time : real,
				       total_time : real,
				       clock_time : real}
   end
     
structure Time : TIME =
   struct
      val time = fn (f,n) =>
	  let open System.Timer
	      val extract = fn (TIME {usec,sec}) => (real usec)/1.0E6+real sec
	      val timeofday = System.Unsafe.CInterface.gettimeofday
	      fun loop i = if i<n then (f(); loop (i+1)) else ()
	      val tod = timeofday ()
	      val timer = start_timer()
	      val _ = loop 0
	      val user_time = extract (check_timer timer)
	      val sys_time = extract (check_timer_sys timer)
	      val gc_time = extract (check_timer_gc timer)
	      val clock = extract (sub_time (timeofday(),tod))
	      val total = user_time + sys_time + gc_time
	      val n' = real n
	   in 
	       {user_time = user_time/n',
		sys_time = sys_time/n',
		gc_time = gc_time/n',
		total_time = total/n',
		clock_time = clock/n'}
	   end
   end

	   
           
  
