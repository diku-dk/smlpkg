
signature SUSP =
   sig
      
      type 'a susp

      val delay : (unit -> 'a) -> 'a susp
      val force : 'a susp -> 'a
         
   end
