cartX = unifReal(-6.0;-5.0);
cartY = unifReal(-6.0; -5.0);
steerX = unifReal(-0.75;-0.6);
steerY= unifReal(-0.8;-0.7);
exitCond = 0;
count = 0;
while (exitCond <= 0) do
      count = count +1;
      if (steerX <= 0.6)
      then
	steerX = 0.71 + unifReal(-0.1;0.1);
        steerY = 0.71 + unifReal(-0.1;0.1)
      else 
         if (steerX >= 0.8)
	 then 
	    	steerX = 0.71 + unifReal(-0.1;0.1);
		steerY = 0.71 + unifReal(-0.1;0.1)
	 end
      end;
      if (steerY <= 0.6)
      then
	steerX = 0.71 + unifReal(-0.1;0.1);
        steerY = 0.71 + unifReal(-0.1;0.1)	
      else 
         if (steerY >= 0.8)
	 then 
	    	steerX = 0.71 + unifReal(-0.1;0.1);
		steerY = 0.71 + unifReal(-0.1;0.1)	
	 end
      end;

      cartX = cartX + steerX;
      cartY = cartY + steerY;
      steerX = steerX + unifReal(-0.1;0.1);
      steerY = steerY + unifReal(-0.1; 0.1);
      if (count >= 3)
      then 
      	 exitCond = 1
      end;

      if (cartX <= 1.)
      then 
      	   if (cartX >= -1.)
	   then 
	   	if (cartY <= 1.)
		then 
		     if (cartY >= -1.)
		     then 
		     	  exitCond = 1
		     end
	       end
           end
       end
end;



