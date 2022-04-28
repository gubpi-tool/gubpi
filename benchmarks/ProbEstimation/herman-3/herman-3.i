p = 0.5;

process1 = 0.0;
process2 = 0.0;
process3 = 0.0;

if (unifReal(0,1) <= p)
then
	process1 = 1.0
end;
if (unifReal(0,1) <= p)
then
	process2 = 1.0
end;
if (unifReal(0,1) <= p)
then
	process3 = 1.0
end;

sum = process1 + process2 + process3;
if (sum < 1.0)
then
	notStable = 1.0
else
	if (sum > 1.0)
	then
		notStable = 1.0
	else
		notStable = 0.0
	end
end;	

count = 0;
while (notStable > 0.5) do
      oldprocess1 = process1;
      oldprocess2 = process2;
      oldprocess3 = process3;
      
      if (process1 <= oldprocess3) 
      then
		if (process1 >= oldprocess3)
		then	     
			     if (unifReal(0,1) <= p)
      			     then
				     process1 = 0.0
      			     else
			             process1 = 1.0
     		             end
		else
			process1 = oldprocess3
		end
      else
		process1 = oldprocess3
      end;

      if (process2 <= oldprocess1) 
      then
		if (process2 >= oldprocess1)
		then	     
			     if (unifReal(0,1) <= p)
      			     then
				     process2 = 0.0
      			     else
			             process2 = 1.0
     		             end
		else
			process2 = oldprocess1
		end
      else
		process2 = oldprocess1
      end;

      if (process3 <= oldprocess2) 
      then
		if (process3 >= oldprocess2)
		then	     
			     if (unifReal(0,1) <= p)
      			     then
				     process3 = 0.0
      			     else
			             process3 = 1.0
     		             end
		else
			process3 = oldprocess2
		end
      else
		process3 = oldprocess2
      end;

      sum = process1 + process2 + process3;
      if (sum < 1.0)
      then
		notStable = 1.0
      else
		if (sum > 1.0)
		then
			notStable = 1.0
		else
			notStable = 0.0
		end
      end;
      
      count = count + 1
end;




