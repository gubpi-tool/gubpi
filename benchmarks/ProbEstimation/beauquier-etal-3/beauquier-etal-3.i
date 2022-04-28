p = 0.5;

process1d = 0.0;
process1p = 0.0;
process2d = 0.0;
process2p = 0.0;
process3d = 0.0;
process3p = 0.0;


if (unifReal(0,1) <= p)
then
	process1d = 1.0
end;
if (unifReal(0,1) <= p)
then
	process1p = 1.0
end;
if (unifReal(0,1) <= p)
then
	process2d = 1.0
end;
if (unifReal(0,1) <= p)
then
	process2p = 1.0
end;
if (unifReal(0,1) <= p)
then
	process3d = 1.0
end;
if (unifReal(0,1) <= p)
then
	process3p = 1.0
end;

sum = process1p + process2p + process3p;
if (sum > 1.0)
then
	notStable = 1.0
else
	notStable = 0.0
end;	

count = 0;
while (notStable > 0.5) do
      oldprocess1d = process1d;
      oldprocess1p = process1p;
      oldprocess2d = process2d;
      oldprocess2p = process2p;
      oldprocess3d = process3d;
      oldprocess3p = process3p;
      
      if (oldprocess1d >= oldprocess3d) 
      then
	if (oldprocess1d <= oldprocess3d)
	then
		if (oldprocess1p >= oldprocess3p)
		then
			if (oldprocess1p <= oldprocess3p)
			then
				process1d = 1.0 - oldprocess1d;
				if (unifReal(0,1) <= p)
				then
					process1p = oldprocess1p
				else
					process1p = 1.0 - oldprocess1p
				end
			else
				process1d = 1.0 - oldprocess1d
			end
		else
			process1d = 1.0 - oldprocess1d
		end
	end
      end;

      if (oldprocess2d >= oldprocess1d) 
      then
	if (oldprocess2d <= oldprocess1d)
	then
		if (oldprocess2p >= oldprocess1p)
		then
			if (oldprocess2p <= oldprocess1p)
			then
				process2d = 1.0 - oldprocess2d;
				if (unifReal(0,1) <= p)
				then
					process2p = oldprocess2p
				else
					process2p = 1.0 - oldprocess2p
				end
			else
				process2d = 1.0 - oldprocess2d
			end
		else
			process2d = 1.0 - oldprocess2d
		end
	end
      end;

      if (oldprocess3d >= oldprocess2d) 
      then
	if (oldprocess3d <= oldprocess2d)
	then
		if (oldprocess3p >= oldprocess2p)
		then
			if (oldprocess3p <= oldprocess2p)
			then
				process3d = 1.0 - oldprocess3d;
				if (unifReal(0,1) <= p)
				then
					process3p = oldprocess3p
				else
					process3p = 1.0 - oldprocess3p
				end
			else
				process3d = 1.0 - oldprocess3d
			end
		else
			process3d = 1.0 - oldprocess3d
		end
	end
      end;

      sum = process1p + process2p + process3p;
      if (sum > 1.0)
      then
	notStable = 1.0
      else
	notStable = 0.0
      end;
      
      count = count + 1
end;




