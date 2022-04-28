MAX = 500; 
curValue = unifInt(0;500); 
tgtValue = unifInt(0;500); 
count = 0;
exitCond = 0; 
while ( exitCond <= 0 ) do
  delta = (tgtValue - curValue);
  print curValue;
  print tgtValue;
  print delta;
  d = (delta + unifInt(-20;20));
  curValue = (curValue + d);

  if (curValue > MAX) then
      curValue = MAX end;

  if (curValue < 1) then
      curValue = 1 end;

  print curValue;
  print tgtValue;
  print delta;
  print count;
  count = (count + 1);
  if ( -5 <= (curValue - tgtValue))
  then
	if ( (curValue - tgtValue) <= 5)
	then 
	      exitCond = 1
	end
  end;
  if ( count >= 10) then
     exitCond = 1
  end
end;


