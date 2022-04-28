logScr = unifReal(-0.6,0.6);
age = unifReal(30,80);
isFemale = unifReal(0,1.0);
isAA = unifReal(0,0.55);
f = 4.94;
if (isFemale >= 0.5) then 
    k = -0.357;
    if (logScr <= k)
    then 
       f = f  - 0.329 * ( logScr - k )
    else 
       f = f - 1.209 * ( logScr - k )
    end;
    f = f + 0.017
else 
    k = -0.105;
    if (logScr <= k)
    then 
       f = f  - 0.411 * (logScr - k)
    else 
       f = f - 1.209 * (logScr - k)
    end
end;
if (isAA >= 0.5)
then 
  f = f + 0.148
end;
logScrErr = logScr + unifReal(-0.1,0.1);
ageErr = age + unifReal(-1.0,1.0);
flip1 = unifReal(0,1);
if (flip1 <= 0.01)
then 
     isFemaleErr = 1.0 - isFemale
else 		
     isFemaleErr = isFemale
end;
flip2 = unifReal(0,1);
if (flip2 <= 0.01)
then 
    isAAErr = 1.0 - isAA
else
    isAAErr = isAA
end;
f1 = 4.94;
if (isFemaleErr >= 0.5)
then 
    k = -0.357;
    if (logScrErr <= k)
    then 
       f1 = f1  - 0.329 * (logScrErr - k)
    else 
       f1 = f1 - 1.209 * (logScrErr - k)
    end;
    f1 = f1 + 0.017
else 
    k = -0.105;
    if (logScr <= k)
    then 
       f1 = f1  - 0.411 * (logScrErr - k)
    else 
       f1 = f1 - 1.209 * (logScrErr - k)
    end
end;
if (isAAErr >= 0.5)
then 
  f1 = f1 + 0.148
end;
