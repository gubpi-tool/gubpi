if (unifReal(0.0, 1.0) <= 0.5) then alice_strength = 10.0 else alice_strength = 5.0 end;
if (unifReal(0.0, 1.0) <= 0.333) then alice_total = 0.5*alice_strength else alice_total = alice_strength end;
if (unifReal(0.0, 1.0) <= 0.5) then bob_strength = 10.0 else bob_strength = 5.0 end;
if (unifReal(0.0, 1.0) <= 0.333) then bob_total = 0.5*bob_strength else bob_total = bob_strength end;
if (unifReal(0.0, 1.0) <= 0.5) then tom_strength = 10.0 else tom_strength = 5.0 end;
if (unifReal(0.0, 1.0) <= 0.333) then tom_total = 0.5*tom_strength else tom_total = tom_strength end;
if (unifReal(0.0, 1.0) <= 0.5) then sue_strength = 10.0 else sue_strength = 5.0 end;
if (unifReal(0.0, 1.0) <= 0.333) then sue_total = 0.5*sue_strength else sue_total = sue_strength end;
total_alice_bob = alice_total + bob_total;
total_alice_tom = alice_total + tom_total;
total_alice_sue = alice_total + sue_total;
total_bob_tom   = bob_total + tom_total;
total_bob_sue   = bob_total + sue_total;
total_tom_sue   = tom_total + sue_total;

