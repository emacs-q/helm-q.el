f:.Q.dd[`:test;`pass]
f 0: ("user1:pass";"user2:pass");
/{system"$QHOME/l64/q -p ",x," -u ",y," & >\/dev/null"}'[string 5000 5001;2#enlist 1_ string f];
system"$QHOME/l64/q -p 5000 -u test/pass & >\/dev/null";
system"$QHOME/l64/q -p 5001              & >\/dev/null";

exit 0
