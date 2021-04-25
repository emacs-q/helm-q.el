 system"cp ~/.helm-q/instances-meta.json ~/bak/"   / backup
 t:gen each 3000 5000 8000 10000     / generate json for each record count

gen:{[n]
 system"S 42";
 s:` sv' flip (n?`6;n?`6;n?`hdb`rdb`tp);
 f:hsym .Q.dd[`$"/home/dk/.helm-q/instances-meta"]n,`json;
 f 0:enlist .j.j t:flip `address`env`region`service!(n#enlist":5000";n?`uat`prod;n?`amrs`emea`apac;s);
 t}
