WORDS="p@55W0rd1234
supersecret
bondjamesbond007
wonderw0m4|\|
14m4h4ck3r
motdepasse1234
qwertyuiopasdfghjklzxcvbnm
azertyuiopqsdfghjklmwxcvbn
azertyazerty
merdeaceluiquilit
la_securite_c'est_pour_les_nuls
joliechoupinette63
beaugossedu75
lisael
lelundic'estpourri
CestPasLeLundiQueTuHais"

sflock -h -c $(shuf -n 1 <<< $WORDS)
