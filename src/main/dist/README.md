# BamBirds

To run the agent, run the command 
```bash
./bin/bambirds -h <hostname> -t <teamid> -p <pathtoprolog>
```

* \<hostname\> - name of the server (default `localhost`)
* \<teamid\> - ID of the BamBirds team (default `1`)
* \<pathtoprolog\> - path to SWI-Prolog (swipl) in your machine (default `/usr/bin/swipl`)

Run
```bash
./bin/bambirds --help 
```
for more options.

Important: 
----------

The path to the "Prolog"-directory must not contain a space character! 