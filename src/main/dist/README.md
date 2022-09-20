# BamBirds

## Prerequisities

- Some JRE/JDK  >= 8, <= 17 (Application was compiled with JDK 8)
- SWI-Prolog >= 8.2.4
- On unix:
  - glibc >= 2.27 (i.e. debian buster / ubuntu bionic / CentOS 8 or later versions)

> Windows is currently not fully supported (advanced rebound calculations will not work)!

## Execution

To run the agent, run the command 
```bash
./bin/bambirds -h <hostname> -t <teamid> -s <pathtoswipl>
```

* \<hostname\> - name of the server (default `localhost`)
* \<teamid\> - ID of the BamBirds team (default `1`)
* \<pathtoswipl\> - path to SWI-Prolog (swipl) in your machine (default `swipl` from `PATH`)

Run
```bash
./bin/bambirds --help 
```
for more options.
