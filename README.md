# nagios-plugin-vaultaire-throughput

Accesses a Vaultaire telemetry origin and checks the write throughput for
a given origin. The check passes only when the origin has addresses, all of
them can be read, and at least one of them has been written to in the last 10
minutes.

Running the check is straightforward:

````
$ nagios-plugin-vaultaire-throughput -b chateau-02.syd1 \
        -c tcp://chevalier-02.syd1:6283 \
        X14C82 \
        894LRK
OK: Nonzero points written for origin 894LRK | points-written=556727;0;;;
````

For detailed usage instructions use the `--help` command-line option:

```
Usage: nagios-plugin-vaultaire-throughput [-b|--broker-host BROKER-HOST]
                                          [-c|--chevalier-uri CHEVALIER-URI]
                                          TELEMETRY-ORIGIN CHECK-ORIGIN

Available options:
  -h,--help                Show this help text
  -b,--broker-host BROKER-HOST
                           Vault broker host (default: "localhost")
  -c,--chevalier-uri CHEVALIER-URI
                           Chevalier reader URI (default: tcp://localhost:6283)
  TELEMETRY-ORIGIN         Origin Vaultaire telemetry is written to
  CHECK-ORIGIN             Origin for which to check throughput
```


