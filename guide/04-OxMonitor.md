Chapter 3: Traffic Monitoring
=============================

In this exercise, you will write a controller that measures the volume
of Web traffic on a network. To implement monitoring efficiently, you
will learn how to read the traffic [statistics] that OpenFlow switches
maintain. You will compose your new traffic monitor with the
[repeater] and [firewall] you wrote in earlier exercises.

As usual, you will proceed in two steps: you will first write and test
a traffic monitoring function, and then implement it efficiently uses
flow tables and OpenFlow statistics.

### The Monitoring Function

Use `Monitor.ml` as a template for this exercise.

Your monitor must count the total number of packets _sent to port 80 and
received from_ port 80. Since the monitoring function receives all
packets, you can maintain a global counter and increment it each time
the `packet_in` function receives a new HTTP packet:

```ocaml
let num_http_packets = ref 0

let packet_in (sw : switchId) (xid : xid) (pktIn : packetIn) : unit =
  if is_http_packet (parse_payload pktIn.payload) then
    begin
        num_http_packets := !num_http_packets + 1;
        Printf.printf "Seen %d HTTP packets.\n%!" !num_http_packets
    end
```

#### Programming Task

Write the `is_http_packet` predicate, using the [packet accessors] you used
to build the firewall.

You're not just monitoring Web traffic. You need to block ICMP traffic
and use route non-ICMP traffic, as you did before. In fact, you should
_use the `packet_in` function from `Firewall.ml` verbatim_.

#### Building and Testing Your Monitor

You should first test that the your monitor preserves the features of the
firewall and repeater. To do so, you'll run the same tests you in the previous
chapter. You will also test the monitor by checking that traffic to and from
port 80 increments the counter (and that other traffic does not).

- Build and launch the controller:

  ```shell
  $ make Firewall.d.byte
  $ ./Firewall.d.byte
  ```

- In a separate terminal window, start Mininet using the same
  parameters you've used before:

  ```
  $ sudo ./mn --controller=remote --topo=single,3 --mac
  ```

- Test that the firewall correctly drops pings, reporting `100% packet loss`:

  ```
  mininet> h1 ping h2
  mininet> h2 ping h1
  ```

- Test that Web traffic is unaffected, but logged. To do so, you will
   run a Web server on one host and a client on another:

  * In Mininet, start new terminals for `h1` and `h2`:

    ```
    mininet> h1 xterm &
    mininet> h2 xterm &
    ```

  * In the terminal for `h1` start a local Web server:

    ```
    # cd ~/src/frenetic/guide
    # python -m SimpleHTTPServer
    ```

  * In the terminal for `h2` fetch a web page from `h1`:

    ```
    # curl 10.0.0.1/index.html
    ```

    This command should succeed and in the controller's terminal, you 
    should find that HTTP traffic is logged.

    > fill in output

- Finally, you should test to ensure that other traffic is neither
  blocked by the firewall nor counted by your monitor. To do so, kill the
  Web server running on `h2` and start it on a non-standard port (e.g., 8080):

  * On the terminal for `h2`:

    ```
    $ python -m SimpleHTTPServer 8080 .
    ```

  * On the terminal for h1, fetch a page:

    ```
    $ curl 10.0.0.2 8080
    ```

  The client should successfully download the page. Furthermore, in the
  controller terminal, you should find that no traffic is logged during
  this connection.


### Efficient Monitoring

Switches themselves keeps track of the number of packets (and bytes)
it receives.  To implement an efficient monitor, you will use
OpenFlow's [statistics] API to query these counters.

### Example: Counting Dropped ICMP Packets

In this section, we describe an extension to the [firewall] that also counts
the number of packets it drops. This example will help you build your own
monitoring controller.

Recall that the firewall installs two rules in the flow table:

```ocaml
let switch_connected (sw : switchId) : unit =
  send_flow_mod sw 0l (FlowMod.add_flow hi icmp_pat []);
  send_flow_mod sw 0l (FlowMod.add_flow lo all_pat [Output AllPorts])
```

Each rule is associated with a packet-counter that counts the number of packets
on which the rule is applied. For example, if 10 ICMP packets are blocked and
50 other packets are not blocked, the counters would look as follows:

<table>
<tr>
  <th>Priority</th> <th>Pattern</th> <th>Action</th> <th>Counter (bytes)</th>
</tr>
<tr>
  <td>hi</td><td>ICMP</td><td>drop</td><td>10</td>
</tr>
  <td>50</td><td>ALL</td><td>Output AllPorts</td><td>50</td>
</tr>
</table>

To count the number of ICMP packets, we need to read the first counter. We can
read the counter using the [send_stats_request] function. This function
can be used issue several different kinds of statictics requests.

The easiest way to use this API is to issue an `AggregateRequest` in this form:

```ocaml
send_stats_request switch xid (Stats.AggregateRequest (pattern, 0, None))
```
Above, `switch` is the switch to query, `xid` is a unique identifier that is
returned in the reply, and `pattern` is the pattern to match.

In response to this request, the switch will response with an
[aggregateStats] reply, which is sent to the `stats_reply` handler:

```ocaml
let stats_reply (sw : switchId) (xid : xid) (stats : Stats.reply) : unit =
  match stats with
  | Stats.AggregateFlowRep stat ->
    Printf.printf "Blocked %Ld ICMP packets.\n%!" stats.Stats.total_packet_count
  | _ -> Printf.printf "unexpected stats reply.\n%!"
```

For the complete example, see [OxCountingFirewall.ml]. It has 

> TODO(arjun): fill in.

### Efficiently Monitoring Web Traffic

- Although Web traffic is forwarded in exactly the same manner as non-ICMP
  traffic, you need to create separate, higher priority rules to match Web
  traffic, so that you have counters that match Web traffic exclusively.

- You cannot write a single OpenFlow pattern that matches both HTTP
  requests and replies. You need to match they separately, thus you need two
  rules, which gives you two counters. Therefore, you need to issue two statistics
  requests and calculate their sum.

- Have your monitor print the number of HTTP packets whenever you receive
  updated statistics in the `stats_reply` function.

- However, each call to `stats_reply` will provide either the number of HTTP
  requests or the number of HTTP responses --- not both.

- Therefore, when you receive updated statistics on HTTP requests, you
  need to have remember the last statistics received on HTTP
  responses. Similarly, you the last known statistics on HTTP requests
  when you receive an update on HTTP rsponses.

- Create two variables to hold the latest statistics on HTTP requests
  and HTTP responses.

- When you receive new statistics, you must update the appropriate value,
  and print their sum.

- Use the following template

  > FILL

- Bonus: Have we forgetten anything?



[repeater]: [./02-OxRepeater.md]

[firewall]: [./03-OxFirewall.md]

[statistics]: [http://frenetic-lang.github.io/frenetic/docs/OpenFlow0x01_Stats.html]

[send_stats_request]: [http://frenetic-lang.github.io/frenetic/docs/OOx.OxPlatform.html#VALsend_stats_request]
