Erlang Cron implementation
============

Supports all standard cron fields (Minutes, Hours, Day of month, Month, Day of week, Year) and symbols: * , - /

Tested on Erlang/OTP 18.

## Compile
```
$ rebar compile
```

## Examples
**1**  Add cron job (anonymous function)

```
application:start(cron).
cron_server:add_job({"*/2 * * * *", fun () -> io:format("job completed!~n", []) end}).
```
**2**  Add cron job (MFA)
```
application:start(cron).
cron_server:add_job({"*/2 * * * *", m, f, [1, 2, 3]}).
```

**3** Get scheduled jobs
```
cron_server:get_jobs().
```

**4** Get only next run date by Cron expression
```
time:get_next_run_date("* 2 * 4 *").
time:get_next_run_date("* 14 * * *").
time:get_next_run_date("*/2 * * * *").
time:get_next_run_date("10-20 7,10-20 3 2 2-4").
time:get_next_run_date("6,9-20/2 7,10-20 3 2 2-4").
time:get_next_run_date("6,9-20/2 7,8,10-20 */3 2,3 2-4").
```