Erlang Cron implementation
============

Supports all standard cron fields (Minutes, Hours, Day of month, Month, Day of week) and symbols: * , - /

Tested on Erlang/OTP 18, Erlang/OTP 19, Erlang/OTP 20.

## Compile
```
$ rebar compile
```

## Run
```
$ erl -pa ebin/
```

## Examples
**1**  Add cron job (anonymous function).
```
application:start(cron).
cron_server:add_job({"*/2 * * * *", fun () -> io:format("job completed!~n", []) end}).
```

**2**  Add cron job (MFA).
```
application:start(cron).
cron_server:add_job({"*/2 * * * *", {m, f, [1, 2, 3]}}).
```

**3**  Add named cron job (anonymous function). Name can be any term.
```
application:start(cron).
cron_server:add_job({"*/2 * * * *", fun () -> io:format("job completed!~n", []) end, <<"JobName">>}).
```

**4**  Add named cron job (MFA). Name can be any term.
```
application:start(cron).
cron_server:add_job({"*/2 * * * *", {m, f, [1, 2, 3]}, jobname}).
```

**5**  Delete cron job by reference.
```
application:start(cron).
{ok, Ref} = cron_server:add_job({"*/2 * * * *", {m, f, [1, 2, 3]}}).
cron_server:delete_by_ref(Ref).
```

**6**  Delete cron job by name.
```
application:start(cron).
cron_server:add_job({"*/2 * * * *", {m, f, [1, 2, 3]}, "jobname"}).
cron_server:delete_by_name("jobname").
```

**7**  Delete all cron jobs.
```
cron_server:delete_all().
```

**8** Get scheduled jobs.
```
cron_server:get_jobs().
```

**9** Get only next run date by Cron expression.
```
time:get_next_run_date("* 2 * 4 *").
time:get_next_run_date("* 14 * * *").
time:get_next_run_date("*/2 * * * *").
time:get_next_run_date("10-20 7,10-20 3 2 2-4").
time:get_next_run_date("6,9-20/2 7,10-20 3 2 2-4").
time:get_next_run_date("6,9-20/2 7,8,10-20 */3 2,3 2-4").
```