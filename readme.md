Erlang Cron implementation
============

Supports all standard cron fields (minutes, hours, day of month, month, day of week) and symbols: * , - /

Tested on Erlang/OTP 18, Erlang/OTP 19, Erlang/OTP 20; rebar / rebar3.

## Compile
```
$ rebar compile
```

## EUnit tests
```
$ rebar eunit
```

## Run
```
$ erl -pa ebin/
```

## Examples
**1**  Add cron job (anonymous function).
```
application:start(erl_cron).
erl_cron_server:add_job({"*/2 * * * *", fun () -> io:format("job completed!~n", []) end}).
```

**2**  Add cron job (MFA).
```
application:start(erl_cron).
erl_cron_server:add_job({"*/2 * * * *", {m, f, [1, 2, 3]}}).
```

**3**  Add named cron job (anonymous function). Name can be any term.
```
application:start(erl_cron).
erl_cron_server:add_job({"*/2 * * * *", fun () -> io:format("job completed!~n", []) end, <<"JobName">>}).
```

**4**  Add named cron job (MFA). Name can be any term.
```
application:start(erl_cron).
erl_cron_server:add_job({"*/2 * * * *", {m, f, [1, 2, 3]}, jobname}).
```

**5**  Delete cron job by reference.
```
application:start(erl_cron).
{ok, Ref} = erl_cron_server:add_job({"*/2 * * * *", {m, f, [1, 2, 3]}}).
erl_cron_server:delete_by_ref(Ref).
```

**6**  Delete cron job by name.
```
application:start(erl_cron).
erl_cron_server:add_job({"*/2 * * * *", {m, f, [1, 2, 3]}, "jobname"}).
erl_cron_server:delete_by_name("jobname").
```

**7**  Delete all cron jobs.
```
erl_cron_server:delete_all().
```

**8** Get scheduled jobs.
```
erl_cron_server:get_jobs().
```

**9** Get only next run date by Cron expression.
```
erl_cron_time:get_next_run_date("* 2 * 4 *").
erl_cron_time:get_next_run_date("* 14 * * *").
erl_cron_time:get_next_run_date("*/2 * * * *").
erl_cron_time:get_next_run_date("10-20 7,10-20 3 2 2-4").
erl_cron_time:get_next_run_date("6,9-20/2 7,10-20 3 2 2-4").
erl_cron_time:get_next_run_date("6,9-20/2 7,8,10-20 */3 2,3 2-4").
```