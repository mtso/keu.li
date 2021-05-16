크리 keu.li
==========

`keu.li` lets users set up a simple page with social media links so that just
one link can be shared to share all the links :) (exactly like linktree).

Configuration
=============

Runtime variables
-----------------

Update `config/sys.config` with database credentials.

nginx
-----

Assuming the node is running and listening on 127.0.0.1:3000, and nginx is
set up and running.

`/etc/nginx/sites-available/default` is symlinked to `/etc/nginx/sites-enabled/default`.
No need to directly modify `/etc/nginx/nginx.conf`, `include /etc/nginx/sites-enabled/*;`
will include the configs in sites-available/default.

```
server {
        ...
        location / { # Comment out the try_files line and add the proxy lines.
                # First attempt to serve request as file, then
                # as directory, then fall back to displaying a 404.
                # try_files $uri $uri/ =404;
                proxy_pass http://127.0.0.1:3000;

                proxy_set_header Host $host;
                proxy_set_header X-Real-IP $remote_addr;
                proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                proxy_set_header X-Forwarded-Proto $scheme;
        }
}
```

Release
=======

`<bin>/keuli restart` should automatically upgrade if the new version was
set in rebar.config and .app.src in the release compilation.

```sh
$ ./rebar3 release
$ ./_build/default/rel/keuli/bin/keuli restart
```

If not running, spin up the app in the background with `<bin>/keuli daemon`
```sh
$ ./_build/default/rel/keuli/bin/keuli daemon
```

Build
=====

    $ rebar3 compile

    ./rebar3 compile
    ./rebar3 release
    ./rebar3 run

Run in prod:
```sh
<in keuli folder>./_build/default/rel/keuli/bin/keuli daemon
```

Dev
---
local ssh dev
```sh
sshfs -o allow_other,default_permissions,IdentityFile=<absolute path (~ expands to remote fs)>/.ssh/id_rsa root@<ip address>:/ ~/dev/mnt/droplet
umount root@<ip address>:/
```

Data
----
```sql
CREATE TABLE IF NOT EXISTS users (
  id SERIAL PRIMARY KEY,
  create_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  modify_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  username VARCHAR(255),
  display_name VARCHAR(4095),
  display_image_url VARCHAR(4095),
  email VARCHAR(255),
  email_is_verified BOOLEAN DEFAULT FALSE,
  fields JSON,
  UNIQUE(username)
)

INSERT INTO users (username, email, fields) VALUES ('name', 'example@email.com', '{"links":[{"url":"https://instagram.com/name","name":"ig"},{"url":"https://twitch.tv/name","name":"twitch"}]}')
```

Running Dev Server on Production Node
-------------------------------------

Rename the `apps/keuli` directory to `apps/keuli2`, modify the following files,
and then run on a different port like `PORT=1337 ./rebar3 run`.

```
--- a/rebar.config
+++ b/rebar.config

-{relx, [{release, {keuli, "0.1.0"},
-         [keuli,
+{relx, [{release, {keuli2, "0.1.0"},
+         [keuli2,
```

```
--- a/config/sys.config
+++ b/config/sys.config

-  {keuli, [
+  {keuli2, [
```

```
--- a/config/vm.args
+++ b/config/vm.args

- -sname keuli
+ -sname keuli2
```

```
--- a/apps/keuli/src/keuli.app.src %% Note filename change on .app.src file!
+++ b/apps/keuli2/src/keuli2.app.src

- {application, keuli,
+ {application, keuli2,
```

```
--- a/apps/keuli/src/keuli_app.erl
+++ b/apps/keuli2/src/keuli_app.erl

- case code:priv_dir(keuli) of
+ case code:priv_dir(keuli2) of

- PgConf = get_pg_conf(keuli),
+ PgConf = get_pg_conf(keuli2),
```

```
--- a/apps/keuli/src/keuli_style_handler.erl
+++ b/apps/keuli2/src/keuli_style_handler.erl

-    case code:priv_dir(keuli) of
+    case code:priv_dir(keuli2) of
```

