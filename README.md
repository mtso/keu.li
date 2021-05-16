keuli2
=====

An OTP application

Build
-----

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