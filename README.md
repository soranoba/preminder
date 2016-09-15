preminder
=============

Review reminder of pull request for Github and Slack.

## Requires

* Erlang/OTP 18.0 or later.

## Build and run

There provides a way using the docker.

The usual way, please refer to [Makefile](Makefile).

```bash
# build
make build
# run : Please use the -v option, if you want to have the data to the outside of the container.
docker run -i -t -d --env USER_DETS_PATH=user.dets \
                    --env PR_DETS_PATH=pr.dets \
                    --env SLACK_TOKEN=xoxb-XXXXXXXX-XXXXXXXXXXXXXXXX \
                    --env GITHUB_ENDPOINT=https://api.github.com \
                    --env GITHUB_TOKEN=XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \
                    --name ${NAME} preminder
docker run -i -t -d --name ${NAME} preminder
# stop : Currently it does not support SIGTERM. So, -t option should be set to 0.
docker stop -t 0

# debug (1)
docker exec -it ${NAME} /bin/bash
root@1c707cbca218:/# erl -sname debug -remsh preminder@$(hostname) -setcookie preminder
(preminder@1c707cbca218)1> q().
root@1c707cbca218:/# exit

# debug (2) : way of detach is [Ctrl + p] and [Ctrl + q]
docker attach ${NAME}
(preminder@1c707cbca218)1>
```

## Configuration

|               |description|
|:--------------|:----------|
|USER_DETS_PATH |File to store the user information|
|PR_DETS_PATH   |File to store the pull request information|
|SLACK_TOKEN    |The token of slack. See also: [Bots](https://my.slack.com/services/new/bot)|
|GITHUB_ENDPOINT|Api end point of Github (Enterprise). e.g. `https://api.github.com` |
|GITHUB_TOKEN   |The token of Github (Enterprise). See also: [Personal Access Token](https://github.com/settings/tokens) |

## Usage

[![https://gyazo.com/67c4161b9ac8ed20df37126fabc317e1](https://i.gyazo.com/67c4161b9ac8ed20df37126fabc317e1.png)](https://gyazo.com/67c4161b9ac8ed20df37126fabc317e1)

## License

[MIT License](LICENSE)
