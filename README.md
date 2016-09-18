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

[![https://gyazo.com/36cb4838496c036934fcc6d9702e66d8](https://i.gyazo.com/36cb4838496c036934fcc6d9702e66d8.png)](https://gyazo.com/36cb4838496c036934fcc6d9702e66d8)

### User Registration

* It **MUST** be the same as the your email address that is using on Slack and Github.
* Email's public setting is **SHOULD** be public on Github.
 * You can also manually register with the register command.

More informations.

* It collect the id, name and email from Slack, when Slack User change the status to active from away.
* It collect the account and email, when Pull Request URL is pasted on Slack channel that is joined preminder.

### Pull Request

To the registration of the reviewer, you **MUST** be in the following format description.

```markdown
- [ ] @soranoba

* [ ] @soranoba
```

* String after @ character, **MUST** be a login id of Github.
* When your review finished, you **MUST** put a check in this check box.
* In order to update the information of preminder, you **SHOULD** use the bot of Github.
 * Update of the informations is only when the Pull Request URL is pasted on Slack.

### Reminder

If you use the remind method of Slack and send a mention to preminder, it acts as a reminder.

## License

[MIT License](LICENSE)
