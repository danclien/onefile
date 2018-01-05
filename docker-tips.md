Random collection of things I've tried to do in Docker

## Make files created in a container owned by the current user instead of `root`

From: https://github.com/moby/moby/issues/3206#issuecomment-152682860

Set the user/group when creating the container:

```
docker run ... --user $(id -u):$(id -g) ...
```
