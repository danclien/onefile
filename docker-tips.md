Random collection of things I've tried to do in Docker

## Make files created in a container owned by the current user instead of `root`

From: https://github.com/moby/moby/issues/3206#issuecomment-152682860

Set the user/group when creating the container:

```
docker run ... --user $(id -u):$(id -g) ...
```

## Use `--mount` instead of `-v`/`--volume`

From: https://docs.docker.com/engine/admin/volumes/bind-mounts/#choosing-the--v-or-mount-flag

Example:
```
docker run -d \
  -it \
  --name devtest \
  --mount type=bind,source="$(pwd)"/target,target=/app \
  nginx:latest
```


