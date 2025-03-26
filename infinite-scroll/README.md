## Infinite scroll

this simple example shows how to achieve infinite scroll using ambiorix + htmx.

the key is **pagination**: only loading the next set of table rows when all
the previous ones are revealed on the browser.

for demo purposes, we have used the dataset `babynames` from [{babynames}](https://github.com/hadley/babynames). it has lots of observations and is perfect for this example.

in practice though, you will mostly use infinite scroll in conjunction with database pagination.

## demo

![infinite-scroll-demo](./infinite-scroll.gif)

## deployment

### `.Renviron`

set these env vars when deploying:

```
APP_ENV = prod
APP_BASE_PATH = /infinite-scroll
```

setting the `APP_BASE_PATH` variable is only important if you're deploying
the app at a sub-path.

for example, if the app is deployed at `https://try.ambiorix.dev/infinite-scroll`,
the env var `APP_BASE_PATH` should be set to `/infinite-scroll`.

### Docker

- build docker image:

    ```
    sudo docker build -t infinite-scroll .
    ```

- start services:

    ```
    docker compose up -d --remove-orphans
    ```

    The app is now accessible on the host machine at **port 3000**.

- stop services in this context:

    ```
    sudo docker compose down
    ```
