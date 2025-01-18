## Infinite scroll

this simple example shows how to achieve infinite scroll using ambiorix + htmx.

the key is **pagination**: only loading the next set of table rows when all
the previous ones are revealed on the browser.

for demo purposes, we have used the dataset `babynames` from [{babynames}](https://github.com/hadley/babynames). it has lots of observations and is perfect for this example.

in practice though, you will mostly use infinite scroll in conjunction with database pagination.

## demo

![infinite-scroll-demo](./infinite-scroll.gif)
