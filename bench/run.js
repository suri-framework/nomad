const WebSocket = require("ws")

let ws = new WebSocket("wss://bazaar.fly.dev/ws")

ws.on("open", () => {
  console.log("opened");
  setInterval(() => {
    ws.send("hello world 🚀")
  }, 500);
})

ws.on("close", () => {
  console.log("closed")
})

ws.on("error", (err) => {
  console.error("error", err);
})

ws.on("message", (msg) => {
  console.log("recv: ", msg.toString());
})
