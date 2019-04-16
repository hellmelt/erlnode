function sayHello(world) {
  return `Hello ${world}, JS here!`;
}


let globalTime;
function setGlobalTime(time) {
  globalTime = time;
}

function sayGood() {
  return new Promise((res) => {
    setTimeout(() => {
      res(`Good${globalTime}, JS here!`)}, 150);
    })
}

function quit() {
  return new Promise((res) => {
    setTimeout(() => {
      res({a: 'ok'})}, 8000);
  })
}

module.exports = { sayHello, setGlobalTime, sayGood, quit };
