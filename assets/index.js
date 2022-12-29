const {
  Elm
} = require('../src/elm/Main.elm');
const Pointer = require("./pointer.js");

const app = Elm.Main.init({
  node: document.getElementById('application')
});

new Pointer(app);
