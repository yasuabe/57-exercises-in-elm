import { initializeIndexedDB } from './indexeddb.js';

window.initializeApp = function () {
  const app = Elm.Main.init({
    node: document.getElementById('app'),
  });

  console.log('App initialized: port = ' + JSON.stringify(app.ports));

  initializeIndexedDB(app);

  return app;
};