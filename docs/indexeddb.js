class IndexedDBManager {
  constructor(appInstance) {
    this.app = appInstance;
    this.db = null;
    this.dbName = 'ExercisesForProgrammersDB';
    this.storeName = 'ex57Store';
    this.init();
  }
  async init() {
    try {
      this.db = await this.openDatabase();
      this.setupPortSubscriptions();
    } catch (error) {
      console.error('Error initializing IndexedDB:', error);
    }
  }
  openDatabase() {
    return new Promise((resolve, reject) => {
      const request = indexedDB.open(this.dbName, 1);
      request.onupgradeneeded = (event) => {
        const db = event .target .result;
        db.createObjectStore('ex57Store', { keyPath: 'id', autoIncrement: true });
      };
      request.onsuccess = (event) => {
        resolve(event.target.result);
      };
      request.onerror = (event) => {
        reject(event.target.error);
      };
    });
  }
  createTransaction(mode = 'readwrite') {
    return this.db.transaction([this.storeName], mode).objectStore(this.storeName);
  }
  subscribeToPort(portName, callback) {
    this.app.ports[portName].subscribe(callback);
  }
  setupPortSubscriptions() {
    this.subscribeToPort('writeToIndexedDB', (data) => {
      const req = this.createTransaction().add({
        data: data,
        timestamp: new Date().toISOString()
      });
      req.onsuccess = () => {
        this.app.ports.indexedDBResult.send('written: ' + data);
      };
    });
    this.subscribeToPort('readFromIndexedDB', () => {
      const req = this.createTransaction('readonly').getAll();
      
      req.onsuccess = () => {
        const records = req.result;
        this.app.ports.indexedDBReadResult.send(records);
      };
    });
    this.subscribeToPort('deleteTodo', (todoId) => {
      const req = this.createTransaction().delete(todoId)

      req.onsuccess = () => {
        this.app.ports.deleteTodoResult.send("deleted: " + todoId);
      };
    });
  };
}

window.initializeApp = function() {
  const app = Elm.Main.init({
    node: document.getElementById('app'),
  })
  console.log('App initialized: port = ' + JSON.stringify(app.ports));
  new IndexedDBManager(app);
  return app;
};