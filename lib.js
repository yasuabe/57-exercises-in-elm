class Lib {
  constructor(app) {
    this.app = app;
    this.timeChannel = null;
    this.init();
  }
  async init() {
    try {
      this.timeChannel = new BroadcastChannel('time_channel');

      this.app.ports.requestTime.subscribe(() => {
        this.timeChannel.postMessage('request_time');
      });

      this.timeChannel.onmessage = (event) => {
        this.app.ports.timeReceived.send(event.data);
      };
    } catch (error) {
      console.error('Error initializing Timeserver:', error);
    }
  }
}
class SessionStorage {
  constructor(app) {
    this.app = app;
    this.init();
  }
  async init() {
    this.app.ports.getItem.subscribe((key) => {
      const value = sessionStorage.getItem(key);
      this.app.ports.itemReceived.send(value ? JSON.parse(value) : {});
    });
    this.app.ports.setItem.subscribe(([key, value]) => {
      sessionStorage.setItem(key, JSON.stringify(value));
    });
  }
}

export function initializeLibrary(app) {
  new Lib(app);
  new SessionStorage(app);
}
