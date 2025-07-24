class Lib {
  constructor(app) {
    this.app = app;
    this.timeChannel = null;
    this.init();
  }
  async init() {
    try {
      this.timeChannel= new BroadcastChannel('time_channel');

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

export function initializeLibrary(app) {
  new Lib(app);
}
