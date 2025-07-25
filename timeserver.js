class TimeServer {
  constructor(app) {
    this.app = app;
    this.timeChannel = null;
    this.init();
  }
  async init() {
    try {
      this.timeChannel = new BroadcastChannel('time_channel');
      this.app.ports.responseTime.subscribe(time => {
        this.timeChannel.postMessage(time);
      });
      this.timeChannel.onmessage = (event) => {
        this.app.ports.timeRequested.send(null);
      };
    } catch (error) {
      console.error('Error initializing Timeserver:', error);
    }
  }
}

export function initializeTimeServer(app) {
  new TimeServer(app);
}
