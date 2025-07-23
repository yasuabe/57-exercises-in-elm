class Lib {
  constructor(app) {
    this.app = app;
  }
} 

export function initializeLibrary(app) {
  new Lib(app);
}
