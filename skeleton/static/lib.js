// Accessing the property via string literal prevents renaming by javascript minifiers which can cause FFI errors
window['skeleton_lib'] = {
  log: txt => console.log('Received "' + txt + '" from FFI'),
};
