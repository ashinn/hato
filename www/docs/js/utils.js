
var fadesteps = 6;
var faders = new Object;

function shownote (id, text) {
  var elt = document.getElementById(id);
  elt.className = 'note';
  if (elt.innerHTML != text) {
    faders[id] = new Object;
    elt.innerHTML = text;
  }
}

function hidenote (id) {
  var elt = document.getElementById(id);
  var f = function() {
    if ((! faders[id]) || (! faders[id].fixed)) {
      var i = (parseInt(elt.className.substring(4), 10)||0)+1;
      if (i<=fadesteps) {
        elt.className = elt.className.substring(0,4)+i.toString();
      } else {
        clearInterval(faders[id].intervalId);
        elt.innerHTML = '';
      }
    }
  }
  if ((! faders[id]) || (! faders[id].fixed)) {
    faders[id] = {intervalId: setInterval(f, 2000)};
  }
}

function fixnote (id) {
  var elt = document.getElementById(id);
  elt.className = 'note';
  if (faders[id]) {
    clearInterval(faders[id]);
  } else {
    faders[id] = new Object;
  }
  faders[id].fixed = true;
  return false;
}
