'use strict';

function rijksdriehoek(λ, φ) {
  // Coordinates of origin (Amersfoort)
  var x0 = 155000.0;
  var y0 = 463000.0;
  var φ0 = 52.15517440;
  var λ0 =  5.38720621;
  var R = [
    {"p":0, "q":1, "R": 190094.945},
    {"p":1, "q":1, "R": -11832.228},
    {"p":2, "q":1, "R": -114.221},
    {"p":0, "q":3, "R": -32.391},
    {"p":1, "q":0, "R": -0.705},
    {"p":3, "q":1, "R": -2.340},
    {"p":1, "q":3, "R": -0.608},
    {"p":0, "q":2, "R": -0.008},
    {"p":2, "q":3, "R": 0.148}]
  var S = [
    {"p":1, "q":0, "S": 309056.544},
    {"p":0, "q":2, "S": 3638.893},
    {"p":2, "q":0, "S": 73.077},
    {"p":1, "q":2, "S": -157.984},
    {"p":3, "q":0, "S": 59.788},
    {"p":0, "q":1, "S": 0.433},
    {"p":2, "q":2, "S": -6.439},
    {"p":1, "q":1, "S": -0.032},
    {"p":0, "q":4, "S": 0.092},
    {"p":1, "q":4, "S": -0.054}]
  var dφ = 0.36*(180*φ/Math.PI - φ0);
  var dλ = 0.36*(180*λ/Math.PI - λ0);
  var x = x0;
  for (var i = 0; i < R.length; ++i) {
    var p = R[i].p;
    var q = R[i].q;
    var r = R[i].R;
    x += r*Math.pow(dφ, p)*Math.pow(dλ, q);
  }
  var y = y0;
  for (var i = 0; i < S.length; ++i) {
    var p = S[i].p;
    var q = S[i].q;
    var s = S[i].S;
    y += s*Math.pow(dφ, p)*Math.pow(dλ, q);
  }
  return [x, y];
}

rijksdriehoek.invert = function(x, y) {
  // Coordinates of origin (Amersfoort)
  var x0 = 155000.0;
  var y0 = 463000.0;
  var φ0 = 52.15517440;
  var λ0 =  5.38720621;
  var K = [ 
    {"p":0, "q":1, "K":3235.65389}, 
    {"p":2, "q":0, "K": -32.58297}, 
    {"p":0, "q":2, "K":  -0.24750}, 
    {"p":2, "q":1, "K":  -0.84978}, 
    {"p":0, "q":3, "K":  -0.06550}, 
    {"p":2, "q":2, "K":  -0.01709}, 
    {"p":1, "q":0, "K":  -0.00738}, 
    {"p":4, "q":0, "K":   0.00530}, 
    {"p":2, "q":3, "K":  -0.00039}, 
    {"p":4, "q":1, "K":   0.00033}, 
    {"p":1, "q":1, "K":  -0.00012}];
  var L = [
    {"p":1, "q":0, "L":5260.52916}, 
    {"p":1, "q":1, "L": 105.94684}, 
    {"p":1, "q":2, "L":   2.45656}, 
    {"p":3, "q":0, "L":  -0.81885}, 
    {"p":1, "q":3, "L":   0.05594}, 
    {"p":3, "q":1, "L":  -0.05607}, 
    {"p":0, "q":1, "L":   0.01199}, 
    {"p":3, "q":2, "L":  -0.00256}, 
    {"p":1, "q":4, "L":   0.00128}];
  var dx = (x - x0)/1E5;
  var dy = (y - y0)/1E5;
  var φ = φ0;
  for (var i = 0; i < K.length; ++i) {
    var p = K[i].p;
    var q = K[i].q;
    var k = K[i].K;
    φ += k*Math.pow(dx, p)*Math.pow(dy, q)/3600;
  }
  var λ = λ0; 
  for (var i = 0; i < L.length; ++i) {
    var p = L[i].p;
    var q = L[i].q;
    var l = L[i].L;
    λ += l*Math.pow(dx, p)*Math.pow(dy, q)/3600;
  }
  return [180*λ/Math.PI, 180*φ/Math.PI];
};

d3.geo.rijksdriehoek = function() {
  return d3.geo.projection(rijksdriehoek);
}

