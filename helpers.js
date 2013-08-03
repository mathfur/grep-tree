function getParam(key){
  var pairs = (location.href.split("?")[1] || "").split("&").map(function(pair){ return pair.split("=") });
  return (_.find(pairs, function(pair){ return pair[0] == key }) || [])[1];
}

function strToCode(str, max){
  return +d3.range(str.length).map(function(i){ return str[i].charCodeAt().toString() }).join("") % max
}
