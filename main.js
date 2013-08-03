var margin = {top: 10, right: 200, bottom: 10, left: 40},
    width  = (getParam("width") || 1400) - margin.left - margin.right,
    height = (getParam("height") || 800) - margin.top - margin.bottom;
    offset_right = 300;

var svg = d3.select("body")
            .append("svg")
            .attr("width", width + margin.left + margin.right)
            .attr("height", height + margin.top + margin.bottom + width/10);

var base = svg.append("g")
              .attr("transform", "translate(" + margin.left + ", " + margin.top + ")");

var tree = d3.layout.tree()
             .separation(function(a, b) { return (a.fname == b.fname) ? 1 : 0.8 })
             .children(function(a){
               var result_before_limit = a.children.filter(function(e){
                 var ex_pattern = getParam("exclude");
                 var in_pattern = getParam("include");

                 function match(pattern){
                   return (new RegExp(pattern, 'i')).test(e.fname);
                 }

                 return (!in_pattern || match(in_pattern)) && (!ex_pattern || !match(ex_pattern));
               })

               var remainders_num = _.max([result_before_limit.length - getParam("limit"), 0]);
               var remainders = (0 < remainders_num) ? [{
                 "name": "remainders", "label": ("rem:" + remainders_num), "children": []
               }] : [];

               return result_before_limit.slice(0, getParam("limit") || Infinity).concat(remainders);
             })
             .size([height, width + height/10 - offset_right]);

var diagonal = d3.svg.diagonal()
                     .projection(function(d) { return [d.y, d.x + d.y / 10]; });

var color = d3.scale.category20();
var rails_directory_hue = d3.scale.ordinal()
                                  .domain([null, "controller", "model", "view", "helper", "lib", "vendor", "config", "js", "stylesheet", "db"])
                                  .rangeBands([0, 360]);
// var gray_scale = d3.scale.linear().domain([0, 255]).range(["red", "blue"]);

d3.json("input.json", function(json) {
  var nodes = tree.nodes(json);
  var links = tree.links(nodes);

  var font_size = 9;

  base.selectAll("path.link")
     .data(links)
     .enter()
     .append("path")
     .attr("class", "link")
     .attr("d", function(d){
         return diagonal(d);
      });

  base.selectAll("g.textnode")
      .data(nodes)
      .enter()
      .append("g")
      .each(function(d){
           var texts = [d.label || (d.corners + " " + ((d.fname && d.lnum) ? (d.fname + ":" + d.lnum) : ''))];
           var offset_x = 0; //(-1) * d3.max(texts, function(t){ return font_size * (t || "").length }) / 4;
           var offset_y = (-1) * (font_size * texts.length) / 2;

           var text = d3.select(this).selectAll("text.label")
                        .data(texts)
                        .enter()
                        .append("text")
                        .attr("class", "label")
                        .attr("x", function(d){ return offset_x; })
                        .attr("y", function(d, i){ return offset_y + font_size*(i+1); })
                        .attr("fill", "#000")
                        .attr("font-family", "trebuchet ms, helvetica, sans-serif")
                        .attr("text-anchor", "start")
                        .attr("font-size", font_size)
                        .text(function(t){ return t });

           var real_box_width  = d3.max(text[0].map(function(e){ return e.getBBox().width; }));
           var real_box_height = d3.sum(text[0].map(function(e){ return e.getBBox().height; }));

           d3.select(this)
             .insert("rect", "text")
             .attr("width", function(e){  return real_box_width })
             .attr("height", function(e){ return real_box_height })
             .attr("rx", 3)
             .attr("ry", 3)
             //.style("fill", function(e){ return gray_scale(strToCode(d.search_word || "", 255)) })
             .style("fill", function(e){ return d3.hsl(rails_directory_hue(d.rails_directory), 1, 0.5) })
             .attr("x", function(e){ return offset_x })
             .attr("y", function(e){ return offset_y })
             .attr("fill-opacity", function(e){ return 0.3 })
             .style("stroke-width", "0");
      })
      .attr("transform",  function(d){ return "translate(" + d.y + ",  " + (d.x + d.y / 10) + ")"; });
});
