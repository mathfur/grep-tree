var margin = {top: 10, right: 200, bottom: 10, left: 40},
    width  = 1200 - margin.left - margin.right,
    height = 800 - margin.top - margin.bottom;

var svg = d3.select("body")
            .append("svg")
            .attr("width", width + margin.left + margin.right)
            .attr("height", height + margin.top + margin.bottom + width/10);

var base = svg.append("g")
              .attr("transform", "translate(" + margin.left + ", " + margin.top + ")");

var tree = d3.layout.tree()
             .size([height, width + height/10]);

var diagonal = d3.svg.diagonal()
                     .projection(function(d) { return [d.y, d.x + d.y / 10]; });

var color = d3.scale.category20();

d3.json("foo.json", function(json) {
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
           //var texts = [d.primary_word, d.fname + ":" + d.lnum, d.corners].filter(function(e){ return e });
           var texts = [d.corners + " " + d.fname + ":" + d.lnum];
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
             .style("fill", function(e){ return color(d.search_word) })
             .attr("x", function(e){ return offset_x })
             .attr("y", function(e){ return offset_y })
             .attr("fill-opacity", function(e){ return 0.3 })
             .style("stroke-width", "0");
      })
      .attr("transform",  function(d){ return "translate(" + d.y + ",  " + (d.x + d.y / 10) + ")"; });
});
