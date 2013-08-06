var margin = {top: 10, right: 200, bottom: 10, left: 40},
    width  = (getParam("width") || 1400) - margin.left - margin.right,
    height = (getParam("height") || 800) - margin.top - margin.bottom;
    offset_right = 800;
    offset_buttom = 300;

var svg = d3.select("body")
            .append("svg")
            .attr("width", width + margin.left + margin.right + offset_right)
            .attr("height", height + margin.top + margin.bottom + width/10 + offset_buttom);

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
             .size([height, width + height/10]);

// === scales =============================================

var x_scale = function(x){
  var threshold = 500;
  var scale = 5;

  if(x < threshold){
    return (x * 1.0)/scale;
  }else{
    return x - threshold + threshold/scale;
  }
}

var diagonal = d3.svg.diagonal()
                     .projection(function(d) { return [x_scale(d.y), d.x + d.y / 10]; });

var color = d3.scale.category20();
var rails_directory_hue = d3.scale.ordinal()
                                  .domain([null, "controller", "model", "view", "helper", "lib", "vendor", "config", "js", "stylesheet", "db"])
                                  .rangeBands([0, 360]);

var icon = function(name){
  switch(name){
    case "is_filter_def": return "public/images/filter.png";
    default: return null;
  }
}

// === main =============================================

function setupJSON(json){
  if(json.is_action){
    json.children = [];
  }

  if(json.children){
    _.each(json.children, function(json_){ setupJSON(json_) });
  }
}

d3.json("input.json", function(json) {
  setupJSON(json);

  var nodes = tree.nodes(json);
  var links = tree.links(nodes);

  var font_size = 10;
  var icon_size = 13;

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
           var offset_x = 0; //(-1) * d3.max(texts, function(t){ return font_size * (t || "").length }) / 4;
           var offset_y = (-1) * font_size / 2;

           var text = d3.select(this)
                        .append("text")
                        .attr("class", "label")
                        .attr("x", function(d){ return offset_x; })
                        .attr("y", function(d, i){ return offset_y + font_size*(i+1); })
                        .attr("fill", "#000")
                        .attr("font-family", "trebuchet ms, helvetica, sans-serif")
                        .attr("text-anchor", "start")
                        .attr("font-size", font_size)
                        .text(function(t){
                           return d.label || (((d.fname && d.lnum) ? (d.fname + ":" + d.lnum) : '') + " --- " + d.corners);
                        })
                        .on("mouseover", function(){
                            return tooltip.style("visibility", "visible");
                        })
                        .on("mousemove", function(){
                            var inner = ""
                            _.each(d.around_text || [], function(arr){
                              inner += "<tr class='" + (d.lnum == arr[0] ? 'current' : '') + "'>"
                                    + "<td class='lnum'>" + arr[0] + "</td>"
                                    + "<td class='content'><pre>" + arr[1] + "</pre></td>"
                                    + "</tr>"
                            });
                            return tooltip.style("top", (d3.event.pageY + 10) + "px")
                                          .style("left", (d3.event.pageX - 100) + "px")
                                          .html("<table>" + inner + "</table>");
                        })
                        .on("mouseout", function(){
                            return tooltip.style("visibility", "hidden");
                        })
                        .on("click", function(text){
                           $('#fname-lnum-for-copy').val(d.fname + ":" + d.lnum);
                        });

           var real_box_width  = d3.max(text[0].map(function(e){ return e.getBBox().width; }));
           var real_box_height = d3.sum(text[0].map(function(e){ return e.getBBox().height; }));

           d3.select(this)
             .insert("rect", "text.label")
             .attr("width", function(e){  return real_box_width })
             .attr("height", function(e){ return real_box_height })
             .attr("rx", 3)
             .attr("ry", 3)
             .style("fill", function(e){ return d3.hsl(rails_directory_hue(d.rails_directory), 1, 0.5) })
             .attr("x", function(e){ return offset_x })
             .attr("y", function(e){ return offset_y })
             .attr("fill-opacity", function(e){ return 0.3 })
             .style("stroke-width", "0");

           // -- icon -------------------------
           d3.select(this)
             .append("image")
             .attr("x", "-" + icon_size)
             .attr("y", "-" + icon_size/2)
             .attr("width", icon_size)
             .attr("height", icon_size)
             .attr("opacity", 0.3)
             .attr("xlink:href", function(e){ return d.is_filter_def ? icon("is_filter_def") : "" });
      })
      .attr("transform",  function(d){ return "translate(" + x_scale(d.y) + ",  " + (d.x + d.y / 10) + ")"; });

    var tooltip = d3.select("body")
                    .append('div')
                    .attr('class', 'tooltip');
});
