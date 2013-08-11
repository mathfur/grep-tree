var margin = {top: 10, right: 200, bottom: 10, left: 40},
    width  = (getParam("width") || 1400) - margin.left - margin.right,
    height = (getParam("height") || 800) - margin.top - margin.bottom;
    offset_right = 800;
    offset_buttom = 300;

var base = d3.select("div#base")
            .append("svg")
            .attr("width", width + margin.left + margin.right + offset_right)
            .attr("height", height + margin.top + margin.bottom + width/10 + offset_buttom);

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
    case "is_filter_def": return "images/filter.png";
    case "is_action":     return "images/action.png";
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
  var icon_size = 20;

  base.selectAll("path.link")
     .data(links)
     .enter()
     .append("path")
     .attr("class", "link")
     .attr("data-source", function(d){ return d.source.name; })
     .attr("data-target", function(d){ return d.target.name; })
     .attr("d", function(d){
         return diagonal(d);
      });

  base.selectAll("g.textnode")
      .data(nodes)
      .enter()
      .append("g")
      .each(function(d){
           var node_height = font_size + 8;
           var text = d3.select("div#base")
                        .append("div")
                        .attr("class", "label")
                        .attr("data-name", d.name)
                        .style("top", function(e){  return (d.x + d.y/10) - node_height/2 + "px"; })
                        .style("left", function(e){ return x_scale(d.y)                   + "px"; })
                        .style("height", node_height + "px")
                        .style("font-size", font_size)
                        .style("background-color", function(e){ return d3.hsl(rails_directory_hue(d.rails_directory), 1, 0.5) })
                        .html(function(t){
                           var checkbox = "<input type='checkbox'/>";
                           var delete_button = "<div class='delete genericon genericon-close'></div>";
                           var label = d.label || (((d.fname && d.lnum) ? (d.fname + ":" + d.lnum) : '') + " --- " + d.corners);
                           return checkbox + delete_button + label;
                        })
                        .on("mouseover", function(){
                            return tooltip.style("visibility", "visible");
                        })
                        .on("mousemove", function(){
                            var inner = ""
                            _.each(d.around_text || [], function(arr){
                              inner += "<tr class='" + (d.lnum == arr[0] ? 'current' : '') + "'>"
                                    + "<td class='lnum'>" + arr[0] + "</td>"
                                    + "<td class='content'><pre>" + $('<span/>').text(arr[1]).html() + "</pre></td>"
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
                           $('#fname-lnum-for-copy').val(d.fname + ":" + d.lnum).select();
                        });

           // -- icon -------------------------
           d3.select(this)
             .selectAll(".icon")
             .data(_.compact([
                 d.is_action     ? icon("is_action") : null,
                 d.is_filter_def ? icon("is_filter_def") : null,
             ]))
             .enter()
             .append("image")
             .attr("class", "icon")
             .attr("data-name", d.name)
             .attr("x", function(e, i){ return "-" + icon_size*(i+1) + "px"; })
             .attr("y", "-" + icon_size/2 + "px")
             .attr("width", icon_size + "px")
             .attr("height", icon_size + "px")
             .attr("opacity", 0.7)
             .attr("xlink:href", function(e){ return e });
      })
      .attr("transform", function(d){ return "translate(" + x_scale(d.y) + "px,  " + (d.x + d.y / 10) + "px)"; });

    var tooltip = d3.select("div#base")
                    .append('div')
                    .attr('class', 'tooltip');

    $('.delete').click(function(e){
      var name = $(this).parent().attr("data-name");

      ascendants(json, name, function(n){
        $('[data-name="' + n + '"]').hide();
        console.log(n);
        d3.selectAll('path.link[data-source="' + n + '"]').remove();
        d3.selectAll('path.link[data-target="' + n + '"]').remove();
      });
    });
});
