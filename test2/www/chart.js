
	var width = 960,
		height = 700,
		radius = (Math.min(width, height) / 2) - 10;

	var formatNumber = d3.format(",d");

	var x = d3.scale.linear()
			  .range([0, 2 * Math.PI]);

	var y = d3.scale.sqrt()
			  .range([0, radius]);

	var color = function(d) {
			var colors;
			if (!d.parent) {
				colors = d3.scale.category20c()
                    .domain(d3.range(0,10));
                    d.color = "#fff";
							} 
			else if (d.children) {
				var startColor = d3.hcl(d.color)
                                    .darker(),
                    endColor   = d3.hcl(d.color)
                                    .brighter();
                                    colors = d3.scale.linear()
                        .interpolate(d3.interpolateHcl)
                        .range([
                            startColor.toString(),
                            endColor.toString()
                        ])
                        .domain([0,d.children.length+1]);

            }
            if (d.children) {d.children.map(function(child, i) {
                    return {value: child.value, idx: i};
                }).sort(function(a,b) {
                    return b.value - a.value
                }).forEach(function(child, i) {
                    d.children[child.idx].color = colors(i);
                });
            }

            return d.color;
        };

	var partition = d3.layout.partition()
					  .value(function(d) { return d.size; });

	var arc = d3.svg.arc()
			    .startAngle(function(d) { return Math.max(0, Math.min(2 * Math.PI, x(d.x))); })
				.endAngle(function(d) { return Math.max(0, Math.min(2 * Math.PI, x(d.x + d.dx))); })
				.innerRadius(function(d) { return Math.max(0, y(d.y)); })
				.outerRadius(function(d) { return Math.max(0, y(d.y + d.dy)); });

	var svg = d3.select("body").append("svg")
				.attr("width", width)
				.attr("height", height)
				.append("g")
				.attr("transform", "translate(" + width / 2 + "," + (height / 2) + ")")
				.attr("align","center");
				
	d3.json("sub_genre.json", function(error, root) 
		{
			if (error) throw error;

			svg.selectAll("path")
			   .data(partition.nodes(root))
			   .enter().append("path")
			   .attr("d", arc)	   
			   .style("fill", color)
			   .on("click", click)
			   .append("title")
			   .text(function(d) { return d.name + "\n" + "Number of tracks: " + formatNumber(d.size); });
		}
	);

	function click(d) {
			svg.transition()
			   .duration(750)
			   .tween("scale", function()
			   {
					var xd = d3.interpolate(x.domain(), [d.x, d.x + d.dx]),
					yd = d3.interpolate(y.domain(), [d.y, 1]),
					yr = d3.interpolate(y.range(), [d.y ? 20 : 0, radius]);
					return function(t) 
					{ 
						x.domain(xd(t)); y.domain(yd(t)).range(yr(t)); 
					};
				})
				
				.selectAll("path")
				.attrTween("d", function(d) { return function() { return arc(d); }; });
		}
	
d3.select(self.frameElement).style("height", height + "px");
