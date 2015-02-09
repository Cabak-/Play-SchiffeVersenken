
function init(fs, bl){
    var fieldSize = fs; // size of the field
    var boatLength = bl; // length of the boat
    var boatStart = null; // first selected coordinate of the boat
}

/** function to submit data via POST */
function submitData(path, params) {
    var form = document.createElement("form");
    form.setAttribute("method", "POST");
    form.setAttribute("action", path);

    for(var key in params) {
        if(params.hasOwnProperty(key)) {
            var hiddenField = document.createElement("input");
            hiddenField.setAttribute("type", "hidden");
            hiddenField.setAttribute("name", key);
            hiddenField.setAttribute("value", params[key]);
            form.appendChild(hiddenField);
        }
    }

    document.body.appendChild(form);
    form.submit();
}

/** getElementsByClassName function (not available for all browsers) */
function getElementsByClassName(node,classname) {
    if (node.getElementsByClassName) { // use native implementation if available
        return node.getElementsByClassName(classname);
    } else {
        return (function getElementsByClass(searchClass,node) {
            if ( node == null )
                node = document;
            var classElements = [],
                els = node.getElementsByTagName("*"),
                elsLen = els.length,
                pattern = new RegExp("(^|\\s)"+searchClass+"(\\s|$)"), i, j;

            for (i = 0, j = 0; i < elsLen; i++) {
                if ( pattern.test(els[i].className) ) {
                    classElements[j] = els[i];
                    j++;
                }
            }
            return classElements;
        })(classname, node);
    }
}

/** function to place a boat by selecting two coordinates */
function placeBoat(x,y) {
    var player = '@session.get("id").getOrElse(null)';
    if (boatStart == null) {
        boatStart = new Array(x,y);
    } else {
        if (x == boatStart[0] && y == boatStart[1] ) { // same coordinates: remove the first point
            boatStart = null ;
        } else if ( x == boatStart[0] && Math.abs(y - boatStart[1]) < boatLength) { // vertical placement
            if (y > boatStart[1]) {
                if (boatStart[1] + boatLength - 1 >= fieldSize) return;
            } else {
                if (boatStart[1] - boatLength + 1 < 0) return;
                boatStart[1] = boatStart[1] - boatLength + 1;
            }
            submitData('@routes.Application.placeBoat',{'player': player, 'x': boatStart[0], 'y': boatStart[1], 'boatLength': boatLength, 'orientation': 'v'});
        } else if ( y == boatStart[ 1 ] && Math.abs(x - boatStart[0]) < boatLength) { // horizontal placement
            if (x > boatStart[0]) {
                if (boatStart[0] + boatLength - 1 >= fieldSize) return;
            } else {
                if (boatStart[0] - boatLength + 1 < 0) return;
                boatStart[0] = boatStart[0] - boatLength + 1;
            }
            submitData('@routes.Application.placeBoat',{'player': player, 'x': boatStart[0], 'y': boatStart[1], 'boatLength': boatLength, 'orientation': 'h'});
        }
    }
}

/** mouseover-function for the boat-placement field */
function placementMouseEvent(element,x,y) {
    resetHighlights();
    if (boatStart == null) {
        element.style.backgroundColor = "#00F";
    } else {
        if (x == boatStart[ 0 ] && y == boatStart[ 1 ] ) { // same coordinates: remove the first point
            element.style.backgroundColor = "#F00";
        } else if ( x == boatStart[ 0 ] && Math.abs(y - boatStart[1]) < boatLength) { // vertical placement
            var first = boatStart[1];
            var last = boatStart[1] + boatLength - 1;
            if (y < boatStart[1]) {
                first = boatStart[1] - boatLength + 1;
                last = boatStart[1];
            }
            if (first < 0 || first >= fieldSize || last < 0 || last >= fieldSize) return
            for ( var i=first ; i <= last; i ++ ) {
                document.getElementById("own_cell_"+x+"_"+i).style.backgroundColor = "#0F0";
            }
        } else if ( y == boatStart[ 1 ] && Math.abs(x - boatStart[0]) < boatLength) { // horizontal placement
            var first = boatStart[0];
            var last = boatStart[0] + boatLength - 1;
            if (x < boatStart[0]) {
                first = boatStart[0] - boatLength + 1;
                last = boatStart[0];
            }
            if (first < 0 || first >= fieldSize || last < 0 || last >= fieldSize) return
            for ( var i=first ; i <= last; i ++ ) {
                document.getElementById("own_cell_"+i+"_"+y).style.backgroundColor = "#0F0";
            }
        }
    }
}

/** reset all the cell colors */
function resetHighlights() {
    var elements = getElementsByClassName(document, "own_cell");
    for (var i = 0; i < elements.length; i++) {
        if (boatStart != null && elements[i].id == "own_cell_"+boatStart[0]+"_"+boatStart[1]) {
            elements[i].style.backgroundColor = "#FF0" ;
        } else {
            elements[i].style.backgroundColor = "#DDD" ;
        }
    }
}

/** function to shoot at a coordinate */
function shoot(x,y) {
    var player = '@session.get("id").getOrElse(null)';
    //alert('Shoot at '+x+','+y);
    submitData('@routes.Application.shoot',{'player': player, 'x': x, 'y': y})
}