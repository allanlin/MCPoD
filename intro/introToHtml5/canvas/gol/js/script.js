/* NodeMap object
 * Stores state data for the game
 * Written with object notation
 */
function NodeMap(width, height) {
  var i, j;

  //JS loves to muck around with scope. self = this locks the current scope to self.
  self = this;

  self.width = width;
  self.height = height;

  //Initialize arrays. Duck typing is great and all but undefined[0] will throw an exception. Same thing as you gotta do with ruby/python
  self.nodes = [];
  self._oldNodes = [];
  for(i = 0; i < height; i++) {
    self.nodes[i] = [];

    for(j = 0; j < width; j++) {
      self.nodes[i][j] = 0;
    }
  }
  //Array.slice(index) copies array from index to the end instead of assigning the pointer
  self._oldNodes[i] = self.nodes.slice(0);

  self.spawn = function(x, y) {
    self.nodes[y][x] = 1;
  };

  self.kill = function(x, y) {
    self.nodes[y][x] = 0;
  };

  //Grab state from the _oldNodes array. Prevents nodes that were created/killed in the current iteration from affecting the board
  self.node = function(x, y) {
    var temp = self._oldNodes[y];
    if(temp) {
      return self._oldNodes[y][x];
    }
    else {
      return;
    }
  };

  //Find neighbours of a cell and return how many of them are alive. Uses 8-connectivity
  self.neighbours = function(x, y) {
    var tl, t, tr, l, r, bl, b, br, i, tarr, sum = 0;

    tl = self.node(x - 1, y - 1);
    t  = self.node(x, y - 1);
    tr = self.node(x + 1, y - 1);
    l  = self.node(x - 1, y);
    r  = self.node(x + 1, y);
    bl = self.node(x - 1, y + 1);
    b  = self.node(x, y + 1);
    br = self.node(x + 1, y + 1);

    tarr = [tl, t, tr, l, r, bl, b, br];
    for(i = 0; i < 8; i++) {
      sum += tarr[i] || 0;
    }

    return sum;
  };

  //Run one cycle of the board
  self.cycle = function() {
    var i, j, neighbours;

    self._oldNodes = self.nodes.slice(0);

    for(i = 0; i < height; i++) {
      self.nodes[i] = [];

      for(j = 0; j < width; j++) {
        self.nodes[i][j] = 0;
      }
    }

    for(i = 0; i < height; i++) {
      for(j = 0; j < width; j++) {
        neighbours = self.neighbours(j, i);

        if(neighbours == 3) {
          self.spawn(j, i);
        }
        else if(neighbours == 2) {
          self.nodes[i][j] = self.node(j, i);
        }
        else {
          self.kill(j, i);
        }
      }
    }
  };

  //Loads the imageData from the canvas into the nodes array
  self.loadMap = function(imageData) {
    var i, j, k = 0, data = imageData.data;

    for(i = 0; i < height; i++) {
      for(j = 0; j < width; j++) {
        //Threshhold the value. Anything > 0 is considered alive, 0 considered dead
        self.nodes[i][j] = data[k] > 0 ? 1 : 0;

        k += 4;
      }
    }
  };

  //Sets the state data stored in nodes to the pixel data of the canvas
  self.setImageData = function(imageData) {
    var i, j, k = 0, p, data = imageData.data;

    for(i = 0; i < height; i++) {
      for(j = 0; j < width; j++) {
        p = self.nodes[i][j] * 255;
        //Setting the data attribute of ImageData is very slow! need to manipulate it directly
        //ImageData.data is a 1D array of pixel values. Each pixel is 4 ints representing RGBA
        data[k] = p;
        data[k + 1] = p;
        data[k + 2] = p;
        data[k + 3] = 255;

        k += 4;
      }
    }
  };

  return this;
}

/*
* Stage object
* Runs the game
* Written in closure notation (way cooler than object notation)
*/
function Stage() {
  //Grab canvas and context
  var canvas = $('#canvas')[0];
  //The standard allows other contexts to be supported in the future. Right now, only 2d is supported.
  var context = canvas.getContext('2d');

  var width = $('#canvas').attr('width');
  var height = $('#canvas').attr('height');

  //The step function will be executed asynchronously. Using this as a semaphore
  var lock = 0;

  var map = new NodeMap(width, height);

  var imageData = context.getImageData(0, 0, width, height);

  var self =  {
    init: function() {
      map.loadMap(imageData);

      self.draw();
    },

    draw: function() {
      map.setImageData(imageData);
      context.putImageData(imageData, 0, 0);

      return true;
    },

    step: function() {
      if(lock === 0) {
        lock = 1;

        self.createEntropy();
        map.cycle();
        self.draw();

        lock = 0;
      }
    },

    //Sets the pixels on each edge to random values. Ensures that the game never ends
    createEntropy: function() {
      var i, rand;
      for(i = 0; i < width; i++) {
        rand = Math.round(Math.random());
        map.nodes[0][i] = rand;

        rand = Math.round(Math.random());
        map.nodes[height - 1][i] = rand;
      }

      for(i = 0; i < height; i++) {
        rand = Math.round(Math.random());
        map.nodes[i][0] = rand;

        rand = Math.round(Math.random());
        map.nodes[i][width - 1] = rand;
      }
    },

    start: function() {
      //Calls the step function at 60 fps. Calls are done asynchronously, and therefore a step can be called before the last one finishes. The lock is used to prevent this.
      setInterval(self.step, 1000 / 60);
    }
  }

  return self;
}

var stage;
//Runs when domReady event is fired (DOM and script tags are loaded, images not necessarily loaded).
$(function() {
  stage = new Stage();
  stage.init();

  $('#canvas').click(stage.start);
});
