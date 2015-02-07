function Transport(board) {
    this.bullet = $.bullet('ws://127.0.0.1:8080/ws');
    this.bullet.onopen = function(){
        console.log('bullet: opened');
    };
    this.bullet.ondisconnect = function(){
        console.log('bullet: disconnected');
    };
    this.bullet.onclose = function(){
        console.log('bullet: closed');
    };
    this.bullet.onmessage = function(e){
        board.onmessage($.parseJSON(e.data));
    };
    // bullet.onheartbeat = function(){
    //     bullet.send('ping');
    // };
}

function Board(rows, cols, canvas) {
    this.transport = new Transport(this);
    this.rows = rows;
    this.cols = cols;
    this.board = [];
    this.canvas = canvas;

    for ( var i = 0; i < rows; i++ ) {
        this.board[i] = [];
        for ( var j = 0; j < cols; j++) {
            this.board[i][j] = 0;
        }
    }
}

Board.prototype.onmessage = function(msg) {
    console.log(msg);
    if (msg.command == 'add_block') {
        console.log(msg.data[1], msg.data[0]);
        this.board[msg.data[1]][msg.data[0]] = 1;
        this.render();
    } else if (msg.command == 'reset_board') {
        console.log("Resetting board by server request");
        for ( var y = 0; y < this.rows; y++ ) {
            for ( var x = 0; x < this.rows; x++ ) {
                this.board[y][x] = msg.board[this.cols * y + x];
            }
        }
        this.render();
    } else {
        console.log("Unknown command", msg);
    }
};

Board.prototype.keydown = function(key) {
};

Board.prototype.render = function() {
    var ctx = this.canvas.getContext('2d');
    var width = this.canvas.width;
    var height = this.canvas.height;
    var block_width = width / this.cols;
    var block_height = height / this.rows;

    ctx.clearRect(0, 0, width, height);
    ctx.fillStyle = 'red';
    for (var y = 0; y < this.rows; y++) {
        for (var x = 0; x < this.cols; x++) {
            if ( this.board[y][x] ) {
                ctx.fillRect( block_width * x, block_height * y, block_width - 1, block_height - 1 );
            }
        }
    }
};

$(document).ready(function () {
    var board = new Board(20, 10, document.getElementsByTagName("canvas")[0]);
    board.board[7][7] = 1;
    board.render();
});
