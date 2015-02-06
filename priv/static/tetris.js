function Board(rows, cols) {
    this.rows = rows;
    this.cols = cols;
    this.board = [];

    for ( var i = 0; i < rows; i++ ) {
        this.board[i] = [];
        for ( var j = 0; j < cols; j++) {
            this.board[i][j] = 0;
        }
    }
}

Board.prototype.render = function(canvas) {
    var ctx = canvas.getContext('2d');
    var width = canvas.width;
    var height = canvas.height;
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

var board = new Board(20, 10);
board.board[5][5] = true;
board.render(document.getElementsByTagName("canvas")[0]);


$(document).ready(function () {
    var bullet = $.bullet('ws://127.0.0.1:8080/ws');
    bullet.onopen = function(){
        console.log('bullet: opened');
    };
    bullet.ondisconnect = function(){
        console.log('bullet: disconnected');
    };
    bullet.onclose = function(){
        console.log('bullet: closed');
    };
    bullet.onmessage = function(e){
        alert(e.data);
    };
    // bullet.onheartbeat = function(){
    //     bullet.send('ping');
    // };
});
