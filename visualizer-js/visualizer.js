"use strict";
var visualizer;
(function (visualizer_1) {
    var Coord = /** @class */ (function () {
        function Coord(input) {
            this.x = input[0];
            this.y = input[1];
        }
        Coord.prototype.scale = function (scale) {
            return new Coord([this.x * scale, this.y * scale]);
        };
        return Coord;
    }());
    var Ship = /** @class */ (function () {
        function Ship(input) {
            this.role = input["role"];
            this.id = input["id"];
            this.position = new Coord(input["position"]);
            this.velocity = new Coord(input["velocity"]);
            this.x4 = input["x4"];
            this.x5 = input["x5"];
            this.x6 = input["x6"];
            this.x7 = input["x7"];
        }
        Ship.prototype.ship_color = function () {
            var isAttacker = this.role == "Attacker";
            return isAttacker ? "rgb(255, 196, 8)" : "rgb(100, 149, 237)";
        };
        return Ship;
    }());
    var ShipWithCommands = /** @class */ (function () {
        function ShipWithCommands(input) {
            this.ship = new Ship(input[0]);
            this.commands = input[1];
        }
        return ShipWithCommands;
    }());
    var Visualizer = /** @class */ (function () {
        function Visualizer(canvas, shipLogs) {
            this.canvas_size = 600;
            this.space_size = 260;
            this.planet_size = 32;
            this.ship_size = 2;
            this.beam_size = 4;
            this.scale = 2;
            canvas.width = this.canvas_size;
            canvas.height = this.canvas_size;
            this.ctx = canvas.getContext('2d');
            this.shipLogs = shipLogs;
        }
        Visualizer.prototype.convert_to_canvas_coord = function (point) {
            point = point.scale(this.scale);
            return new Coord([point.x + this.canvas_size / 2, point.y + this.canvas_size / 2]);
        };
        Visualizer.prototype.draw_square = function (center, side_length, color) {
            this.ctx.fillStyle = color;
            center = this.convert_to_canvas_coord(center);
            side_length *= this.scale;
            this.ctx.fillRect(center.x - side_length / 2, center.y - side_length / 2, side_length, side_length);
        };
        Visualizer.prototype.draw_segments = function (points, color) {
            var _this = this;
            points = points.map(function (point) { return _this.convert_to_canvas_coord(point); });
            this.ctx.strokeStyle = color;
            this.ctx.beginPath();
            this.ctx.moveTo(points[0].x, points[0].y);
            for (var i = 1; i < points.length; i++) {
                this.ctx.lineTo(points[i].x, points[i].y);
            }
            this.ctx.stroke();
        };
        Visualizer.prototype.draw = function (turn) {
            var _this = this;
            var shipLog = this.shipLogs[turn];
            this.ctx.clearRect(0, 0, this.canvas_size, this.canvas_size);
            this.draw_square(new Coord([0, 0]), this.space_size, "rgb(0, 0, 0)");
            this.draw_square(new Coord([0, 0]), this.planet_size, "rgb(255, 255, 255)");
            shipLog.forEach(function (shipWithCommands) {
                var ship = shipWithCommands.ship;
                shipWithCommands.commands.forEach(function (command) {
                    if ("Shot" in command) {
                        var target = new Coord(command["Shot"]["target"]);
                        _this.draw_square(target, _this.beam_size, "rgb(255, 255, 100)");
                        _this.draw_segments([ship.position, target], "rgb(255, 255, 100)");
                    }
                });
            });
            shipLog.forEach(function (shipWithCommands) {
                var ship = shipWithCommands.ship;
                _this.draw_square(ship.position, _this.ship_size, ship.ship_color());
            });
        };
        return Visualizer;
    }());
    visualizer_1.init = function () {
        var logFile = document.getElementById("log_file");
        var loadLogFile = function (file, callback) {
            var statePrefix = "state: ";
            var reader = new FileReader();
            reader.readAsText(file);
            reader.onloadend = function () {
                if (reader.result) {
                    var lines = reader.result.toString().split('\n');
                    var shipLogs_1 = [];
                    var rawLogs_1 = [];
                    lines.forEach(function (line) {
                        if (line.startsWith(statePrefix)) {
                            var state = JSON.parse(line.substring(statePrefix.length));
                            shipLogs_1.push(state["ship_and_commands"].map(function (input) { return new ShipWithCommands(input); }));
                            rawLogs_1.push(state["ship_and_commands"].map(function (input) { return JSON.stringify(input); }));
                        }
                    });
                    callback(shipLogs_1, rawLogs_1);
                }
            };
        };
        var canvas = document.getElementById("canvas");
        var turn = document.getElementById("turn");
        var turn_slider = document.getElementById("turn_slider");
        var server_response = document.getElementById("server_response");
        return function () {
            logFile.files && logFile.files[0] && loadLogFile(logFile.files[0], function (shipLogs, rawLogs) {
                var visualizer = new Visualizer(canvas, shipLogs);
                turn_slider.value = "0";
                turn_slider.max = (shipLogs.length - 1).toString();
                var update_turn = function (turn_number) {
                    turn.innerText = turn_number.toString();
                    visualizer.draw(turn_number);
                    var innerHtml = [];
                    rawLogs[turn_number].forEach(function (rawLog, i) {
                        innerHtml.push("<div style='color: " + shipLogs[turn_number][i].ship.ship_color() + "'>" + rawLogs[turn_number][i] + "</div>");
                    });
                    server_response.innerHTML = "<div>" + innerHtml.join("") + "</div>";
                };
                update_turn(0);
                turn_slider.onchange = function () {
                    update_turn(parseInt(turn_slider.value));
                };
            });
        };
    };
})(visualizer || (visualizer = {}));
window.onload = function () {
    document.getElementById("log_file").onchange = visualizer.init();
};
//# sourceMappingURL=visualizer.js.map