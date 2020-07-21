module visualizer {
    class Coord {
        x: number;
        y: number;

        constructor(input: number[]) {
            this.x = input[0];
            this.y = input[1];
        }

        scale(scale: number) {
            return new Coord([this.x * scale, this.y * scale])
        }
    }

    class Ship {
        role: string;
        id: number;
        position: Coord;
        velocity: Coord;
        x4: number[];
        x5: number;
        x6: number;
        x7: number;

        constructor(input: any) {
            this.role = input["role"];
            this.id = input["id"]
            this.position = new Coord(input["position"]);
            this.velocity = new Coord(input["velocity"]);
            this.x4 = input["x4"];
            this.x5 = input["x5"];
            this.x6 = input["x6"];
            this.x7 = input["x7"];
        }

        ship_color() {
            const isAttacker = this.role == "Attacker";
            return isAttacker ? "rgb(255, 196, 8)" : "rgb(100, 149, 237)";
        }
    }

    class Visualizer {
        private shipLogs: Ship[][];
        private canvas_size:number = 600;
        private space_size = 260;
        private planet_size = 32;
        private ship_size = 2;
        private scale = 2;
        private ctx: CanvasRenderingContext2D;

        constructor(canvas: HTMLCanvasElement, shipLogs: Ship[][]) {
            canvas.width = this.canvas_size;
            canvas.height = this.canvas_size;
            this.ctx = canvas.getContext('2d')!;
            this.shipLogs = shipLogs;
        }



        draw_square(center: Coord, side_length: number, color: string) {
            this.ctx.fillStyle = color;
            center = center.scale(this.scale);
            side_length *= this.scale;
            this.ctx.fillRect(
                center.x + this.canvas_size / 2 - side_length / 2,
                center.y + this.canvas_size / 2 - side_length / 2,
                side_length,
                side_length
            )
        }

        draw(turn: number) {
            const shipLog = this.shipLogs[turn];
            this.ctx.clearRect(0, 0, this.canvas_size, this.canvas_size);
            this.draw_square(new Coord([0, 0]), this.space_size, "rgb(0, 0, 0)");
            this.draw_square(new Coord([0, 0]), this.planet_size, "rgb(255, 255, 255)");
            shipLog.forEach((ship) => {
                this.draw_square(ship.position, this.ship_size, ship.ship_color());
            });
        }
    }

    export const init = () => {
        const logFile = <HTMLInputElement>document.getElementById("log_file");
        const loadLogFile = (file: File, callback: (logs: Ship[][], rawLogs: String[][]) => void) => {
            const statePrefix = "state: ";
            const reader = new FileReader();
            reader.readAsText(file);
            reader.onloadend = function () {
                if (reader.result) {
                    const lines = reader.result.toString().split('\n');
                    const shipLogs: Ship[][] = []
                    const rawLogs: String[][] = [];
                    lines.forEach(line => {
                        if (line.startsWith(statePrefix)) {
                            const state = JSON.parse(line.substring(statePrefix.length));
                            const ships: Ship[] = state["ship_and_commands"]
                                .map((input: any) => new Ship(input[0]));
                            rawLogs.push(state["ship_and_commands"].map((input: any) => JSON.stringify(input)));
                            shipLogs.push(ships);
                        }
                    });
                    callback(shipLogs, rawLogs);
                }
            }
        };


        const canvas = <HTMLCanvasElement> document.getElementById("canvas");
        const turn = <HTMLInputElement>document.getElementById("turn");
        const turn_slider = <HTMLInputElement>document.getElementById("turn_slider");
        const server_response = <HTMLDivElement>document.getElementById("server_response");

        return () => {
            logFile.files && logFile.files[0] && loadLogFile(logFile.files[0], (shipLogs, rawLogs) => {
                const visualizer = new Visualizer(canvas, shipLogs);
                turn_slider.value = "0";
                turn_slider.max = (shipLogs.length - 1).toString();

                const update_turn = (turn_number: number) => {
                    turn.innerText = turn_number.toString();
                    visualizer.draw(turn_number);

                    const innerHtml: String[] = []
                    rawLogs[turn_number].forEach((rawLog, i) => {
                        innerHtml.push(`<div style='color: ${shipLogs[turn_number][i].ship_color()}'>` + rawLogs[turn_number][i] + "</div>")
                    })
                    server_response.innerHTML = "<div>" + innerHtml.join("") + "</div>"
                }

                update_turn(0);
                turn_slider.onchange = () => {
                    update_turn(parseInt(turn_slider.value))
                };
            });
        }
    };
}

window.onload = () => {
    (<HTMLButtonElement>document.getElementById("log_file")).onchange = visualizer.init();
}