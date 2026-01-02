#!/bin/bash

# OCaml Server Manager
# Usage: ./manage_ocaml.sh [start|stop|restart|status|log]

WORKSPACE_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
OCAML_DIR="$WORKSPACE_ROOT/ocaml"
DATA_DIR="$WORKSPACE_ROOT/data"
DB_PATH="$DATA_DIR/wkbl.db"
LOG_FILE="$OCAML_DIR/ocaml_server.log"
PORT=8081

# Ensure DB path is absolute for OCaml
export WKBL_DB="$DB_PATH"

function start() {
    if lsof -i :$PORT > /dev/null; then
        echo "⚠️  OCaml server is already running on port $PORT"
    else
        echo "🚀 Starting OCaml server on port $PORT..."
        echo "   DB: $WKBL_DB"
        cd "$OCAML_DIR" && nohup ./_build/default/bin/main.exe > "$LOG_FILE" 2>&1 &
        sleep 2
        if lsof -i :$PORT > /dev/null; then
            echo "✅ Server started!"
        else
            echo "❌ Failed to start server. Check logs:"
            tail -n 5 "$LOG_FILE"
        fi
    fi
}

function stop() {
    PID=$(lsof -t -i :$PORT)
    if [ -z "$PID" ]; then
        echo "Running nothing on port $PORT"
    else
        echo "🛑 Stopping OCaml server (PID $PID)..."
        kill -9 $PID
        echo "✅ Stopped."
    fi
}

function status() {
    if lsof -i :$PORT > /dev/null; then
        PID=$(lsof -t -i :$PORT)
        echo "🟢 OCaml server is RUNNING (PID $PID, Port $PORT)"
        curl -s "http://localhost:$PORT/health"
        echo ""
    else
        echo "⚪️ OCaml server is STOPPED"
    fi
}

function log() {
    echo "📜 Tailing logs (Ctrl+C to stop)..."
    tail -f "$LOG_FILE"
}

case "$1" in
    start)   start ;;
    stop)    stop ;;
    restart) stop; sleep 1; start ;;
    status)  status ;;
    log)     log ;;
    *)       echo "Usage: $0 {start|stop|restart|status|log}" ;;
esac
