#!/usr/bin/env bash
# WKBL launchd service management
set -euo pipefail

PLIST_NAME="com.jeongsik.wkbl-refresh.plist"
PLIST_SRC="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/launchd/${PLIST_NAME}"
PLIST_DST="$HOME/Library/LaunchAgents/${PLIST_NAME}"
DISABLED_DST="${PLIST_DST}.disabled"

usage() {
    echo "Usage: $0 {status|enable|disable|reload|run-now|logs}"
    echo ""
    echo "Commands:"
    echo "  status   - Show service status"
    echo "  enable   - Enable and load the service"
    echo "  disable  - Unload and disable the service"
    echo "  reload   - Reload the service configuration"
    echo "  run-now  - Trigger an immediate run"
    echo "  logs     - Show recent logs"
}

status() {
    echo "=== WKBL Refresh Service Status ==="
    echo ""

    # Check source plist
    if [[ -f "$PLIST_SRC" ]]; then
        echo "✅ Source plist exists: $PLIST_SRC"
    else
        echo "❌ Source plist missing: $PLIST_SRC"
    fi

    # Check destination
    if [[ -f "$PLIST_DST" ]]; then
        echo "✅ LaunchAgent installed: $PLIST_DST"
    elif [[ -f "$DISABLED_DST" ]]; then
        echo "⚠️  LaunchAgent disabled: $DISABLED_DST"
    else
        echo "❌ LaunchAgent not installed"
    fi

    # Check if loaded
    if launchctl list 2>/dev/null | grep -q "com.jeongsik.wkbl-refresh"; then
        echo "✅ Service is loaded"
        launchctl list com.jeongsik.wkbl-refresh 2>/dev/null || true
    else
        echo "⚠️  Service is NOT loaded"
    fi

    echo ""
    echo "=== Data Freshness ==="
    python3 "$(dirname "${BASH_SOURCE[0]}")/wkbl_health_check.py" --quiet 2>/dev/null || python3 "$(dirname "${BASH_SOURCE[0]}")/wkbl_health_check.py"
}

enable_service() {
    echo "Enabling WKBL refresh service..."

    # Remove disabled version if exists
    if [[ -f "$DISABLED_DST" ]]; then
        rm -f "$DISABLED_DST"
        echo "Removed disabled plist"
    fi

    # Copy/update plist
    cp "$PLIST_SRC" "$PLIST_DST"
    echo "Installed: $PLIST_DST"

    # Load service
    launchctl unload "$PLIST_DST" 2>/dev/null || true
    launchctl load "$PLIST_DST"
    echo "✅ Service enabled and loaded"

    # Verify
    if launchctl list 2>/dev/null | grep -q "com.jeongsik.wkbl-refresh"; then
        echo "✅ Verified: service is running"
    else
        echo "⚠️  Warning: service may not be running"
    fi
}

disable_service() {
    echo "Disabling WKBL refresh service..."

    # Unload if loaded
    if launchctl list 2>/dev/null | grep -q "com.jeongsik.wkbl-refresh"; then
        launchctl unload "$PLIST_DST" 2>/dev/null || true
        echo "Unloaded service"
    fi

    # Rename to disabled
    if [[ -f "$PLIST_DST" ]]; then
        mv "$PLIST_DST" "$DISABLED_DST"
        echo "✅ Service disabled: $DISABLED_DST"
    else
        echo "Service was not installed"
    fi
}

reload_service() {
    echo "Reloading WKBL refresh service..."
    disable_service 2>/dev/null || true
    enable_service
}

run_now() {
    echo "Triggering immediate refresh..."
    if launchctl list 2>/dev/null | grep -q "com.jeongsik.wkbl-refresh"; then
        launchctl start com.jeongsik.wkbl-refresh
        echo "✅ Triggered via launchctl"
    else
        echo "Service not loaded, running directly..."
        "$(dirname "${BASH_SOURCE[0]}")/run_wkbl_refresh.sh"
    fi
}

show_logs() {
    LOG_FILE="$HOME/me/logs/wkbl_refresh.log"
    LAUNCHD_LOG="$HOME/me/logs/wkbl_refresh.launchd.log"

    echo "=== Recent Refresh Logs ==="
    if [[ -f "$LOG_FILE" ]]; then
        tail -50 "$LOG_FILE"
    else
        echo "No refresh log found at $LOG_FILE"
    fi

    echo ""
    echo "=== Launchd Log ==="
    if [[ -f "$LAUNCHD_LOG" ]]; then
        tail -20 "$LAUNCHD_LOG"
    else
        echo "No launchd log found"
    fi
}

case "${1:-}" in
    status)
        status
        ;;
    enable)
        enable_service
        ;;
    disable)
        disable_service
        ;;
    reload)
        reload_service
        ;;
    run-now)
        run_now
        ;;
    logs)
        show_logs
        ;;
    *)
        usage
        exit 1
        ;;
esac
