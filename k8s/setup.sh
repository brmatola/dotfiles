#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

###############################################################################
# Colima                                                                       #
###############################################################################

setup_colima() {
    echo "==> Setting up Colima..."

    if colima status &>/dev/null; then
        echo "    Colima is already running"
        echo "    To apply config changes: colima stop && colima start"
    else
        # Copy config before first start
        mkdir -p "$HOME/.colima/default"
        cp "$SCRIPT_DIR/colima.yaml" "$HOME/.colima/default/colima.yaml"
        echo "    Copied colima.yaml to ~/.colima/default/"

        echo "    Starting Colima..."
        colima start
    fi

    # Verify Docker is accessible
    if ! docker info &>/dev/null; then
        echo "    ERROR: Docker not accessible. Check Colima status."
        exit 1
    fi
    echo "    Docker is ready"
}

###############################################################################
# Local Registry                                                               #
###############################################################################

setup_registry() {
    echo "==> Setting up local container registry..."

    if docker inspect kind-registry &>/dev/null; then
        if [ "$(docker inspect -f '{{.State.Running}}' kind-registry)" = "true" ]; then
            echo "    Registry already running on localhost:5001"
            return
        fi
        echo "    Starting existing registry container..."
        docker start kind-registry
    else
        echo "    Creating registry on localhost:5001..."
        docker run -d --restart=always -p "127.0.0.1:5001:5000" --network bridge --name kind-registry registry:2
    fi
    echo "    Registry ready on localhost:5001"
}

###############################################################################
# Kind Cluster                                                                 #
###############################################################################

setup_kind() {
    local cluster_name="twiglylabs-local"
    echo "==> Setting up Kind cluster: $cluster_name"

    if kind get clusters 2>/dev/null | grep -q "^${cluster_name}$"; then
        echo "    Cluster already exists"
        kubectl cluster-info --context "kind-${cluster_name}" &>/dev/null \
            && echo "    Cluster is healthy" \
            || echo "    WARNING: Cluster exists but may be unhealthy"
    else
        echo "    Creating cluster from kind-cluster.yaml..."
        kind create cluster --config "$SCRIPT_DIR/kind-cluster.yaml"
        echo "    Cluster created"
    fi

    # Connect registry to Kind network
    if ! docker network inspect kind | grep -q kind-registry; then
        echo "    Connecting registry to Kind network..."
        docker network connect kind kind-registry 2>/dev/null || true
    fi

    # Tell Kind nodes about the local registry
    kubectl apply -f - <<EOF
apiVersion: v1
kind: ConfigMap
metadata:
  name: local-registry-hosting
  namespace: kube-system
data:
  localRegistryHosting.v1: |
    host: "localhost:5001"
    help: "https://kind.sigs.k8s.io/docs/user/local-registry/"
EOF

    # Patch CoreDNS to use Google DNS for external resolution
    echo "    Patching CoreDNS for external DNS..."
    kubectl get configmap coredns -n kube-system -o json \
        | sed 's|forward . /etc/resolv.conf|forward . 8.8.8.8 8.8.4.4|' \
        | kubectl apply -f - 2>/dev/null
    kubectl rollout restart deployment coredns -n kube-system
    kubectl rollout status deployment coredns -n kube-system --timeout=60s

    echo "    Cluster ready: kind-${cluster_name}"
}

###############################################################################
# Status                                                                       #
###############################################################################

show_status() {
    echo ""
    echo "=========================================="
    echo "  Local K8s Environment"
    echo "=========================================="
    echo ""
    echo "  Colima VM:"
    colima list 2>/dev/null | sed 's/^/    /'
    echo ""
    echo "  Kind clusters:"
    kind get clusters 2>/dev/null | sed 's/^/    /'
    echo ""
    echo "  Registry: localhost:5001"
    echo "  Context:  kind-twiglylabs-local"
    echo ""
}

###############################################################################
# Commands                                                                     #
###############################################################################

case "${1:-setup}" in
    setup)
        setup_colima
        setup_registry
        setup_kind
        show_status
        ;;
    reset)
        echo "==> Resetting Kind cluster..."
        kind delete cluster --name twiglylabs-local 2>/dev/null || true
        setup_kind
        show_status
        ;;
    teardown)
        echo "==> Tearing down everything..."
        kind delete cluster --name twiglylabs-local 2>/dev/null || true
        docker rm -f kind-registry 2>/dev/null || true
        colima stop 2>/dev/null || true
        echo "    Done. Run '$0 setup' to recreate."
        ;;
    status)
        show_status
        ;;
    *)
        echo "Usage: $0 {setup|reset|teardown|status}"
        exit 1
        ;;
esac
