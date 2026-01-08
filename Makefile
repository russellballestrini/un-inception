# UN CLI Inception - Makefile
# Egress shielding setup for sandbox nodes

PREFIX ?= /usr/local
MICROSOCKS_DIR = /opt/microsocks
REDSOCKS_PORT = 12345
SOCKS_PORT = 1080

.PHONY: help egress-shield microsocks redsocks-config egress-iptables egress-services clean-egress

help:
	@echo "Egress Shielding:"
	@echo "  make egress-shield     - Full setup (build + config + iptables + services)"
	@echo "  make microsocks        - Build microsocks SOCKS5 proxy"
	@echo "  make redsocks-config   - Generate /etc/redsocks.conf"
	@echo "  make egress-iptables   - Set up iptables NAT rules"
	@echo "  make egress-services   - Install and enable systemd services"
	@echo "  make clean-egress      - Remove egress shielding"

# Full setup
egress-shield: microsocks redsocks-config egress-services egress-iptables
	@echo "Egress shielding active. All TCP exits through SOCKS proxy."

# Build microsocks
microsocks:
	@echo "Building microsocks..."
	apt-get install -y git build-essential
	rm -rf $(MICROSOCKS_DIR)
	git clone --depth 1 https://github.com/rofl0r/microsocks $(MICROSOCKS_DIR)
	$(MAKE) -C $(MICROSOCKS_DIR)
	ln -sf $(MICROSOCKS_DIR)/microsocks $(PREFIX)/bin/microsocks

# Generate redsocks config
redsocks-config:
	@echo "Installing redsocks and config..."
	apt-get install -y redsocks
	cp services/redsocks.conf /etc/redsocks.conf

# Set up iptables rules
egress-iptables:
	@echo "Setting up iptables NAT rules..."
	# Create chain if not exists
	iptables -t nat -N REDSOCKS 2>/dev/null || true
	# Flush existing rules
	iptables -t nat -F REDSOCKS
	# Skip local traffic
	iptables -t nat -A REDSOCKS -d 0.0.0.0/8 -j RETURN
	iptables -t nat -A REDSOCKS -d 10.0.0.0/8 -j RETURN
	iptables -t nat -A REDSOCKS -d 127.0.0.0/8 -j RETURN
	iptables -t nat -A REDSOCKS -d 169.254.0.0/16 -j RETURN
	iptables -t nat -A REDSOCKS -d 172.16.0.0/12 -j RETURN
	iptables -t nat -A REDSOCKS -d 192.168.0.0/16 -j RETURN
	iptables -t nat -A REDSOCKS -d 224.0.0.0/4 -j RETURN
	iptables -t nat -A REDSOCKS -d 240.0.0.0/4 -j RETURN
	# Redirect all other TCP to redsocks
	iptables -t nat -A REDSOCKS -p tcp -j REDIRECT --to-ports $(REDSOCKS_PORT)
	# Apply to OUTPUT chain
	iptables -t nat -C OUTPUT -p tcp -j REDSOCKS 2>/dev/null || \
		iptables -t nat -A OUTPUT -p tcp -j REDSOCKS
	@echo "iptables rules applied."

# Install systemd services
egress-services:
	@echo "Installing systemd services..."
	cp services/microsocks.service /etc/systemd/system/
	cp services/redsocks.service /etc/systemd/system/
	cp services/egress-iptables.service /etc/systemd/system/
	systemctl daemon-reload
	systemctl enable --now microsocks
	systemctl enable --now redsocks
	systemctl enable --now egress-iptables
	@echo "Services installed and running."

# Remove egress shielding
clean-egress:
	@echo "Removing egress shielding..."
	-systemctl disable --now microsocks redsocks egress-iptables 2>/dev/null
	-rm -f /etc/systemd/system/microsocks.service
	-rm -f /etc/systemd/system/redsocks.service
	-rm -f /etc/systemd/system/egress-iptables.service
	-systemctl daemon-reload
	-iptables -t nat -D OUTPUT -p tcp -j REDSOCKS 2>/dev/null
	-iptables -t nat -F REDSOCKS 2>/dev/null
	-iptables -t nat -X REDSOCKS 2>/dev/null
	-rm -rf $(MICROSOCKS_DIR)
	-rm -f $(PREFIX)/bin/microsocks
	@echo "Egress shielding removed."
