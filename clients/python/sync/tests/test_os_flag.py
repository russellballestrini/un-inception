"""Tests for the global --os flag (ticket #123, FreeBSD jails backend).

The "os" field goes in POST /execute, /sessions and /services bodies only
when a target OS is given, so older API versions see unchanged bodies.

No mocking of our SDK: requests go through our real _make_request to a local
HTTP server on 127.0.0.1 that records each body.
"""

import json
import sys
import threading
from http.server import BaseHTTPRequestHandler, HTTPServer

import pytest

import un


class _Recorder(BaseHTTPRequestHandler):
    requests = []

    def _reply(self, obj):
        data = json.dumps(obj).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def do_POST(self):
        n = int(self.headers.get("Content-Length") or 0)
        body = self.rfile.read(n).decode()
        _Recorder.requests.append(("POST", self.path, body))
        if self.path.startswith("/execute"):
            self._reply({"stdout": "", "stderr": "", "exit_code": 0})
        elif self.path.startswith("/sessions"):
            self._reply({"session_id": "s1"})
        else:
            self._reply({"service_id": "svc1"})

    def do_GET(self):
        _Recorder.requests.append(("GET", self.path, ""))
        self._reply({})

    def log_message(self, *a):
        pass


@pytest.fixture
def api(monkeypatch):
    srv = HTTPServer(("127.0.0.1", 0), _Recorder)
    t = threading.Thread(target=srv.serve_forever, daemon=True)
    t.start()
    _Recorder.requests = []
    monkeypatch.setattr(un, "API_BASE", f"http://127.0.0.1:{srv.server_address[1]}")
    monkeypatch.setenv("UNSANDBOX_PUBLIC_KEY", "unsb-pk-test-0000-0000-0000")
    monkeypatch.setenv("UNSANDBOX_SECRET_KEY", "unsb-sk-test0-00000-00000-00000")
    yield _Recorder
    srv.shutdown()
    srv.server_close()


def _body(rec, path):
    for method, p, body in rec.requests:
        if method == "POST" and p.split("?")[0] == path:
            return json.loads(body)
    raise AssertionError(f"no POST to {path}: {rec.requests}")


def _cli(monkeypatch, argv):
    monkeypatch.setattr(sys, "argv", ["un.py"] + argv)
    try:
        un.cli_main()
    except SystemExit as e:
        assert e.code in (0, None), f"cli exited {e.code}"


class TestOsFlagCli:
    def test_execute_sends_os_freebsd(self, api, monkeypatch):
        _cli(monkeypatch, ["--os", "freebsd", "-s", "bash", "echo hi"])
        assert _body(api, "/execute")["os"] == "freebsd"

    def test_execute_without_flag_omits_os(self, api, monkeypatch):
        _cli(monkeypatch, ["-s", "bash", "echo hi"])
        assert "os" not in _body(api, "/execute")

    def test_os_is_case_insensitive(self, api, monkeypatch):
        _cli(monkeypatch, ["--os", "FreeBSD", "-s", "bash", "echo hi"])
        assert _body(api, "/execute")["os"] == "freebsd"

    def test_os_equals_form_with_file(self, api, monkeypatch, tmp_path):
        script = tmp_path / "hi.sh"
        script.write_text("echo hi\n")
        _cli(monkeypatch, ["--os=freebsd", str(script)])
        body = _body(api, "/execute")
        assert body["os"] == "freebsd"
        assert body["language"] == "bash"

    def test_session_sends_os(self, api, monkeypatch):
        _cli(monkeypatch, ["--os", "freebsd", "session"])
        assert _body(api, "/sessions")["os"] == "freebsd"

    def test_session_without_flag_omits_os(self, api, monkeypatch):
        _cli(monkeypatch, ["session"])
        assert "os" not in _body(api, "/sessions")

    def test_service_sends_os(self, api, monkeypatch):
        _cli(monkeypatch, ["--os", "freebsd", "service", "--name", "ostest",
                           "--ports", "80", "--bootstrap", "true"])
        assert _body(api, "/services")["os"] == "freebsd"

    def test_service_without_flag_omits_os(self, api, monkeypatch):
        _cli(monkeypatch, ["service", "--name", "ostest", "--ports", "80", "--bootstrap", "true"])
        assert "os" not in _body(api, "/services")

    @pytest.mark.parametrize("argv", [
        ["--os", "openbsd", "session"],
        ["--os", "openbsd", "-s", "bash", "echo hi"],
    ])
    def test_invalid_os_exits_nonzero_and_sends_nothing(self, api, monkeypatch, capsys, argv):
        monkeypatch.setattr(sys, "argv", ["un.py"] + argv)
        with pytest.raises(SystemExit) as exc:
            un.cli_main()
        assert exc.value.code != 0
        assert "openbsd" in capsys.readouterr().err
        assert api.requests == []


class TestOsLibrary:
    def test_execute_code_target_os(self, api):
        un.execute_code("bash", "echo hi", target_os="freebsd")
        assert _body(api, "/execute")["os"] == "freebsd"

    def test_with_os_rejects_unknown(self):
        with pytest.raises(ValueError):
            un._with_os({}, "openbsd")

    def test_with_os_none_leaves_body_alone(self):
        assert un._with_os({"a": 1}, None) == {"a": 1}
