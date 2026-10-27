# This is free software for the public good of a permacomputer hosted at
# permacomputer.com, an always-on computer by the people, for the people.
# One which is durable, easy to repair, & distributed like tap water
# for machine learning intelligence.
#
# The permacomputer is community-owned infrastructure optimized around
# four values:
#
#   TRUTH      First principles, math & science, open source code freely distributed
#   FREEDOM    Voluntary partnerships, freedom from tyranny & corporate control
#   HARMONY    Minimal waste, self-renewing systems with diverse thriving connections
#   LOVE       Be yourself without hurting others, cooperation through natural law
#
# This software contributes to that vision by enabling code execution across 42+ programming languages through a unified interface, accessible to all.
# Code is seeds to sprout on any abandoned technology.

"""Tests for the global --os flag (tickets #123, #124, FreeBSD jails backend).

The "os" field goes in POST /execute, /sessions and /services bodies only
when a target OS is given, so older API versions see unchanged bodies.

No mocking of our SDK: requests go through our real _make_request (aiohttp)
to a local HTTP server on 127.0.0.1 that records each body. Tests drive
coroutines with asyncio.run, so they need no pytest-asyncio.
"""

import asyncio
import json
import sys
import threading
from http.server import BaseHTTPRequestHandler, HTTPServer

import pytest

import un_async


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
    monkeypatch.setattr(un_async, "API_BASE", f"http://127.0.0.1:{srv.server_address[1]}")
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


def _run(monkeypatch, argv):
    monkeypatch.setattr(sys, "argv", ["un_async.py"] + argv)
    try:
        un_async.cli_main()
    except SystemExit as e:
        return e.code
    return 0


class TestOsFlagCli:
    def test_execute_sends_os_freebsd(self, api, monkeypatch):
        assert _run(monkeypatch, ["--os", "freebsd", "-s", "bash", "echo hi"]) in (0, None)
        assert _body(api, "/execute")["os"] == "freebsd"

    def test_execute_without_flag_omits_os(self, api, monkeypatch):
        _run(monkeypatch, ["-s", "bash", "echo hi"])
        assert "os" not in _body(api, "/execute")

    def test_os_is_case_insensitive(self, api, monkeypatch):
        _run(monkeypatch, ["--os", "FreeBSD", "-s", "bash", "echo hi"])
        assert _body(api, "/execute")["os"] == "freebsd"

    def test_os_equals_form_with_file(self, api, monkeypatch, tmp_path):
        script = tmp_path / "hi.sh"
        script.write_text("echo hi\n")
        assert _run(monkeypatch, ["--os=freebsd", str(script)]) in (0, None)
        body = _body(api, "/execute")
        assert body["os"] == "freebsd"
        assert body["language"] == "bash"

    def test_session_sends_os(self, api, monkeypatch):
        _run(monkeypatch, ["--os", "freebsd", "session"])
        assert _body(api, "/sessions")["os"] == "freebsd"

    def test_session_without_flag_omits_os(self, api, monkeypatch):
        _run(monkeypatch, ["session"])
        assert "os" not in _body(api, "/sessions")

    def test_service_sends_os(self, api, monkeypatch):
        _run(monkeypatch, ["--os", "freebsd", "service", "--name", "ostest",
                           "--ports", "80", "--bootstrap", "true"])
        assert _body(api, "/services")["os"] == "freebsd"

    def test_service_without_flag_omits_os(self, api, monkeypatch):
        _run(monkeypatch, ["service", "--name", "ostest", "--ports", "80", "--bootstrap", "true"])
        assert "os" not in _body(api, "/services")

    @pytest.mark.parametrize("argv", [
        ["--os", "openbsd", "session"],
        ["--os", "openbsd", "-s", "bash", "echo hi"],
    ])
    def test_invalid_os_exits_nonzero_and_sends_nothing(self, api, monkeypatch, capsys, argv):
        assert _run(monkeypatch, argv) != 0
        assert "openbsd" in capsys.readouterr().err
        assert api.requests == []


class TestCliExecuteRouting:
    """`un_async.py FILE` and `-s LANG 'code'` used to die in argparse because
    the code or filename landed in our subcommand slot (ticket #124)."""

    def test_inline_code(self, api, monkeypatch, capsys):
        assert _run(monkeypatch, ["-s", "bash", "echo hi"]) in (0, None)
        assert _body(api, "/execute") == {"language": "bash", "code": "echo hi"}
        assert "Exit code: 0" in capsys.readouterr().out

    def test_file(self, api, monkeypatch, tmp_path):
        script = tmp_path / "session.py"  # named like a subcommand prefix
        script.write_text("print(1)\n")
        assert _run(monkeypatch, [str(script)]) in (0, None)
        assert _body(api, "/execute")["language"] == "python"

    def test_no_args_prints_help_exits_2(self, api, monkeypatch, capsys):
        assert _run(monkeypatch, []) == 2
        assert "usage:" in capsys.readouterr().out
        assert api.requests == []

    def test_session_list_still_routes(self, api, monkeypatch):
        _run(monkeypatch, ["session", "--list"])
        assert [r for r in api.requests if r[0] == "GET" and r[1].startswith("/sessions")]


class TestOsLibrary:
    def test_execute_code_target_os(self, api):
        asyncio.run(un_async.execute_code("bash", "echo hi", target_os="FreeBSD"))
        assert _body(api, "/execute")["os"] == "freebsd"

    def test_execute_async_target_os(self, api):
        asyncio.run(un_async.execute_async("bash", "echo hi", target_os="linux"))
        assert _body(api, "/execute")["os"] == "linux"

    def test_create_session_and_service_target_os(self, api):
        asyncio.run(un_async.create_session(target_os="freebsd"))
        asyncio.run(un_async.create_service("t", [80], "true", target_os="freebsd"))
        assert _body(api, "/sessions")["os"] == "freebsd"
        assert _body(api, "/services")["os"] == "freebsd"

    def test_invalid_target_os_raises_before_request(self, api):
        with pytest.raises(ValueError):
            asyncio.run(un_async.create_session(target_os="openbsd"))
        assert api.requests == []

    def test_with_os_none_leaves_body_alone(self):
        assert un_async._with_os({"a": 1}, None) == {"a": 1}
