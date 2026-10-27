"""CLI execute through our real entry point (ticket #124 item 2).

`un.py FILE` and `un.py -s LANG 'code'` used to die in argparse because the
code or filename landed in our subcommand slot. These tests drive cli_main()
with real argv against a local HTTP server on 127.0.0.1 that records bodies.
No network beyond loopback, no credentials.
"""

import sys

import pytest

import un
from .test_os_flag import api, _body  # noqa: F401  (pytest fixture + helper)


def _run(monkeypatch, argv):
    monkeypatch.setattr(sys, "argv", ["un.py"] + argv)
    try:
        un.cli_main()
    except SystemExit as e:
        return e.code
    return 0


class TestCliExecute:
    def test_inline_code(self, api, monkeypatch, capsys):
        assert _run(monkeypatch, ["-s", "bash", "echo hi"]) in (0, None)
        body = _body(api, "/execute")
        assert body["language"] == "bash"
        assert body["code"] == "echo hi"
        assert "Exit code: 0" in capsys.readouterr().out

    def test_inline_code_after_other_globals(self, api, monkeypatch):
        assert _run(monkeypatch, ["-n", "zerotrust", "--shell=python", "print(1)"]) in (0, None)
        body = _body(api, "/execute")
        assert body["language"] == "python"
        assert body["code"] == "print(1)"

    def test_file(self, api, monkeypatch, tmp_path):
        script = tmp_path / "hi.sh"
        script.write_text("echo hi\n")
        assert _run(monkeypatch, [str(script)]) in (0, None)
        body = _body(api, "/execute")
        assert body["language"] == "bash"
        assert body["code"] == "echo hi\n"

    def test_file_named_like_subcommand_prefix(self, api, monkeypatch, tmp_path):
        # Only exact subcommand names route to subcommands.
        script = tmp_path / "session.py"
        script.write_text("print(1)\n")
        assert _run(monkeypatch, [str(script)]) in (0, None)
        assert _body(api, "/execute")["language"] == "python"

    def test_missing_file_exits_nonzero_and_sends_nothing(self, api, monkeypatch, tmp_path):
        assert _run(monkeypatch, [str(tmp_path / "nope.sh")]) != 0
        assert api.requests == []

    def test_shell_without_code_exits_2(self, api, monkeypatch):
        assert _run(monkeypatch, ["-s", "bash"]) == 2
        assert api.requests == []

    def test_no_args_prints_help_exits_2(self, api, monkeypatch, capsys):
        assert _run(monkeypatch, []) == 2
        assert "usage:" in capsys.readouterr().out
        assert api.requests == []


class TestCliSubcommandsStillRoute:
    def test_session_list(self, api, monkeypatch):
        _run(monkeypatch, ["session", "--list"])
        assert [r for r in api.requests if r[0] == "GET" and r[1].startswith("/sessions")]
        assert not [r for r in api.requests if r[1].startswith("/execute")]

    def test_service_create_with_globals_first(self, api, monkeypatch):
        _run(monkeypatch, ["-y", "service", "--name", "t", "--ports", "80", "--bootstrap", "true"])
        assert _body(api, "/services")["name"] == "t"
