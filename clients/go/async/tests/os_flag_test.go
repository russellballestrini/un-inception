// Tests for the global --os flag (tickets #123, #124, FreeBSD jails backend).
//
// The "os" field goes in POST /execute, /sessions and /services bodies only
// when --os is given, so older API versions see unchanged bodies.
//
// Each case runs our real CliMain in a child process (this test binary,
// re-executed) against a local HTTP server that records request bodies.
package tests

import (
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"os"
	"os/exec"
	"strings"
	"sync"
	"testing"

	un_async "github.com/unsandbox/un-go-async/src"
)

const osCLIArgsEnv = "UN_OS_FLAG_TEST_ARGS"

// TestOSFlagCLIHelper is not a test on its own: in a child process it runs CliMain.
func TestOSFlagCLIHelper(t *testing.T) {
	raw := os.Getenv(osCLIArgsEnv)
	if raw == "" {
		t.Skip("helper process only")
	}
	var args []string
	if err := json.Unmarshal([]byte(raw), &args); err != nil {
		os.Exit(99)
	}
	os.Args = append([]string{"un"}, args...)
	un_async.CliMain()
}

type recorded struct {
	method, path string
	body         map[string]interface{}
}

func runCLI(t *testing.T, args ...string) (int, string, []recorded) {
	t.Helper()
	var mu sync.Mutex
	var reqs []recorded
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		b, _ := io.ReadAll(r.Body)
		rec := recorded{method: r.Method, path: r.URL.Path}
		if len(b) > 0 {
			_ = json.Unmarshal(b, &rec.body)
		}
		mu.Lock()
		reqs = append(reqs, rec)
		mu.Unlock()
		w.Header().Set("Content-Type", "application/json")
		if strings.HasPrefix(r.URL.Path, "/execute") {
			_, _ = w.Write([]byte(`{"stdout":"","stderr":"","exit_code":0}`))
			return
		}
		w.WriteHeader(http.StatusBadRequest)
		_, _ = w.Write([]byte(`{"error":"mock: not created"}`))
	}))
	defer srv.Close()

	argJSON, _ := json.Marshal(args)
	cmd := exec.Command(os.Args[0], "-test.run=^TestOSFlagCLIHelper$")
	cmd.Env = append(os.Environ(),
		osCLIArgsEnv+"="+string(argJSON),
		"UNSANDBOX_API_URL="+srv.URL,
		"UNSANDBOX_PUBLIC_KEY=unsb-pk-test-0000-0000-0000",
		"UNSANDBOX_SECRET_KEY=unsb-sk-test0-00000-00000-00000",
		"HOME="+t.TempDir(),
	)
	out, err := cmd.CombinedOutput()
	code := 0
	if ee, ok := err.(*exec.ExitError); ok {
		code = ee.ExitCode()
	} else if err != nil {
		t.Fatalf("running helper: %v", err)
	}
	mu.Lock()
	defer mu.Unlock()
	return code, string(out), append([]recorded(nil), reqs...)
}

// osOf returns the "os" value of the first POST to path, "<absent>" when the
// key is missing, "<no-request>" when no such POST was made.
func osOf(reqs []recorded, path string) string {
	for _, r := range reqs {
		if r.method == "POST" && r.path == path {
			if v, ok := r.body["os"]; ok {
				s, _ := v.(string)
				return s
			}
			return "<absent>"
		}
	}
	return "<no-request>"
}

func TestOSFlagRequestBodies(t *testing.T) {
	cases := []struct {
		name string
		args []string
		path string
		want string
	}{
		{"execute freebsd", []string{"--os", "freebsd", "-s", "bash", "echo hi"}, "/execute", "freebsd"},
		{"execute no flag", []string{"-s", "bash", "echo hi"}, "/execute", "<absent>"},
		{"execute mixed case after code", []string{"-s", "bash", "echo hi", "--os", "FreeBSD"}, "/execute", "freebsd"},
		{"execute --os=linux", []string{"--os=linux", "-s", "bash", "echo hi"}, "/execute", "linux"},
		{"session freebsd", []string{"--os", "freebsd", "session"}, "/sessions", "freebsd"},
		{"session no flag", []string{"session"}, "/sessions", "<absent>"},
		{"service freebsd", []string{"--os", "freebsd", "service", "--name", "ostest", "--ports", "80", "--bootstrap", "true"}, "/services", "freebsd"},
		{"service no flag", []string{"service", "--name", "ostest", "--ports", "80", "--bootstrap", "true"}, "/services", "<absent>"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			code, out, reqs := runCLI(t, tc.args...)
			if got := osOf(reqs, tc.path); got != tc.want {
				t.Fatalf("os = %s, want %s (exit %d)\n%s", got, tc.want, code, out)
			}
		})
	}
}

func TestOSFlagInvalidSendsNothing(t *testing.T) {
	for _, args := range [][]string{
		{"--os", "openbsd", "-s", "bash", "echo hi"},
		{"--os=", "-s", "bash", "echo hi"},
		{"--os", "", "-s", "bash", "echo hi"},
		{"-s", "bash", "echo hi", "--os"},
	} {
		code, out, reqs := runCLI(t, args...)
		if code == 0 {
			t.Errorf("%v: exit 0, want non-zero\n%s", args, out)
		}
		if !strings.Contains(out, "--os") {
			t.Errorf("%v: error does not mention --os: %s", args, out)
		}
		if len(reqs) != 0 {
			t.Errorf("%v: sent %d requests, want 0", args, len(reqs))
		}
	}
}

func TestNormalizeOS(t *testing.T) {
	for in, want := range map[string]string{"": "", "linux": "linux", "LINUX": "linux", "FreeBSD": "freebsd"} {
		got, err := un_async.NormalizeOS(in)
		if err != nil || got != want {
			t.Errorf("un_async.NormalizeOS(%q) = %q, %v; want %q", in, got, err, want)
		}
	}
	if _, err := un_async.NormalizeOS("openbsd"); err == nil {
		t.Error("un_async.NormalizeOS(openbsd) accepted")
	}
}

// TestLibraryOSOption covers the library options: "os" sent only when set,
// normalized to lowercase, and an invalid value rejected with no request.
func TestLibraryOSOption(t *testing.T) {
	var mu sync.Mutex
	var reqs []recorded
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		b, _ := io.ReadAll(r.Body)
		rec := recorded{method: r.Method, path: r.URL.Path}
		_ = json.Unmarshal(b, &rec.body)
		mu.Lock()
		reqs = append(reqs, rec)
		mu.Unlock()
		w.Header().Set("Content-Type", "application/json")
		_, _ = w.Write([]byte(`{"stdout":"","stderr":"","exit_code":0,"job_id":"j1"}`))
	}))
	defer srv.Close()
	orig := un_async.APIBase
	un_async.APIBase = srv.URL
	defer func() { un_async.APIBase = orig }()
	creds := &un_async.Credentials{PublicKey: "unsb-pk-test", SecretKey: "unsb-sk-test"}

	<-un_async.ExecuteCodeWithOptions(creds, "bash", "true", &un_async.ExecuteOptions{OS: "FreeBSD"})
	<-un_async.ExecuteAsyncWithOptions(creds, "bash", "true", &un_async.ExecuteOptions{OS: "linux"})
	<-un_async.ExecuteCode(creds, "bash", "true")
	<-un_async.CreateSession(creds, &un_async.SessionOptions{OS: "freebsd"})
	<-un_async.CreateService(creds, "s", []int{80}, "true", &un_async.ServiceOptions{OS: "freebsd"})
	<-un_async.CreateService(creds, "s", []int{80}, "true", nil)

	mu.Lock()
	got := []string{}
	for _, r := range reqs {
		v, ok := r.body["os"]
		if !ok {
			v = "<absent>"
		}
		s, _ := v.(string)
		got = append(got, r.path+"="+s)
	}
	reqs = nil
	mu.Unlock()
	want := []string{"/execute=freebsd", "/execute=linux", "/execute=<absent>", "/sessions=freebsd", "/services=freebsd", "/services=<absent>"}
	if strings.Join(got, " ") != strings.Join(want, " ") {
		t.Fatalf("requests = %v, want %v", got, want)
	}

	if r := <-un_async.ExecuteCodeWithOptions(creds, "bash", "true", &un_async.ExecuteOptions{OS: "openbsd"}); r.Err == nil {
		t.Error("ExecuteCodeWithOptions accepted os openbsd")
	}
	if r := <-un_async.ExecuteAsyncWithOptions(creds, "bash", "true", &un_async.ExecuteOptions{OS: "openbsd"}); r.Err == nil {
		t.Error("ExecuteAsyncWithOptions accepted os openbsd")
	}
	if r := <-un_async.CreateSession(creds, &un_async.SessionOptions{OS: "openbsd"}); r.Err == nil {
		t.Error("CreateSession accepted os openbsd")
	}
	if r := <-un_async.CreateService(creds, "s", []int{80}, "true", &un_async.ServiceOptions{OS: "openbsd"}); r.Err == nil {
		t.Error("CreateService accepted os openbsd")
	}
	mu.Lock()
	defer mu.Unlock()
	if len(reqs) != 0 {
		t.Errorf("invalid os sent %d requests, want 0", len(reqs))
	}
}
