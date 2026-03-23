// Client provides a struct-based API around the function-based SDK.
// Designed for consumers like orchestra that prefer method receivers
// and context-aware patterns.
package un

import (
	"context"
	"fmt"
)

// Client wraps resolved credentials for method-based API access.
type Client struct {
	Creds *Credentials
}

// NewClient creates a client from explicit credentials.
func NewClient(publicKey, secretKey string) *Client {
	return &Client{Creds: &Credentials{PublicKey: publicKey, SecretKey: secretKey}}
}

// NewClientFromEnv resolves credentials from the 4-tier priority system.
func NewClientFromEnv() (*Client, error) {
	creds, err := ResolveCredentials("", "", -1)
	if err != nil {
		return nil, err
	}
	return &Client{Creds: creds}, nil
}

// ExecuteResult holds typed execution output.
type ExecuteResult struct {
	JobID  string
	Status string
	Output string
	Error  string
	Raw    map[string]interface{}
}

func toExecuteResult(raw map[string]interface{}) *ExecuteResult {
	r := &ExecuteResult{Raw: raw}
	if v, ok := raw["job_id"].(string); ok {
		r.JobID = v
	}
	if v, ok := raw["status"].(string); ok {
		r.Status = v
	}
	// unsandbox API returns stdout/stderr, not output/error
	if v, ok := raw["output"].(string); ok {
		r.Output = v
	}
	if v, ok := raw["stdout"].(string); ok && r.Output == "" {
		r.Output = v
	}
	if v, ok := raw["error"].(string); ok {
		r.Error = v
	}
	if v, ok := raw["stderr"].(string); ok && r.Error == "" {
		r.Error = v
	}
	return r
}

// Execute runs code synchronously (blocks until completion).
func (c *Client) Execute(_ context.Context, language, code string) (*ExecuteResult, error) {
	return c.ExecuteWithOpts(context.Background(), language, code, "")
}

// ExecuteWithOpts runs code with optional network mode.
func (c *Client) ExecuteWithOpts(_ context.Context, language, code, networkMode string) (*ExecuteResult, error) {
	// Use the low-level makeRequest to support network_mode
	data := map[string]interface{}{
		"language": language,
		"code":     code,
	}
	if networkMode != "" {
		data["network_mode"] = networkMode
	}

	resp, err := makeRequest("POST", "/execute", c.Creds, data)
	if err != nil {
		return nil, err
	}

	result := toExecuteResult(resp)

	// Poll if job is not terminal
	if result.JobID != "" && result.Status != "completed" && result.Status != "failed" {
		polled, err := WaitForJob(c.Creds, result.JobID)
		if err != nil {
			return nil, err
		}
		return toExecuteResult(polled), nil
	}

	return result, nil
}

// WaitForJobResult polls a job until completion.
func (c *Client) WaitForJobResult(_ context.Context, jobID string) (*ExecuteResult, error) {
	raw, err := WaitForJob(c.Creds, jobID)
	if err != nil {
		return nil, err
	}
	return toExecuteResult(raw), nil
}

// SessionResult holds typed session info.
type SessionResult struct {
	ID     string
	Status string
	Raw    map[string]interface{}
}

// CreateSession creates a new execution session.
func (c *Client) CreateSession(_ context.Context, language, networkMode string) (*SessionResult, error) {
	opts := &SessionOptions{
		Shell: language,
	}
	if networkMode != "" {
		opts.NetworkMode = networkMode
	}

	raw, err := CreateSession(c.Creds, opts)
	if err != nil {
		return nil, err
	}

	s := &SessionResult{Raw: raw}
	if v, ok := raw["session_id"].(string); ok {
		s.ID = v
	}
	if v, ok := raw["id"].(string); ok && s.ID == "" {
		s.ID = v
	}
	if v, ok := raw["status"].(string); ok {
		s.Status = v
	}
	return s, nil
}

// ShellExec runs a command in an existing session.
func (c *Client) ShellExec(_ context.Context, sessionID, command string) (*ExecuteResult, error) {
	raw, err := ShellSession(c.Creds, sessionID, command)
	if err != nil {
		return nil, err
	}

	result := toExecuteResult(raw)

	// Poll if async
	if result.JobID != "" && result.Status != "completed" && result.Status != "failed" {
		polled, err := WaitForJob(c.Creds, result.JobID)
		if err != nil {
			return nil, err
		}
		return toExecuteResult(polled), nil
	}

	return result, nil
}

// DestroySession deletes a session.
func (c *Client) DestroySession(_ context.Context, sessionID string) error {
	_, err := DeleteSession(c.Creds, sessionID)
	return err
}

// Sessions returns all active sessions.
func (c *Client) Sessions(_ context.Context) ([]map[string]interface{}, error) {
	return ListSessions(c.Creds)
}

// Services returns all services.
func (c *Client) Services(_ context.Context) ([]map[string]interface{}, error) {
	return ListServices(c.Creds)
}

// ServiceLogs retrieves logs for a service.
func (c *Client) ServiceLogs(_ context.Context, serviceID string) (string, error) {
	raw, err := GetServiceLogs(c.Creds, serviceID, false)
	if err != nil {
		return "", err
	}
	if v, ok := raw["logs"].(string); ok {
		return v, nil
	}
	return "", nil
}

// CheckKeys verifies credentials are valid and returns key info.
func (c *Client) CheckKeys(_ context.Context) (map[string]interface{}, error) {
	return ValidateKeys(c.Creds)
}

// InjectDirectory uploads a local directory to a session as a tarball.
// This is a higher-level operation that creates a tar.gz, base64-chunks it,
// and streams it into the session via shell commands.
func (c *Client) InjectDirectory(_ context.Context, sessionID, localDir, remoteDir string) error {
	return fmt.Errorf("InjectDirectory not implemented in SDK client — use orchestra's upload package")
}
