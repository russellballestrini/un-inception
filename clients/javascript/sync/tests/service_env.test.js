/**
 * setServiceEnv sends PUT /services/{id}/env with the .env text as text/plain,
 * and every request signs exactly the bytes it sends ({} bodies included).
 * A local HMAC-checking server stands in for our API: no network, no real keys.
 */
import http from 'http';
import crypto from 'crypto';

const SECRET = 'unsb-sk-test0-00000-00000-00000';
const PUBLIC = 'unsb-pk-test-0000-0000-0000';

let server;
let requests = [];
let un;

beforeAll(async () => {
  server = http.createServer((req, res) => {
    const chunks = [];
    req.on('data', (c) => chunks.push(c));
    req.on('end', () => {
      const body = Buffer.concat(chunks).toString('utf8');
      const message = `${req.headers['x-timestamp']}:${req.method}:${req.url.split('?')[0]}:${body}`;
      const expected = crypto.createHmac('sha256', SECRET).update(message).digest('hex');
      const signed = expected === req.headers['x-signature'];
      requests.push({ method: req.method, path: req.url, body, type: req.headers['content-type'], signed });
      res.writeHead(signed ? 200 : 401, { 'Content-Type': 'application/json' });
      res.end(JSON.stringify({ success: signed, count: 2 }));
    });
  });
  await new Promise((r) => server.listen(0, '127.0.0.1', r));
  // API_BASE is read at module load; point it at our server before importing.
  process.env.UNSANDBOX_API_URL = `http://127.0.0.1:${server.address().port}`;
  un = await import('../src/un.js');
});

afterAll(async () => {
  await new Promise((r) => server.close(r));
});

beforeEach(() => {
  requests = [];
});

describe('service env vault', () => {
  test('setServiceEnv with an object PUTs KEY=VALUE lines as text/plain', async () => {
    await un.setServiceEnv('unsb-service-abc', { API_KEY: 'x', DEBUG: 'true' }, PUBLIC, SECRET);
    expect(requests).toHaveLength(1);
    const [r] = requests;
    expect(r.method).toBe('PUT');
    expect(r.path).toBe('/services/unsb-service-abc/env');
    expect(r.type).toBe('text/plain');
    expect(r.body).toBe('API_KEY=x\nDEBUG=true');
    expect(r.signed).toBe(true);
  });

  test('setServiceEnv with .env text sends it unchanged', async () => {
    await un.setServiceEnv('unsb-service-abc', 'A=1\nB="two words"\n', PUBLIC, SECRET);
    expect(requests[0].method).toBe('PUT');
    expect(requests[0].body).toBe('A=1\nB="two words"\n');
    expect(requests[0].signed).toBe(true);
  });
});

describe('signed bytes are sent bytes', () => {
  test('a {} body is signed as "{}" and sent as "{}"', async () => {
    await un.freezeService('unsb-service-abc', PUBLIC, SECRET);
    const [r] = requests;
    expect(r.method).toBe('POST');
    expect(r.body).toBe('{}');
    expect(r.signed).toBe(true);
  });

  test('a GET signs and sends no body', async () => {
    await un.getService('unsb-service-abc', PUBLIC, SECRET);
    expect(requests[0].body).toBe('');
    expect(requests[0].signed).toBe(true);
  });
});
