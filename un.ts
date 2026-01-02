#!/usr/bin/env ts-node
// PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
//
// This is free public domain software for the public good of a permacomputer hosted
// at permacomputer.com - an always-on computer by the people, for the people. One
// which is durable, easy to repair, and distributed like tap water for machine
// learning intelligence.
//
// The permacomputer is community-owned infrastructure optimized around four values:
//
//   TRUTH    - First principles, math & science, open source code freely distributed
//   FREEDOM  - Voluntary partnerships, freedom from tyranny & corporate control
//   HARMONY  - Minimal waste, self-renewing systems with diverse thriving connections
//   LOVE     - Be yourself without hurting others, cooperation through natural law
//
// This software contributes to that vision by enabling code execution across 42+
// programming languages through a unified interface, accessible to all. Code is
// seeds to sprout on any abandoned technology.
//
// Learn more: https://www.permacomputer.com
//
// Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
// software, either in source code form or as a compiled binary, for any purpose,
// commercial or non-commercial, and by any means.
//
// NO WARRANTY. THE SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.
//
// That said, our permacomputer's digital membrane stratum continuously runs unit,
// integration, and functional tests on all of it's own software - with our
// permacomputer monitoring itself, repairing itself, with minimal human in the
// loop guidance. Our agents do their best.
//
// Copyright 2025 TimeHexOn & foxhop & russell@unturf
// https://www.timehexon.com
// https://www.foxhop.net
// https://www.unturf.com/software

/**
 * un.ts - Unsandbox CLI Client (TypeScript Implementation)
 *
 * Full-featured CLI matching un.c capabilities:
 * - Execute code with env vars, input files, artifacts
 * - Interactive sessions with shell/REPL support
 * - Persistent services with domains and ports
 *
 * Usage:
 *   un.ts [options] <source_file>
 *   un.ts session [options]
 *   un.ts service [options]
 *
 * Requires: UNSANDBOX_API_KEY environment variable
 */

import * as fs from 'fs';
import * as https from 'https';
import * as path from 'path';
import * as crypto from 'crypto';

const API_BASE = "https://api.unsandbox.com";
const PORTAL_BASE = "https://unsandbox.com";
const BLUE = "\x1b[34m";
const RED = "\x1b[31m";
const GREEN = "\x1b[32m";
const YELLOW = "\x1b[33m";
const RESET = "\x1b[0m";

const EXT_MAP: Record<string, string> = {
  ".py": "python", ".js": "javascript", ".ts": "typescript",
  ".rb": "ruby", ".php": "php", ".pl": "perl", ".lua": "lua",
  ".sh": "bash", ".go": "go", ".rs": "rust", ".c": "c",
  ".cpp": "cpp", ".cc": "cpp", ".cxx": "cpp",
  ".java": "java", ".kt": "kotlin", ".cs": "csharp", ".fs": "fsharp",
  ".hs": "haskell", ".ml": "ocaml", ".clj": "clojure", ".scm": "scheme",
  ".lisp": "commonlisp", ".erl": "erlang", ".ex": "elixir", ".exs": "elixir",
  ".jl": "julia", ".r": "r", ".R": "r", ".cr": "crystal",
  ".d": "d", ".nim": "nim", ".zig": "zig", ".v": "v",
  ".dart": "dart", ".groovy": "groovy", ".scala": "scala",
  ".f90": "fortran", ".f95": "fortran", ".cob": "cobol",
  ".pro": "prolog", ".forth": "forth", ".4th": "forth",
  ".tcl": "tcl", ".raku": "raku", ".m": "objc",
};

interface Args {
  command: string | null;
  sourceFile: string | null;
  env: string[];
  files: string[];
  artifacts: boolean;
  outputDir: string | null;
  network: string | null;
  vcpu: number | null;
  apiKey: string | null;
  shell: string | null;
  list: boolean;
  attach: string | null;
  kill: string | null;
  audit: boolean;
  tmux: boolean;
  screen: boolean;
  name: string | null;
  ports: string | null;
  domains: string | null;
  type: string | null;
  bootstrap: string | null;
  bootstrapFile: string | null;
  info: string | null;
  logs: string | null;
  tail: string | null;
  sleep: string | null;
  wake: string | null;
  destroy: string | null;
  execute: string | null;
  command_arg: string | null;
  extend: boolean;
}

interface ApiKeys {
  publicKey: string;
  secretKey: string;
}

function getApiKeys(argsKey: string | null): ApiKeys {
  let publicKey = process.env.UNSANDBOX_PUBLIC_KEY;
  let secretKey = process.env.UNSANDBOX_SECRET_KEY;

  if (!publicKey || !secretKey) {
    const oldKey = argsKey || process.env.UNSANDBOX_API_KEY;
    if (oldKey) {
      publicKey = oldKey;
      secretKey = oldKey;
    } else {
      console.error(`${RED}Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set${RESET}`);
      console.error(`${RED}       (or legacy UNSANDBOX_API_KEY for backwards compatibility)${RESET}`);
      process.exit(1);
    }
  }

  return { publicKey, secretKey };
}

function detectLanguage(filename: string): string {
  const ext = path.extname(filename).toLowerCase();
  const lang = EXT_MAP[ext];
  if (!lang) {
    try {
      const firstLine = fs.readFileSync(filename, 'utf-8').split('\n')[0];
      if (firstLine.startsWith('#!')) {
        if (firstLine.includes('python')) return 'python';
        if (firstLine.includes('node')) return 'javascript';
        if (firstLine.includes('ruby')) return 'ruby';
        if (firstLine.includes('perl')) return 'perl';
        if (firstLine.includes('bash') || firstLine.includes('/sh')) return 'bash';
        if (firstLine.includes('lua')) return 'lua';
        if (firstLine.includes('php')) return 'php';
      }
    } catch (e) {}
    console.error(`${RED}Error: Cannot detect language for ${filename}${RESET}`);
    process.exit(1);
  }
  return lang;
}

function apiRequest(endpoint: string, method: string = "GET", data: any = null, keys: ApiKeys): Promise<any> {
  return new Promise((resolve, reject) => {
    const url = new URL(API_BASE + endpoint);
    const timestamp = Math.floor(Date.now() / 1000).toString();
    const body = data ? JSON.stringify(data) : '';
    const message = `${timestamp}:${method}:${url.pathname}${url.search}:${body}`;
    const signature = crypto.createHmac('sha256', keys.secretKey).update(message).digest('hex');

    const options: https.RequestOptions = {
      hostname: url.hostname,
      path: url.pathname + url.search,
      method: method,
      headers: {
        'Authorization': `Bearer ${keys.publicKey}`,
        'X-Timestamp': timestamp,
        'X-Signature': signature,
        'Content-Type': 'application/json'
      },
      timeout: 300000
    };

    const req = https.request(options, (res) => {
      let body = '';
      res.on('data', chunk => body += chunk);
      res.on('end', () => {
        if (res.statusCode && res.statusCode >= 200 && res.statusCode < 300) {
          try {
            resolve(JSON.parse(body));
          } catch (e) {
            resolve(body);
          }
        } else {
          if (res.statusCode === 401 && body.toLowerCase().includes('timestamp')) {
            console.error(`${RED}Error: Request timestamp expired (must be within 5 minutes of server time)${RESET}`);
            console.error(`${YELLOW}Your computer's clock may have drifted.${RESET}`);
            console.error("Check your system time and sync with NTP if needed:");
            console.error("  Linux:   sudo ntpdate -s time.nist.gov");
            console.error("  macOS:   sudo sntp -sS time.apple.com");
            console.error("  Windows: w32tm /resync");
          } else {
            console.error(`${RED}Error: HTTP ${res.statusCode} - ${body}${RESET}`);
          }
          process.exit(1);
        }
      });
    });

    req.on('error', (e) => {
      console.error(`${RED}Error: ${e.message}${RESET}`);
      process.exit(1);
    });

    if (data) {
      req.write(body);
    }
    req.end();
  });
}

function portalRequest(endpoint: string, method: string = "GET", data: any = null, keys: ApiKeys): Promise<any> {
  return new Promise((resolve, reject) => {
    const url = new URL(PORTAL_BASE + endpoint);
    const timestamp = Math.floor(Date.now() / 1000).toString();
    const body = data ? JSON.stringify(data) : '';
    const message = `${timestamp}:${method}:${url.pathname}${url.search}:${body}`;
    const signature = crypto.createHmac('sha256', keys.secretKey).update(message).digest('hex');

    const options: https.RequestOptions = {
      hostname: url.hostname,
      path: url.pathname + url.search,
      method: method,
      headers: {
        'Authorization': `Bearer ${keys.publicKey}`,
        'X-Timestamp': timestamp,
        'X-Signature': signature,
        'Content-Type': 'application/json'
      },
      timeout: 30000
    };

    const req = https.request(options, (res) => {
      let body = '';
      res.on('data', chunk => body += chunk);
      res.on('end', () => {
        try {
          const parsed = JSON.parse(body);
          resolve(parsed);
        } catch (e) {
          resolve({ error: body, status: res.statusCode });
        }
      });
    });

    req.on('error', (e) => {
      reject(e);
    });

    if (data) {
      req.write(body);
    }
    req.end();
  });
}

async function cmdExecute(args: Args): Promise<void> {
  const keys = getApiKeys(args.apiKey);

  let code: string;
  try {
    code = fs.readFileSync(args.sourceFile!, 'utf-8');
  } catch (e) {
    console.error(`${RED}Error: File not found: ${args.sourceFile}${RESET}`);
    process.exit(1);
  }

  const language = detectLanguage(args.sourceFile!);
  const payload: any = { language, code };

  if (args.env && args.env.length > 0) {
    payload.env = {};
    args.env.forEach(e => {
      const idx = e.indexOf('=');
      if (idx > 0) {
        payload.env[e.substring(0, idx)] = e.substring(idx + 1);
      }
    });
  }

  if (args.files && args.files.length > 0) {
    payload.input_files = args.files.map(filepath => {
      try {
        const content = fs.readFileSync(filepath);
        return {
          filename: path.basename(filepath),
          content_base64: content.toString('base64')
        };
      } catch (e) {
        console.error(`${RED}Error: Input file not found: ${filepath}${RESET}`);
        process.exit(1);
      }
    });
  }

  if (args.artifacts) payload.return_artifacts = true;
  if (args.network) payload.network = args.network;
  if (args.vcpu) payload.vcpu = args.vcpu;

  const result = await apiRequest("/execute", "POST", payload, keys);

  if (result.stdout) process.stdout.write(`${BLUE}${result.stdout}${RESET}`);
  if (result.stderr) process.stderr.write(`${RED}${result.stderr}${RESET}`);

  if (args.artifacts && result.artifacts) {
    const outDir = args.outputDir || '.';
    if (!fs.existsSync(outDir)) fs.mkdirSync(outDir, { recursive: true });
    result.artifacts.forEach((artifact: any) => {
      const filename = artifact.filename || 'artifact';
      const content = Buffer.from(artifact.content_base64, 'base64');
      const filepath = path.join(outDir, filename);
      fs.writeFileSync(filepath, content);
      fs.chmodSync(filepath, 0o755);
      console.error(`${GREEN}Saved: ${filepath}${RESET}`);
    });
  }

  process.exit(result.exit_code || 0);
}

async function cmdSession(args: Args): Promise<void> {
  const keys = getApiKeys(args.apiKey);

  if (args.list) {
    const result = await apiRequest("/sessions", "GET", null, keys);
    const sessions = result.sessions || [];
    if (sessions.length === 0) {
      console.log("No active sessions");
    } else {
      console.log(`${'ID'.padEnd(40)} ${'Shell'.padEnd(10)} ${'Status'.padEnd(10)} Created`);
      sessions.forEach((s: any) => {
        console.log(`${(s.id || 'N/A').padEnd(40)} ${(s.shell || 'N/A').padEnd(10)} ${(s.status || 'N/A').padEnd(10)} ${s.created_at || 'N/A'}`);
      });
    }
    return;
  }

  if (args.kill) {
    await apiRequest(`/sessions/${args.kill}`, "DELETE", null, keys);
    console.log(`${GREEN}Session terminated: ${args.kill}${RESET}`);
    return;
  }

  if (args.attach) {
    console.log(`${YELLOW}Attaching to session ${args.attach}...${RESET}`);
    console.log(`${YELLOW}(Interactive sessions require WebSocket - use un2 for full support)${RESET}`);
    return;
  }

  const payload: any = { shell: args.shell || "bash" };
  if (args.network) payload.network = args.network;
  if (args.vcpu) payload.vcpu = args.vcpu;
  if (args.tmux) payload.persistence = "tmux";
  if (args.screen) payload.persistence = "screen";
  if (args.audit) payload.audit = true;

  // Add input files
  if (args.files && args.files.length > 0) {
    payload.input_files = args.files.map(filepath => {
      try {
        const content = fs.readFileSync(filepath);
        return {
          filename: path.basename(filepath),
          content_base64: content.toString('base64')
        };
      } catch (e) {
        console.error(`${RED}Error: Input file not found: ${filepath}${RESET}`);
        process.exit(1);
      }
    });
  }

  console.log(`${YELLOW}Creating session...${RESET}`);
  const result = await apiRequest("/sessions", "POST", payload, keys);
  console.log(`${GREEN}Session created: ${result.id || 'N/A'}${RESET}`);
  console.log(`${YELLOW}(Interactive sessions require WebSocket - use un2 for full support)${RESET}`);
}

async function cmdService(args: Args): Promise<void> {
  const keys = getApiKeys(args.apiKey);

  if (args.list) {
    const result = await apiRequest("/services", "GET", null, keys);
    const services = result.services || [];
    if (services.length === 0) {
      console.log("No services");
    } else {
      console.log(`${'ID'.padEnd(20)} ${'Name'.padEnd(15)} ${'Status'.padEnd(10)} ${'Ports'.padEnd(15)} Domains`);
      services.forEach((s: any) => {
        const ports = (s.ports || []).join(',');
        const domains = (s.domains || []).join(',');
        console.log(`${(s.id || 'N/A').padEnd(20)} ${(s.name || 'N/A').padEnd(15)} ${(s.status || 'N/A').padEnd(10)} ${ports.padEnd(15)} ${domains}`);
      });
    }
    return;
  }

  if (args.info) {
    const result = await apiRequest(`/services/${args.info}`, "GET", null, keys);
    console.log(JSON.stringify(result, null, 2));
    return;
  }

  if (args.logs) {
    const result = await apiRequest(`/services/${args.logs}/logs`, "GET", null, keys);
    console.log(result.logs || "");
    return;
  }

  if (args.tail) {
    const result = await apiRequest(`/services/${args.tail}/logs?lines=9000`, "GET", null, keys);
    console.log(result.logs || "");
    return;
  }

  if (args.sleep) {
    await apiRequest(`/services/${args.sleep}/sleep`, "POST", null, keys);
    console.log(`${GREEN}Service sleeping: ${args.sleep}${RESET}`);
    return;
  }

  if (args.wake) {
    await apiRequest(`/services/${args.wake}/wake`, "POST", null, keys);
    console.log(`${GREEN}Service waking: ${args.wake}${RESET}`);
    return;
  }

  if (args.destroy) {
    await apiRequest(`/services/${args.destroy}`, "DELETE", null, keys);
    console.log(`${GREEN}Service destroyed: ${args.destroy}${RESET}`);
    return;
  }

  if (args.execute) {
    const payload = { command: args.command_arg };
    const result = await apiRequest(`/services/${args.execute}/execute`, "POST", payload, keys);
    if (result.stdout) process.stdout.write(`${BLUE}${result.stdout}${RESET}`);
    if (result.stderr) process.stderr.write(`${RED}${result.stderr}${RESET}`);
    return;
  }

  if (args.dumpBootstrap) {
    console.error(`Fetching bootstrap script from ${args.dumpBootstrap}...`);
    const payload = { command: "cat /tmp/bootstrap.sh" };
    const result = await apiRequest(`/services/${args.dumpBootstrap}/execute`, "POST", payload, keys);

    if (result.stdout) {
      const bootstrap = result.stdout;
      if (args.dumpFile) {
        // Write to file
        try {
          fs.writeFileSync(args.dumpFile, bootstrap);
          fs.chmodSync(args.dumpFile, 0o755);
          console.log(`Bootstrap saved to ${args.dumpFile}`);
        } catch (e: any) {
          console.error(`${RED}Error: Could not write to ${args.dumpFile}: ${e.message}${RESET}`);
          process.exit(1);
        }
      } else {
        // Print to stdout
        process.stdout.write(bootstrap);
      }
    } else {
      console.error(`${RED}Error: Failed to fetch bootstrap (service not running or no bootstrap file)${RESET}`);
      process.exit(1);
    }
    return;
  }

  if (args.name) {
    const payload: any = { name: args.name };
    if (args.ports) payload.ports = args.ports.split(',').map(p => parseInt(p.trim()));
    if (args.domains) payload.domains = args.domains.split(',');
    if (args.type) payload.service_type = args.type;
    if (args.bootstrap) {
      payload.bootstrap = args.bootstrap;
    }
    if (args.bootstrapFile) {
      if (!fs.existsSync(args.bootstrapFile)) {
        console.error(`${RED}Error: Bootstrap file not found: ${args.bootstrapFile}${RESET}`);
        process.exit(1);
      }
      payload.bootstrap_content = fs.readFileSync(args.bootstrapFile, 'utf-8');
    }
    // Add input files
    if (args.files && args.files.length > 0) {
      payload.input_files = args.files.map(filepath => {
        try {
          const content = fs.readFileSync(filepath);
          return {
            filename: path.basename(filepath),
            content_base64: content.toString('base64')
          };
        } catch (e) {
          console.error(`${RED}Error: Input file not found: ${filepath}${RESET}`);
          process.exit(1);
        }
      });
    }
    if (args.network) payload.network = args.network;
    if (args.vcpu) payload.vcpu = args.vcpu;

    const result = await apiRequest("/services", "POST", payload, keys);
    console.log(`${GREEN}Service created: ${result.id || 'N/A'}${RESET}`);
    console.log(`Name: ${result.name || 'N/A'}`);
    if (result.url) console.log(`URL: ${result.url}`);
    return;
  }

  console.error(`${RED}Error: Specify --name to create a service, or use --list, --info, etc.${RESET}`);
  process.exit(1);
}

function openBrowser(url: string): void {
  const { exec } = require('child_process');
  const platform = process.platform;
  let command: string;

  if (platform === 'darwin') {
    command = `open "${url}"`;
  } else if (platform === 'win32') {
    command = `start "${url}"`;
  } else {
    command = `xdg-open "${url}"`;
  }

  exec(command, (error: any) => {
    if (error) {
      console.error(`${RED}Error opening browser: ${error.message}${RESET}`);
      console.log(`Please visit: ${url}`);
    }
  });
}

async function validateKey(keys: ApiKeys, shouldExtend: boolean): Promise<void> {
  try {
    const result = await portalRequest("/keys/validate", "POST", {}, keys);

    // Handle --extend flag first
    if (shouldExtend) {
      const public_key = result.public_key;
      if (public_key) {
        const extendUrl = `${PORTAL_BASE}/keys/extend?pk=${encodeURIComponent(public_key)}`;
        console.log(`${BLUE}Opening browser to extend key...${RESET}`);
        openBrowser(extendUrl);
        return;
      } else {
        console.error(`${RED}Error: Could not retrieve public key${RESET}`);
        process.exit(1);
      }
    }

    // Check if key is expired
    if (result.expired) {
      console.log(`${RED}Expired${RESET}`);
      console.log(`Public Key: ${result.public_key || 'N/A'}`);
      console.log(`Tier: ${result.tier || 'N/A'}`);
      console.log(`Expired: ${result.expires_at || 'N/A'}`);
      console.log(`${YELLOW}To renew: Visit https://unsandbox.com/keys/extend${RESET}`);
      process.exit(1);
    }

    // Valid key
    console.log(`${GREEN}Valid${RESET}`);
    console.log(`Public Key: ${result.public_key || 'N/A'}`);
    console.log(`Tier: ${result.tier || 'N/A'}`);
    console.log(`Status: ${result.status || 'N/A'}`);
    console.log(`Expires: ${result.expires_at || 'N/A'}`);
    console.log(`Time Remaining: ${result.time_remaining || 'N/A'}`);
    console.log(`Rate Limit: ${result.rate_limit || 'N/A'}`);
    console.log(`Burst: ${result.burst || 'N/A'}`);
    console.log(`Concurrency: ${result.concurrency || 'N/A'}`);
  } catch (error: any) {
    console.error(`${RED}Error validating key: ${error.message}${RESET}`);
    process.exit(1);
  }
}

async function cmdKey(args: Args): Promise<void> {
  const keys = getApiKeys(args.apiKey);
  await validateKey(keys, args.extend);
}

function parseArgs(argv: string[]): Args {
  const args: Args = {
    command: null,
    sourceFile: null,
    env: [],
    files: [],
    artifacts: false,
    outputDir: null,
    network: null,
    vcpu: null,
    apiKey: null,
    shell: null,
    list: false,
    attach: null,
    kill: null,
    audit: false,
    tmux: false,
    screen: false,
    name: null,
    ports: null,
    domains: null,
    type: null,
    bootstrap: null,
    bootstrapFile: null,
    info: null,
    logs: null,
    tail: null,
    sleep: null,
    wake: null,
    destroy: null,
    execute: null,
    command_arg: null,
    dumpBootstrap: null,
    dumpFile: null,
    extend: false,
  };

  let i = 2;
  while (i < argv.length) {
    const arg = argv[i];

    if (arg === 'session' || arg === 'service' || arg === 'key') {
      args.command = arg;
      i++;
    } else if (arg === '-e' && i + 1 < argv.length) {
      args.env.push(argv[++i]);
      i++;
    } else if (arg === '-f' && i + 1 < argv.length) {
      args.files.push(argv[++i]);
      i++;
    } else if (arg === '-a') {
      args.artifacts = true;
      i++;
    } else if (arg === '-o' && i + 1 < argv.length) {
      args.outputDir = argv[++i];
      i++;
    } else if (arg === '-n' && i + 1 < argv.length) {
      args.network = argv[++i];
      i++;
    } else if (arg === '-v' && i + 1 < argv.length) {
      args.vcpu = parseInt(argv[++i]);
      i++;
    } else if (arg === '-k' && i + 1 < argv.length) {
      args.apiKey = argv[++i];
      i++;
    } else if (arg === '-s' || arg === '--shell') {
      args.shell = argv[++i];
      i++;
    } else if (arg === '-l' || arg === '--list') {
      args.list = true;
      i++;
    } else if (arg === '--attach' && i + 1 < argv.length) {
      args.attach = argv[++i];
      i++;
    } else if (arg === '--kill' && i + 1 < argv.length) {
      args.kill = argv[++i];
      i++;
    } else if (arg === '--audit') {
      args.audit = true;
      i++;
    } else if (arg === '--tmux') {
      args.tmux = true;
      i++;
    } else if (arg === '--screen') {
      args.screen = true;
      i++;
    } else if (arg === '--name' && i + 1 < argv.length) {
      args.name = argv[++i];
      i++;
    } else if (arg === '--ports' && i + 1 < argv.length) {
      args.ports = argv[++i];
      i++;
    } else if (arg === '--domains' && i + 1 < argv.length) {
      args.domains = argv[++i];
      i++;
    } else if (arg === '--type' && i + 1 < argv.length) {
      args.type = argv[++i];
      i++;
    } else if (arg === '--bootstrap' && i + 1 < argv.length) {
      args.bootstrap = argv[++i];
      i++;
    } else if (arg === '--bootstrap-file' && i + 1 < argv.length) {
      args.bootstrapFile = argv[++i];
      i++;
    } else if (arg === '--info' && i + 1 < argv.length) {
      args.info = argv[++i];
      i++;
    } else if (arg === '--logs' && i + 1 < argv.length) {
      args.logs = argv[++i];
      i++;
    } else if (arg === '--tail' && i + 1 < argv.length) {
      args.tail = argv[++i];
      i++;
    } else if (arg === '--freeze' && i + 1 < argv.length) {
      args.sleep = argv[++i];
      i++;
    } else if (arg === '--unfreeze' && i + 1 < argv.length) {
      args.wake = argv[++i];
      i++;
    } else if (arg === '--destroy' && i + 1 < argv.length) {
      args.destroy = argv[++i];
      i++;
    } else if (arg === '--execute' && i + 1 < argv.length) {
      args.execute = argv[++i];
      i++;
    } else if (arg === '--command' && i + 1 < argv.length) {
      args.command_arg = argv[++i];
      i++;
    } else if (arg === '--dump-bootstrap' && i + 1 < argv.length) {
      args.dumpBootstrap = argv[++i];
      i++;
    } else if (arg === '--dump-file' && i + 1 < argv.length) {
      args.dumpFile = argv[++i];
      i++;
    } else if (arg === '--extend') {
      args.extend = true;
      i++;
    } else if (!arg.startsWith('-')) {
      args.sourceFile = arg;
      i++;
    } else {
      console.error(`${RED}Unknown option: ${arg}${RESET}`);
      process.exit(1);
    }
  }

  return args;
}

async function main(): Promise<void> {
  const args = parseArgs(process.argv);

  if (args.command === 'session') {
    await cmdSession(args);
  } else if (args.command === 'service') {
    await cmdService(args);
  } else if (args.command === 'key') {
    await cmdKey(args);
  } else if (args.sourceFile) {
    await cmdExecute(args);
  } else {
    console.log(`Unsandbox CLI - Execute code in secure sandboxes

Usage:
  ${process.argv[1]} [options] <source_file>
  ${process.argv[1]} session [options]
  ${process.argv[1]} service [options]
  ${process.argv[1]} key [options]

Execute options:
  -e KEY=VALUE      Environment variable (multiple allowed)
  -f FILE          Input file (multiple allowed)
  -a               Return artifacts
  -o DIR           Output directory for artifacts
  -n MODE          Network mode (zerotrust|semitrusted)
  -v N             vCPU count (1-8)
  -k KEY           API key

Session options:
  -s, --shell NAME  Shell/REPL (default: bash)
  -l, --list       List sessions
  --attach ID      Attach to session
  --kill ID        Terminate session
  --audit          Record session
  --tmux           Enable tmux persistence
  --screen         Enable screen persistence

Service options:
  --name NAME      Service name
  --ports PORTS    Comma-separated ports
  --domains DOMAINS Custom domains
  --type TYPE      Service type (minecraft|mumble|teamspeak|source|tcp|udp)
  --bootstrap CMD  Bootstrap command or URI
  --bootstrap-file FILE  Upload local file as bootstrap script
  -l, --list       List services
  --info ID        Get service details
  --logs ID        Get all logs
  --tail ID        Get last 9000 lines
  --freeze ID       Freeze service
  --unfreeze ID        Unfreeze service
  --destroy ID     Destroy service
  --execute ID     Execute command in service
  --command CMD    Command to execute (with --execute)
  --dump-bootstrap ID  Dump bootstrap script
  --dump-file FILE     File to save bootstrap (with --dump-bootstrap)

Key options:
  --extend         Open browser to extend key expiration
`);
    process.exit(1);
  }
}

main().catch(err => {
  console.error(`${RED}${err}${RESET}`);
  process.exit(1);
});
