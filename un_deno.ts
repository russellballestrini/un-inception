#!/usr/bin/env -S deno run --allow-read --allow-env --allow-net
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

// unsandbox CLI - Deno TypeScript implementation
// Full-featured CLI matching un.c/un.py capabilities

const API_BASE = "https://api.unsandbox.com";
const BLUE = "\x1b[34m";
const RED = "\x1b[31m";
const GREEN = "\x1b[32m";
const YELLOW = "\x1b[33m";
const RESET = "\x1b[0m";

const EXT_MAP: Record<string, string> = {
  py: "python", js: "javascript", ts: "typescript",
  rb: "ruby", php: "php", pl: "perl", lua: "lua",
  sh: "bash", go: "go", rs: "rust", c: "c",
  cpp: "cpp", cc: "cpp", cxx: "cpp",
  java: "java", kt: "kotlin", cs: "csharp", fs: "fsharp",
  hs: "haskell", ml: "ocaml", clj: "clojure", scm: "scheme",
  lisp: "commonlisp", erl: "erlang", ex: "elixir", exs: "elixir",
  jl: "julia", r: "r", R: "r", cr: "crystal",
  d: "d", nim: "nim", zig: "zig", v: "vlang",
  dart: "dart", groovy: "groovy", scala: "scala",
  f90: "fortran", f95: "fortran", cob: "cobol",
  pro: "prolog", forth: "forth", "4th": "forth",
  tcl: "tcl", raku: "raku", pl6: "raku", p6: "raku",
  m: "objc",
};

function getApiKey(): string {
  const key = Deno.env.get("UNSANDBOX_API_KEY");
  if (!key) {
    console.error(`${RED}Error: UNSANDBOX_API_KEY not set${RESET}`);
    Deno.exit(1);
  }
  return key;
}

function detectLanguage(filename: string): string {
  const ext = filename.split(".").pop();
  if (!ext) {
    console.error(`${RED}Error: No file extension found${RESET}`);
    Deno.exit(1);
  }

  const language = EXT_MAP[ext];
  if (!language) {
    console.error(`${RED}Error: Unknown file extension '${ext}'${RESET}`);
    Deno.exit(1);
  }

  return language;
}

async function apiRequest(
  endpoint: string,
  method: string,
  data?: unknown,
  apiKey?: string,
): Promise<any> {
  const url = `${API_BASE}${endpoint}`;
  const headers: Record<string, string> = {
    Authorization: `Bearer ${apiKey}`,
    "Content-Type": "application/json",
  };

  const options: RequestInit = {
    method,
    headers,
  };

  if (data && method !== "GET") {
    options.body = JSON.stringify(data);
  }

  const response = await fetch(url, options);

  if (!response.ok) {
    console.error(`${RED}Error: HTTP ${response.status}${RESET}`);
    const errorText = await response.text();
    console.error(errorText);
    Deno.exit(1);
  }

  return await response.json();
}

async function cmdExecute(args: string[]) {
  const apiKey = getApiKey();
  let sourceFile = "";
  const envVars: Record<string, string> = {};
  const inputFiles: string[] = [];
  let artifacts = false;
  let outputDir = ".";
  let network = "";
  let vcpu = 0;

  // Parse arguments
  for (let i = 0; i < args.length; i++) {
    const arg = args[i];
    switch (arg) {
      case "-e":
        if (i + 1 < args.length) {
          const [key, ...valueParts] = args[++i].split("=");
          envVars[key] = valueParts.join("=");
        }
        break;
      case "-f":
        if (i + 1 < args.length) {
          inputFiles.push(args[++i]);
        }
        break;
      case "-a":
        artifacts = true;
        break;
      case "-o":
        if (i + 1 < args.length) {
          outputDir = args[++i];
        }
        break;
      case "-n":
        if (i + 1 < args.length) {
          network = args[++i];
        }
        break;
      case "-v":
        if (i + 1 < args.length) {
          vcpu = parseInt(args[++i]);
        }
        break;
      default:
        sourceFile = arg;
    }
  }

  if (!sourceFile) {
    console.error("Usage: un_deno.ts [options] <source_file>");
    Deno.exit(1);
  }

  try {
    await Deno.stat(sourceFile);
  } catch {
    console.error(`${RED}Error: File not found: ${sourceFile}${RESET}`);
    Deno.exit(1);
  }

  // Read source file
  const code = await Deno.readTextFile(sourceFile);
  const language = detectLanguage(sourceFile);

  // Build request payload
  const payload: any = {
    language,
    code,
  };

  if (Object.keys(envVars).length > 0) {
    payload.env = envVars;
  }

  if (inputFiles.length > 0) {
    const files = [];
    for (const filepath of inputFiles) {
      try {
        await Deno.stat(filepath);
      } catch {
        console.error(`${RED}Error: Input file not found: ${filepath}${RESET}`);
        Deno.exit(1);
      }
      const content = await Deno.readFile(filepath);
      const encoder = new TextDecoder("latin1");
      const b64Content = btoa(encoder.decode(content));
      files.push({
        filename: filepath.split("/").pop(),
        content_base64: b64Content,
      });
    }
    payload.input_files = files;
  }

  if (artifacts) {
    payload.return_artifacts = true;
  }
  if (network) {
    payload.network = network;
  }
  if (vcpu > 0) {
    payload.vcpu = vcpu;
  }

  // Execute
  const result = await apiRequest("/execute", "POST", payload, apiKey);

  // Print output
  if (result.stdout) {
    Deno.stdout.writeSync(new TextEncoder().encode(`${BLUE}${result.stdout}${RESET}`));
  }
  if (result.stderr) {
    Deno.stderr.writeSync(new TextEncoder().encode(`${RED}${result.stderr}${RESET}`));
  }

  // Save artifacts
  if (artifacts && result.artifacts) {
    await Deno.mkdir(outputDir, { recursive: true });
    for (const artifact of result.artifacts) {
      const filename = artifact.filename;
      const decoder = new TextDecoder("latin1");
      const content = Uint8Array.from(atob(artifact.content_base64), (c) => c.charCodeAt(0));
      const path = `${outputDir}/${filename}`;
      await Deno.writeFile(path, content, { mode: 0o755 });
      console.error(`${GREEN}Saved: ${path}${RESET}`);
    }
  }

  Deno.exit(result.exit_code || 0);
}

async function cmdSession(args: string[]) {
  const apiKey = getApiKey();
  let listMode = false;
  let killId = "";
  let shell = "";
  let network = "";
  let vcpu = 0;

  // Parse arguments
  for (let i = 0; i < args.length; i++) {
    const arg = args[i];
    switch (arg) {
      case "--list":
        listMode = true;
        break;
      case "--kill":
        if (i + 1 < args.length) {
          killId = args[++i];
        }
        break;
      case "--shell":
        if (i + 1 < args.length) {
          shell = args[++i];
        }
        break;
      case "-n":
        if (i + 1 < args.length) {
          network = args[++i];
        }
        break;
      case "-v":
        if (i + 1 < args.length) {
          vcpu = parseInt(args[++i]);
        }
        break;
    }
  }

  if (listMode) {
    const result = await apiRequest("/sessions", "GET", undefined, apiKey);
    const sessions = result.sessions || [];
    if (sessions.length === 0) {
      console.log("No active sessions");
    } else {
      console.log(
        `${"ID".padEnd(40)} ${"Shell".padEnd(10)} ${"Status".padEnd(10)} Created`,
      );
      for (const s of sessions) {
        console.log(
          `${s.id.padEnd(40)} ${s.shell.padEnd(10)} ${s.status.padEnd(10)} ${s.created_at}`,
        );
      }
    }
    return;
  }

  if (killId) {
    await apiRequest(`/sessions/${killId}`, "DELETE", undefined, apiKey);
    console.log(`${GREEN}Session terminated: ${killId}${RESET}`);
    return;
  }

  // Create new session
  const payload: any = {
    shell: shell || "bash",
  };
  if (network) payload.network = network;
  if (vcpu > 0) payload.vcpu = vcpu;

  console.log(`${YELLOW}Creating session...${RESET}`);
  const result = await apiRequest("/sessions", "POST", payload, apiKey);
  console.log(`${GREEN}Session created: ${result.id}${RESET}`);
  console.log(
    `${YELLOW}(Interactive sessions require WebSocket - use un2 for full support)${RESET}`,
  );
}

async function cmdService(args: string[]) {
  const apiKey = getApiKey();
  let listMode = false;
  let infoId = "";
  let logsId = "";
  let sleepId = "";
  let wakeId = "";
  let destroyId = "";
  let name = "";
  let ports = "";
  let bootstrap = "";
  let network = "";
  let vcpu = 0;

  // Parse arguments
  for (let i = 0; i < args.length; i++) {
    const arg = args[i];
    switch (arg) {
      case "--list":
        listMode = true;
        break;
      case "--info":
        if (i + 1 < args.length) {
          infoId = args[++i];
        }
        break;
      case "--logs":
        if (i + 1 < args.length) {
          logsId = args[++i];
        }
        break;
      case "--sleep":
        if (i + 1 < args.length) {
          sleepId = args[++i];
        }
        break;
      case "--wake":
        if (i + 1 < args.length) {
          wakeId = args[++i];
        }
        break;
      case "--destroy":
        if (i + 1 < args.length) {
          destroyId = args[++i];
        }
        break;
      case "--name":
        if (i + 1 < args.length) {
          name = args[++i];
        }
        break;
      case "--ports":
        if (i + 1 < args.length) {
          ports = args[++i];
        }
        break;
      case "--bootstrap":
        if (i + 1 < args.length) {
          bootstrap = args[++i];
        }
        break;
      case "-n":
        if (i + 1 < args.length) {
          network = args[++i];
        }
        break;
      case "-v":
        if (i + 1 < args.length) {
          vcpu = parseInt(args[++i]);
        }
        break;
    }
  }

  if (listMode) {
    const result = await apiRequest("/services", "GET", undefined, apiKey);
    const services = result.services || [];
    if (services.length === 0) {
      console.log("No services");
    } else {
      console.log(
        `${"ID".padEnd(20)} ${"Name".padEnd(15)} ${"Status".padEnd(10)} ${"Ports".padEnd(15)} Domains`,
      );
      for (const s of services) {
        const portStr = (s.ports || []).join(",");
        const domainStr = (s.domains || []).join(",");
        console.log(
          `${s.id.padEnd(20)} ${s.name.padEnd(15)} ${s.status.padEnd(10)} ${portStr.padEnd(15)} ${domainStr}`,
        );
      }
    }
    return;
  }

  if (infoId) {
    const result = await apiRequest(`/services/${infoId}`, "GET", undefined, apiKey);
    console.log(JSON.stringify(result, null, 2));
    return;
  }

  if (logsId) {
    const result = await apiRequest(`/services/${logsId}/logs`, "GET", undefined, apiKey);
    console.log(result.logs || "");
    return;
  }

  if (sleepId) {
    await apiRequest(`/services/${sleepId}/sleep`, "POST", undefined, apiKey);
    console.log(`${GREEN}Service sleeping: ${sleepId}${RESET}`);
    return;
  }

  if (wakeId) {
    await apiRequest(`/services/${wakeId}/wake`, "POST", undefined, apiKey);
    console.log(`${GREEN}Service waking: ${wakeId}${RESET}`);
    return;
  }

  if (destroyId) {
    await apiRequest(`/services/${destroyId}`, "DELETE", undefined, apiKey);
    console.log(`${GREEN}Service destroyed: ${destroyId}${RESET}`);
    return;
  }

  // Create new service
  if (name) {
    const payload: any = { name };

    if (ports) {
      payload.ports = ports.split(",").map((p) => parseInt(p));
    }

    if (bootstrap) {
      // Check if bootstrap is a file
      try {
        const stat = await Deno.stat(bootstrap);
        if (stat.isFile) {
          payload.bootstrap = await Deno.readTextFile(bootstrap);
        } else {
          payload.bootstrap = bootstrap;
        }
      } catch {
        payload.bootstrap = bootstrap;
      }
    }

    if (network) payload.network = network;
    if (vcpu > 0) payload.vcpu = vcpu;

    const result = await apiRequest("/services", "POST", payload, apiKey);
    console.log(`${GREEN}Service created: ${result.id}${RESET}`);
    console.log(`Name: ${result.name}`);
    if (result.url) {
      console.log(`URL: ${result.url}`);
    }
    return;
  }

  console.error(
    `${RED}Error: Specify --name to create a service, or use --list, --info, etc.${RESET}`,
  );
  Deno.exit(1);
}

async function main() {
  const args = Deno.args;

  if (args.length === 0) {
    console.error("Usage: un_deno.ts [options] <source_file>");
    console.error("       un_deno.ts session [options]");
    console.error("       un_deno.ts service [options]");
    Deno.exit(1);
  }

  const firstArg = args[0];

  if (firstArg === "session") {
    await cmdSession(args.slice(1));
  } else if (firstArg === "service") {
    await cmdService(args.slice(1));
  } else {
    await cmdExecute(args);
  }
}

main();
