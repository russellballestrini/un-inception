// PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
//
// This is free public domain software for the public good of a permacomputer hosted
// at permacomputer.com - an always-on computer by the people, for the people. One
// which is durable, easy to repair, and distributed like tap water for machine
// learning intelligence.
//
// The permacomputer is community-owned infrastructure optimized around four values:
//
//   TRUTH    - Source code must be open source & freely distributed
//   FREEDOM  - Voluntary participation without corporate control
//   HARMONY  - Systems operating with minimal waste that self-renew
//   LOVE     - Individual rights protected while fostering cooperation
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


// UN CLI - D Implementation (using curl subprocess for simplicity)
// Compile: dmd un.d -of=un_d
// Or with LDC: ldc2 un.d -of=un_d
// Usage:
//   un_d script.py
//   un_d -e KEY=VALUE script.py
//   un_d session --list
//   un_d service --name web --ports 8080

import std.stdio;
import std.file;
import std.path;
import std.process;
import std.string;
import std.conv;
import std.array;
import std.algorithm;

immutable string API_BASE = "https://api.unsandbox.com";
immutable string BLUE = "\033[34m";
immutable string RED = "\033[31m";
immutable string GREEN = "\033[32m";
immutable string YELLOW = "\033[33m";
immutable string RESET = "\033[0m";

string detectLanguage(string filename) {
    string[string] langMap = [
        ".py": "python", ".js": "javascript", ".ts": "typescript",
        ".go": "go", ".rs": "rust", ".c": "c", ".cpp": "cpp",
        ".d": "d", ".zig": "zig", ".nim": "nim", ".v": "v",
        ".rb": "ruby", ".php": "php", ".sh": "bash"
    ];

    string ext = extension(filename);
    return langMap.get(ext, "");
}

string escapeJson(string s) {
    string result;
    foreach (c; s) {
        switch (c) {
            case '"': result ~= "\\\""; break;
            case '\\': result ~= "\\\\"; break;
            case '\n': result ~= "\\n"; break;
            case '\r': result ~= "\\r"; break;
            case '\t': result ~= "\\t"; break;
            default: result ~= c; break;
        }
    }
    return result;
}

string execCurl(string cmd) {
    auto result = executeShell(cmd);
    return result.output;
}

void cmdExecute(string sourceFile, string[] envs, bool artifacts, string network, int vcpu, string apiKey) {
    string lang = detectLanguage(sourceFile);
    if (lang.empty) {
        stderr.writefln("%sError: Cannot detect language%s", RED, RESET);
        exit(1);
    }

    string code = readText(sourceFile);
    string json = format(`{"language":"%s","code":"%s"`, lang, escapeJson(code));

    if (envs.length > 0) {
        json ~= `,"env":{`;
        foreach (i, e; envs) {
            auto parts = e.split("=");
            if (parts.length == 2) {
                if (i > 0) json ~= ",";
                json ~= format(`"%s":"%s"`, parts[0], escapeJson(parts[1]));
            }
        }
        json ~= "}";
    }

    if (artifacts) json ~= `,"return_artifacts":true`;
    if (!network.empty) json ~= format(`,"network":"%s"`, network);
    if (vcpu > 0) json ~= format(`,"vcpu":%d`, vcpu);
    json ~= "}";

    string cmd = format(`curl -s -X POST '%s/execute' -H 'Content-Type: application/json' -H 'Authorization: Bearer %s' -d '%s'`, API_BASE, apiKey, json);
    string result = execCurl(cmd);

    writeln(result);
}

void cmdSession(bool list, string kill, string shell, string network, int vcpu, bool tmux, bool screen, string apiKey) {
    if (list) {
        string cmd = format(`curl -s -X GET '%s/sessions' -H 'Authorization: Bearer %s'`, API_BASE, apiKey);
        writeln(execCurl(cmd));
        return;
    }

    if (!kill.empty) {
        string cmd = format(`curl -s -X DELETE '%s/sessions/%s' -H 'Authorization: Bearer %s'`, API_BASE, kill, apiKey);
        execCurl(cmd);
        writefln("%sSession terminated: %s%s", GREEN, kill, RESET);
        return;
    }

    string json = format(`{"shell":"%s"`, shell.empty ? "bash" : shell);
    if (!network.empty) json ~= format(`,"network":"%s"`, network);
    if (vcpu > 0) json ~= format(`,"vcpu":%d`, vcpu);
    if (tmux) json ~= `,"persistence":"tmux"`;
    if (screen) json ~= `,"persistence":"screen"`;
    json ~= "}";

    writefln("%sCreating session...%s", YELLOW, RESET);
    string cmd = format(`curl -s -X POST '%s/sessions' -H 'Content-Type: application/json' -H 'Authorization: Bearer %s' -d '%s'`, API_BASE, apiKey, json);
    writeln(execCurl(cmd));
}

void cmdService(string name, string ports, string bootstrap, bool list, string info, string logs, string tail, string sleep, string wake, string destroy, string network, int vcpu, string apiKey) {
    if (list) {
        string cmd = format(`curl -s -X GET '%s/services' -H 'Authorization: Bearer %s'`, API_BASE, apiKey);
        writeln(execCurl(cmd));
        return;
    }

    if (!info.empty) {
        string cmd = format(`curl -s -X GET '%s/services/%s' -H 'Authorization: Bearer %s'`, API_BASE, info, apiKey);
        writeln(execCurl(cmd));
        return;
    }

    if (!logs.empty) {
        string cmd = format(`curl -s -X GET '%s/services/%s/logs' -H 'Authorization: Bearer %s'`, API_BASE, logs, apiKey);
        write(execCurl(cmd));
        return;
    }

    if (!tail.empty) {
        string cmd = format(`curl -s -X GET '%s/services/%s/logs?lines=9000' -H 'Authorization: Bearer %s'`, API_BASE, tail, apiKey);
        write(execCurl(cmd));
        return;
    }

    if (!sleep.empty) {
        string cmd = format(`curl -s -X POST '%s/services/%s/sleep' -H 'Authorization: Bearer %s'`, API_BASE, sleep, apiKey);
        execCurl(cmd);
        writefln("%sService sleeping: %s%s", GREEN, sleep, RESET);
        return;
    }

    if (!wake.empty) {
        string cmd = format(`curl -s -X POST '%s/services/%s/wake' -H 'Authorization: Bearer %s'`, API_BASE, wake, apiKey);
        execCurl(cmd);
        writefln("%sService waking: %s%s", GREEN, wake, RESET);
        return;
    }

    if (!destroy.empty) {
        string cmd = format(`curl -s -X DELETE '%s/services/%s' -H 'Authorization: Bearer %s'`, API_BASE, destroy, apiKey);
        execCurl(cmd);
        writefln("%sService destroyed: %s%s", GREEN, destroy, RESET);
        return;
    }

    if (!name.empty) {
        string json = format(`{"name":"%s"`, name);
        if (!ports.empty) json ~= format(`,"ports":[%s]`, ports);
        if (!bootstrap.empty) {
            if (exists(bootstrap)) {
                string bootCode = readText(bootstrap);
                json ~= format(`,"bootstrap":"%s"`, escapeJson(bootCode));
            } else {
                json ~= format(`,"bootstrap":"%s"`, escapeJson(bootstrap));
            }
        }
        if (!network.empty) json ~= format(`,"network":"%s"`, network);
        if (vcpu > 0) json ~= format(`,"vcpu":%d`, vcpu);
        json ~= "}";

        writefln("%sCreating service...%s", YELLOW, RESET);
        string cmd = format(`curl -s -X POST '%s/services' -H 'Content-Type: application/json' -H 'Authorization: Bearer %s' -d '%s'`, API_BASE, apiKey, json);
        writeln(execCurl(cmd));
        return;
    }

    stderr.writefln("%sError: Specify --name to create a service%s", RED, RESET);
    exit(1);
}

int main(string[] args) {
    string apiKey = environment.get("UNSANDBOX_API_KEY", "");

    if (args.length < 2) {
        stderr.writefln("Usage: %s [options] <source_file>", args[0]);
        stderr.writefln("       %s session [options]", args[0]);
        stderr.writefln("       %s service [options]", args[0]);
        return 1;
    }

    if (args[1] == "session") {
        bool list = false;
        string kill, shell, network;
        int vcpu = 0;
        bool tmux = false, screen = false;

        for (size_t i = 2; i < args.length; i++) {
            if (args[i] == "--list") list = true;
            else if (args[i] == "--kill" && i+1 < args.length) kill = args[++i];
            else if (args[i] == "--shell" && i+1 < args.length) shell = args[++i];
            else if (args[i] == "-n" && i+1 < args.length) network = args[++i];
            else if (args[i] == "-v" && i+1 < args.length) vcpu = to!int(args[++i]);
            else if (args[i] == "--tmux") tmux = true;
            else if (args[i] == "--screen") screen = true;
            else if (args[i] == "-k" && i+1 < args.length) apiKey = args[++i];
        }

        cmdSession(list, kill, shell, network, vcpu, tmux, screen, apiKey);
        return 0;
    }

    if (args[1] == "service") {
        string name, ports, bootstrap;
        bool list = false;
        string info, logs, tail, sleep, wake, destroy, network;
        int vcpu = 0;

        for (size_t i = 2; i < args.length; i++) {
            if (args[i] == "--name" && i+1 < args.length) name = args[++i];
            else if (args[i] == "--ports" && i+1 < args.length) ports = args[++i];
            else if (args[i] == "--bootstrap" && i+1 < args.length) bootstrap = args[++i];
            else if (args[i] == "--list") list = true;
            else if (args[i] == "--info" && i+1 < args.length) info = args[++i];
            else if (args[i] == "--logs" && i+1 < args.length) logs = args[++i];
            else if (args[i] == "--tail" && i+1 < args.length) tail = args[++i];
            else if (args[i] == "--sleep" && i+1 < args.length) sleep = args[++i];
            else if (args[i] == "--wake" && i+1 < args.length) wake = args[++i];
            else if (args[i] == "--destroy" && i+1 < args.length) destroy = args[++i];
            else if (args[i] == "-n" && i+1 < args.length) network = args[++i];
            else if (args[i] == "-v" && i+1 < args.length) vcpu = to!int(args[++i]);
            else if (args[i] == "-k" && i+1 < args.length) apiKey = args[++i];
        }

        cmdService(name, ports, bootstrap, list, info, logs, tail, sleep, wake, destroy, network, vcpu, apiKey);
        return 0;
    }

    // Execute mode
    string[] envs;
    bool artifacts = false;
    string network, sourceFile;
    int vcpu = 0;

    for (size_t i = 1; i < args.length; i++) {
        if (args[i] == "-e" && i+1 < args.length) envs ~= args[++i];
        else if (args[i] == "-a") artifacts = true;
        else if (args[i] == "-n" && i+1 < args.length) network = args[++i];
        else if (args[i] == "-v" && i+1 < args.length) vcpu = to!int(args[++i]);
        else if (args[i] == "-k" && i+1 < args.length) apiKey = args[++i];
        else if (!args[i].startsWith("-")) sourceFile = args[i];
    }

    if (sourceFile.empty) {
        stderr.writefln("%sError: No source file specified%s", RED, RESET);
        return 1;
    }

    cmdExecute(sourceFile, envs, artifacts, network, vcpu, apiKey);
    return 0;
}
