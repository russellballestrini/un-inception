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


#!/usr/bin/env groovy
// un.groovy - Unsandbox CLI Client (Groovy Implementation)
// Run: groovy un.groovy [options] <source_file>
// Requires: UNSANDBOX_API_KEY environment variable

def EXT_MAP = [
    '.java': 'java', '.kt': 'kotlin', '.cs': 'csharp', '.fs': 'fsharp',
    '.groovy': 'groovy', '.dart': 'dart', '.scala': 'scala',
    '.py': 'python', '.js': 'javascript', '.ts': 'typescript',
    '.rb': 'ruby', '.go': 'go', '.rs': 'rust', '.cpp': 'cpp', '.c': 'c',
    '.sh': 'bash', '.pl': 'perl', '.lua': 'lua', '.php': 'php',
    '.hs': 'haskell', '.ml': 'ocaml', '.clj': 'clojure', '.scm': 'scheme',
    '.lisp': 'commonlisp', '.erl': 'erlang', '.ex': 'elixir',
    '.jl': 'julia', '.r': 'r', '.cr': 'crystal', '.f90': 'fortran',
    '.cob': 'cobol', '.pro': 'prolog', '.forth': 'forth', '.tcl': 'tcl',
    '.raku': 'raku', '.d': 'd', '.nim': 'nim', '.zig': 'zig', '.v': 'v'
]

def API_BASE = 'https://api.unsandbox.com'
def PORTAL_BASE = 'https://unsandbox.com'
def BLUE = '\033[34m'
def RED = '\033[31m'
def GREEN = '\033[32m'
def YELLOW = '\033[33m'
def RESET = '\033[0m'

class Args {
    String command = null
    String sourceFile = null
    String apiKey = null
    String network = null
    Integer vcpu = 0
    List<String> env = []
    List<String> files = []
    Boolean artifacts = false
    String outputDir = null
    Boolean sessionList = false
    String sessionShell = null
    String sessionKill = null
    Boolean serviceList = false
    String serviceName = null
    String servicePorts = null
    String serviceType = null
    String serviceBootstrap = null
    String serviceBootstrapFile = null
    String serviceInfo = null
    String serviceLogs = null
    String serviceTail = null
    String serviceSleep = null
    String serviceWake = null
    String serviceDestroy = null
    String serviceExecute = null
    String serviceCommand = null
    String serviceDumpBootstrap = null
    String serviceDumpFile = null
    Boolean keyExtend = false
}

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

def getApiKeys(argsKey) {
    def publicKey = System.getenv('UNSANDBOX_PUBLIC_KEY')
    def secretKey = System.getenv('UNSANDBOX_SECRET_KEY')

    // Fall back to UNSANDBOX_API_KEY for backwards compatibility
    if (!publicKey || !secretKey) {
        def legacyKey = argsKey ?: System.getenv('UNSANDBOX_API_KEY')
        if (!legacyKey) {
            System.err.println("${RED}Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set${RESET}")
            System.exit(1)
        }
        return [legacyKey, null]
    }

    return [publicKey, secretKey]
}

def detectLanguage(filename) {
    def dotIndex = filename.lastIndexOf('.')
    if (dotIndex == -1) {
        System.err.println("${RED}Error: No file extension${RESET}")
        System.exit(1)
    }
    def ext = filename.substring(dotIndex)
    def language = EXT_MAP[ext]
    if (!language) {
        System.err.println("${RED}Error: Unsupported extension: ${ext}${RESET}")
        System.exit(1)
    }
    return language
}

def apiRequest(endpoint, method, data, publicKey, secretKey) {
    def tempFile = File.createTempFile('un_request_', '.json')
    try {
        def body = data ?: ""
        if (data) {
            tempFile.text = data
        }

        def curlCmd = ['curl', '-s', '-X', method, "${API_BASE}${endpoint}",
                       '-H', 'Content-Type: application/json']

        // Add HMAC authentication headers if secretKey is provided
        if (secretKey) {
            def timestamp = (System.currentTimeMillis() / 1000) as long
            def message = "${timestamp}:${method}:${endpoint}:${body}"

            def mac = Mac.getInstance("HmacSHA256")
            mac.init(new SecretKeySpec(secretKey.getBytes("UTF-8"), "HmacSHA256"))
            def signature = mac.doFinal(message.getBytes("UTF-8")).encodeHex().toString()

            curlCmd += ['-H', "Authorization: Bearer ${publicKey}"]
            curlCmd += ['-H', "X-Timestamp: ${timestamp}"]
            curlCmd += ['-H', "X-Signature: ${signature}"]
        } else {
            // Legacy API key authentication
            curlCmd += ['-H', "Authorization: Bearer ${publicKey}"]
        }

        if (data) {
            curlCmd += ['-d', "@${tempFile.absolutePath}"]
        }

        def proc = curlCmd.execute()
        def output = proc.text
        proc.waitFor()

        if (proc.exitValue() != 0) {
            System.err.println("${RED}Error: curl failed${RESET}")
            System.exit(1)
        }

        // Check for timestamp authentication errors
        if (output.toLowerCase().contains('timestamp') &&
            (output.contains('401') || output.toLowerCase().contains('expired') || output.toLowerCase().contains('invalid'))) {
            System.err.println("${RED}Error: Request timestamp expired (must be within 5 minutes of server time)${RESET}")
            System.err.println("${YELLOW}Your computer's clock may have drifted.${RESET}")
            System.err.println("Check your system time and sync with NTP if needed:")
            System.err.println("  Linux:   sudo ntpdate -s time.nist.gov")
            System.err.println("  macOS:   sudo sntp -sS time.apple.com")
            System.err.println("  Windows: w32tm /resync")
            System.exit(1)
        }

        return output
    } finally {
        tempFile.delete()
    }
}

def cmdExecute(args) {
    def (publicKey, secretKey) = getApiKeys(args.apiKey)
    def file = new File(args.sourceFile)
    if (!file.exists()) {
        System.err.println("${RED}Error: File not found: ${args.sourceFile}${RESET}")
        System.exit(1)
    }

    def code = file.text
    def language = detectLanguage(args.sourceFile)

    def escapedCode = code.replace('\\', '\\\\')
                          .replace('"', '\\"')
                          .replace('\n', '\\n')
                          .replace('\r', '\\r')
                          .replace('\t', '\\t')

    def json = """{"language":"${language}","code":"${escapedCode}""""

    if (args.env) {
        def envJson = args.env.collect { e ->
            def parts = e.split('=', 2)
            if (parts.size() == 2) {
                return "\"${parts[0]}\":\"${parts[1]}\""
            }
            return null
        }.findAll { it != null }.join(',')
        if (envJson) {
            json += ""","env":{${envJson}}"""
        }
    }

    if (args.files) {
        def filesJson = args.files.collect { filepath ->
            def f = new File(filepath)
            if (!f.exists()) {
                System.err.println("${RED}Error: Input file not found: ${filepath}${RESET}")
                System.exit(1)
            }
            def content = f.bytes.encodeBase64().toString()
            return """{"filename":"${f.name}","content_base64":"${content}"}"""
        }.join(',')
        json += ""","input_files":[${filesJson}]"""
    }

    if (args.artifacts) {
        json += ',"return_artifacts":true'
    }
    if (args.network) {
        json += ""","network":"${args.network}""""
    }
    if (args.vcpu > 0) {
        json += ""","vcpu":${args.vcpu}"""
    }

    json += '}'

    def output = apiRequest('/execute', 'POST', json, publicKey, secretKey)

    def stdoutMatch = output =~ /"stdout":"((?:[^"\\]|\\.)*)"/
    def stderrMatch = output =~ /"stderr":"((?:[^"\\]|\\.)*)"/
    def exitCodeMatch = output =~ /"exit_code":(\d+)/

    if (stdoutMatch.find()) {
        def stdout = stdoutMatch.group(1)
                               .replace('\\n', '\n')
                               .replace('\\t', '\t')
                               .replace('\\"', '"')
                               .replace('\\\\', '\\')
        print("${BLUE}${stdout}${RESET}")
    }

    if (stderrMatch.find()) {
        def stderr = stderrMatch.group(1)
                               .replace('\\n', '\n')
                               .replace('\\t', '\t')
                               .replace('\\"', '"')
                               .replace('\\\\', '\\')
        System.err.print("${RED}${stderr}${RESET}")
    }

    if (args.artifacts) {
        def artifactsMatch = output =~ /"artifacts":\[(.*?)\]/
        if (artifactsMatch.find()) {
            def outDir = args.outputDir ?: '.'
            new File(outDir).mkdirs()
            System.err.println("${GREEN}Artifacts saved to ${outDir}${RESET}")
        }
    }

    def exitCode = 0
    if (exitCodeMatch.find()) {
        exitCode = exitCodeMatch.group(1).toInteger()
    }

    System.exit(exitCode)
}

def cmdSession(args) {
    def (publicKey, secretKey) = getApiKeys(args.apiKey)

    if (args.sessionList) {
        def output = apiRequest('/sessions', 'GET', null, publicKey, secretKey)
        println("%-40s %-10s %-10s %s".format("ID", "Shell", "Status", "Created"))
        println("No sessions (list parsing not implemented)")
        return
    }

    if (args.sessionKill) {
        apiRequest("/sessions/${args.sessionKill}", 'DELETE', null, publicKey, secretKey)
        println("${GREEN}Session terminated: ${args.sessionKill}${RESET}")
        return
    }

    def json = """{"shell":"${args.sessionShell ?: 'bash'}""""
    if (args.network) {
        json += ""","network":"${args.network}""""
    }
    if (args.vcpu > 0) {
        json += ""","vcpu":${args.vcpu}"""
    }

    // Add input files
    if (args.files) {
        def filesJson = args.files.collect { filepath ->
            def f = new File(filepath)
            if (!f.exists()) {
                System.err.println("${RED}Error: Input file not found: ${filepath}${RESET}")
                System.exit(1)
            }
            def content = f.bytes.encodeBase64().toString()
            return """{"filename":"${f.name}","content_base64":"${content}"}"""
        }.join(',')
        json += ""","input_files":[${filesJson}]"""
    }

    json += '}'

    println("${YELLOW}Creating session...${RESET}")
    def output = apiRequest('/sessions', 'POST', json, publicKey, secretKey)
    def idMatch = output =~ /"id":"([^"]+)"/
    if (idMatch.find()) {
        println("${GREEN}Session created: ${idMatch.group(1)}${RESET}")
    } else {
        println("${GREEN}Session created${RESET}")
    }
    println("${YELLOW}(Interactive sessions require WebSocket - use un2 for full support)${RESET}")
}

def openBrowser(url) {
    def osName = System.getProperty('os.name').toLowerCase()
    try {
        if (osName.contains('linux')) {
            Runtime.runtime.exec(['xdg-open', url] as String[])
        } else if (osName.contains('mac')) {
            Runtime.runtime.exec(['open', url] as String[])
        } else if (osName.contains('win')) {
            Runtime.runtime.exec(['cmd', '/c', 'start', url] as String[])
        }
    } catch (Exception e) {
        System.err.println("${RED}Error opening browser: ${e.message}${RESET}")
    }
}

def cmdKey(args) {
    def (publicKey, secretKey) = getApiKeys(args.apiKey)

    def curlCmd = ['curl', '-s', '-X', 'POST', "${PORTAL_BASE}/keys/validate",
                   '-H', 'Content-Type: application/json']

    // Add HMAC authentication headers if secretKey is provided
    if (secretKey) {
        def timestamp = (System.currentTimeMillis() / 1000) as long
        def message = "${timestamp}:POST:/keys/validate:{}"

        def mac = Mac.getInstance("HmacSHA256")
        mac.init(new SecretKeySpec(secretKey.getBytes("UTF-8"), "HmacSHA256"))
        def signature = mac.doFinal(message.getBytes("UTF-8")).encodeHex().toString()

        curlCmd += ['-H', "Authorization: Bearer ${publicKey}"]
        curlCmd += ['-H', "X-Timestamp: ${timestamp}"]
        curlCmd += ['-H', "X-Signature: ${signature}"]
    } else {
        curlCmd += ['-H', "Authorization: Bearer ${publicKey}"]
    }

    curlCmd += ['-d', '{}']

    def proc = curlCmd.execute()
    def output = proc.text
    proc.waitFor()

    if (proc.exitValue() != 0) {
        println("${RED}Invalid${RESET}")
        System.err.println("${RED}Error: Failed to validate key${RESET}")
        System.exit(1)
    }

    def publicKeyMatch = output =~ /"public_key":"([^"]+)"/
    def tierMatch = output =~ /"tier":"([^"]+)"/
    def statusMatch = output =~ /"status":"([^"]+)"/
    def expiresAtMatch = output =~ /"expires_at":"([^"]+)"/
    def timeRemainingMatch = output =~ /"time_remaining":"([^"]+)"/
    def rateLimitMatch = output =~ /"rate_limit":([0-9.]+)/
    def burstMatch = output =~ /"burst":([0-9.]+)/
    def concurrencyMatch = output =~ /"concurrency":([0-9.]+)/
    def expiredMatch = output =~ /"expired":(true|false)/

    def publicKey = publicKeyMatch.find() ? publicKeyMatch.group(1) : 'N/A'
    def tier = tierMatch.find() ? tierMatch.group(1) : 'N/A'
    def status = statusMatch.find() ? statusMatch.group(1) : 'N/A'
    def expiresAt = expiresAtMatch.find() ? expiresAtMatch.group(1) : 'N/A'
    def timeRemaining = timeRemainingMatch.find() ? timeRemainingMatch.group(1) : 'N/A'
    def rateLimit = rateLimitMatch.find() ? rateLimitMatch.group(1) : 'N/A'
    def burst = burstMatch.find() ? burstMatch.group(1) : 'N/A'
    def concurrency = concurrencyMatch.find() ? concurrencyMatch.group(1) : 'N/A'
    def expired = expiredMatch.find() ? expiredMatch.group(1) == 'true' : false

    if (args.keyExtend && publicKey != 'N/A') {
        def extendUrl = "${PORTAL_BASE}/keys/extend?pk=${publicKey}"
        println("${BLUE}Opening browser to extend key...${RESET}")
        openBrowser(extendUrl)
        return
    }

    if (expired) {
        println("${RED}Expired${RESET}")
        println("Public Key: ${publicKey}")
        println("Tier: ${tier}")
        println("Expired: ${expiresAt}")
        println("${YELLOW}To renew: Visit https://unsandbox.com/keys/extend${RESET}")
        System.exit(1)
    }

    println("${GREEN}Valid${RESET}")
    println("Public Key: ${publicKey}")
    println("Tier: ${tier}")
    println("Status: ${status}")
    println("Expires: ${expiresAt}")
    println("Time Remaining: ${timeRemaining}")
    println("Rate Limit: ${rateLimit}")
    println("Burst: ${burst}")
    println("Concurrency: ${concurrency}")
}

def cmdService(args) {
    def (publicKey, secretKey) = getApiKeys(args.apiKey)

    if (args.serviceList) {
        def output = apiRequest('/services', 'GET', null, publicKey, secretKey)
        println("%-20s %-15s %-10s %-15s %s".format("ID", "Name", "Status", "Ports", "Domains"))
        println("No services (list parsing not implemented)")
        return
    }

    if (args.serviceInfo) {
        def output = apiRequest("/services/${args.serviceInfo}", 'GET', null, publicKey, secretKey)
        println(output)
        return
    }

    if (args.serviceLogs) {
        def output = apiRequest("/services/${args.serviceLogs}/logs", 'GET', null, publicKey, secretKey)
        def logsMatch = output =~ /"logs":"((?:[^"\\]|\\.)*)"/
        if (logsMatch.find()) {
            println(logsMatch.group(1).replace('\\n', '\n'))
        }
        return
    }

    if (args.serviceTail) {
        def output = apiRequest("/services/${args.serviceTail}/logs?lines=9000", 'GET', null, publicKey, secretKey)
        def logsMatch = output =~ /"logs":"((?:[^"\\]|\\.)*)"/
        if (logsMatch.find()) {
            println(logsMatch.group(1).replace('\\n', '\n'))
        }
        return
    }

    if (args.serviceSleep) {
        apiRequest("/services/${args.serviceSleep}/sleep", 'POST', null, publicKey, secretKey)
        println("${GREEN}Service sleeping: ${args.serviceSleep}${RESET}")
        return
    }

    if (args.serviceWake) {
        apiRequest("/services/${args.serviceWake}/wake", 'POST', null, publicKey, secretKey)
        println("${GREEN}Service waking: ${args.serviceWake}${RESET}")
        return
    }

    if (args.serviceDestroy) {
        apiRequest("/services/${args.serviceDestroy}", 'DELETE', null, publicKey, secretKey)
        println("${GREEN}Service destroyed: ${args.serviceDestroy}${RESET}")
        return
    }

    if (args.serviceExecute) {
        def json = """{"command":"${args.serviceCommand}"}"""
        def output = apiRequest("/services/${args.serviceExecute}/execute", 'POST', json, publicKey, secretKey)
        def stdoutMatch = output =~ /"stdout":"((?:[^"\\\\]|\\\\.)*)"/
        def stderrMatch = output =~ /"stderr":"((?:[^"\\\\]|\\\\.)*)"/

        if (stdoutMatch.find()) {
            def stdout = stdoutMatch.group(1)
                                   .replace('\\n', '\n')
                                   .replace('\\t', '\t')
                                   .replace('\\"', '"')
                                   .replace('\\\\', '\\')
            print("${BLUE}${stdout}${RESET}")
        }

        if (stderrMatch.find()) {
            def stderr = stderrMatch.group(1)
                                   .replace('\\n', '\n')
                                   .replace('\\t', '\t')
                                   .replace('\\"', '"')
                                   .replace('\\\\', '\\')
            System.err.print("${RED}${stderr}${RESET}")
        }
        return
    }

    if (args.serviceDumpBootstrap) {
        System.err.println("Fetching bootstrap script from ${args.serviceDumpBootstrap}...")
        def json = """{"command":"cat /tmp/bootstrap.sh"}"""
        def output = apiRequest("/services/${args.serviceDumpBootstrap}/execute", 'POST', json, publicKey, secretKey)

        def stdoutMatch = output =~ /"stdout":"((?:[^"\\\\]|\\\\.)*)"/
        if (stdoutMatch.find()) {
            def bootstrap = stdoutMatch.group(1)
                                      .replace('\\n', '\n')
                                      .replace('\\t', '\t')
                                      .replace('\\"', '"')
                                      .replace('\\\\', '\\')

            if (args.serviceDumpFile) {
                try {
                    new File(args.serviceDumpFile).text = bootstrap
                    "chmod 755 ${args.serviceDumpFile}".execute().waitFor()
                    println("Bootstrap saved to ${args.serviceDumpFile}")
                } catch (Exception e) {
                    System.err.println("${RED}Error: Could not write to ${args.serviceDumpFile}: ${e.message}${RESET}")
                    System.exit(1)
                }
            } else {
                print(bootstrap)
            }
        } else {
            System.err.println("${RED}Error: Failed to fetch bootstrap (service not running or no bootstrap file)${RESET}")
            System.exit(1)
        }
        return
    }

    if (args.serviceName) {
        def json = """{"name":"${args.serviceName}""""
        if (args.servicePorts) {
            def ports = args.servicePorts.split(',').collect { it.trim() }.join(',')
            json += ""","ports":[${ports}]"""
        }
        if (args.serviceType) {
            json += ""","service_type":"${args.serviceType}""""
        }
        if (args.serviceBootstrap) {
            def escaped = args.serviceBootstrap.replace('\\', '\\\\').replace('"', '\\"')
            json += ""","bootstrap":"${escaped}""""
        }
        if (args.serviceBootstrapFile) {
            def file = new File(args.serviceBootstrapFile)
            if (file.exists()) {
                def content = file.text.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n').replace('\r', '\\r').replace('\t', '\\t')
                json += ""","bootstrap_content":"${content}""""
            } else {
                System.err.println("${RED}Error: Bootstrap file not found: ${args.serviceBootstrapFile}${RESET}")
                System.exit(1)
            }
        }
        if (args.network) {
            json += ""","network":"${args.network}""""
        }
        if (args.vcpu > 0) {
            json += ""","vcpu":${args.vcpu}"""
        }

        // Add input files
        if (args.files) {
            def filesJson = args.files.collect { filepath ->
                def f = new File(filepath)
                if (!f.exists()) {
                    System.err.println("${RED}Error: Input file not found: ${filepath}${RESET}")
                    System.exit(1)
                }
                def content = f.bytes.encodeBase64().toString()
                return """{"filename":"${f.name}","content_base64":"${content}"}"""
            }.join(',')
            json += ""","input_files":[${filesJson}]"""
        }

        json += '}'

        def output = apiRequest('/services', 'POST', json, publicKey, secretKey)
        def idMatch = output =~ /"id":"([^"]+)"/
        if (idMatch.find()) {
            println("${GREEN}Service created: ${idMatch.group(1)}${RESET}")
        }
        def nameMatch = output =~ /"name":"([^"]+)"/
        if (nameMatch.find()) {
            println("Name: ${nameMatch.group(1)}")
        }
        def urlMatch = output =~ /"url":"([^"]+)"/
        if (urlMatch.find()) {
            println("URL: ${urlMatch.group(1)}")
        }
        return
    }

    System.err.println("${RED}Error: Specify --name to create a service, or use --list, --info, etc.${RESET}")
    System.exit(1)
}

def parseArgs(argv) {
    def args = new Args()
    def i = 0
    while (i < argv.size()) {
        switch (argv[i]) {
            case 'session':
                args.command = 'session'
                break
            case 'service':
                args.command = 'service'
                break
            case 'key':
                args.command = 'key'
                break
            case '-k':
            case '--api-key':
                args.apiKey = argv[++i]
                break
            case '-n':
            case '--network':
                args.network = argv[++i]
                break
            case '-v':
            case '--vcpu':
                args.vcpu = argv[++i].toInteger()
                break
            case '-e':
            case '--env':
                args.env << argv[++i]
                break
            case '-f':
            case '--files':
                args.files << argv[++i]
                break
            case '-a':
            case '--artifacts':
                args.artifacts = true
                break
            case '-o':
            case '--output-dir':
                args.outputDir = argv[++i]
                break
            case '-l':
            case '--list':
                if (args.command == 'session') args.sessionList = true
                else if (args.command == 'service') args.serviceList = true
                break
            case '-s':
            case '--shell':
                args.sessionShell = argv[++i]
                break
            case '--kill':
                args.sessionKill = argv[++i]
                break
            case '--name':
                args.serviceName = argv[++i]
                break
            case '--ports':
                args.servicePorts = argv[++i]
                break
            case '--type':
                args.serviceType = argv[++i]
                break
            case '--bootstrap':
                args.serviceBootstrap = argv[++i]
                break
            case '--bootstrap-file':
                args.serviceBootstrapFile = argv[++i]
                break
            case '--info':
                args.serviceInfo = argv[++i]
                break
            case '--logs':
                args.serviceLogs = argv[++i]
                break
            case '--tail':
                args.serviceTail = argv[++i]
                break
            case '--freeze':
                args.serviceSleep = argv[++i]
                break
            case '--unfreeze':
                args.serviceWake = argv[++i]
                break
            case '--destroy':
                args.serviceDestroy = argv[++i]
                break
            case '--execute':
                args.serviceExecute = argv[++i]
                break
            case '--command':
                args.serviceCommand = argv[++i]
                break
            case '--dump-bootstrap':
                args.serviceDumpBootstrap = argv[++i]
                break
            case '--dump-file':
                args.serviceDumpFile = argv[++i]
                break
            case '--extend':
                args.keyExtend = true
                break
            default:
                if (argv[i].startsWith('-')) {
                    System.err.println("${RED}Unknown option: ${argv[i]}${RESET}")
                    System.exit(1)
                } else {
                    args.sourceFile = argv[i]
                }
        }
        i++
    }
    return args
}

def printHelp() {
    println '''Usage: groovy un.groovy [options] <source_file>
       groovy un.groovy session [options]
       groovy un.groovy service [options]
       groovy un.groovy key [options]

Execute options:
  -e KEY=VALUE      Set environment variable
  -f FILE           Add input file
  -a                Return artifacts
  -o DIR            Output directory for artifacts
  -n MODE           Network mode (zerotrust/semitrusted)
  -v N              vCPU count (1-8)
  -k KEY            API key

Session options:
  --list            List active sessions
  --shell NAME      Shell/REPL to use
  --kill ID         Terminate session

Service options:
  --list            List services
  --name NAME       Service name
  --ports PORTS     Comma-separated ports
  --type TYPE       Service type (minecraft/mumble/teamspeak/source/tcp/udp)
  --bootstrap CMD   Bootstrap command
  --info ID         Get service details
  --logs ID         Get all logs
  --tail ID         Get last 9000 lines
  --freeze ID        Freeze service
  --unfreeze ID         Unfreeze service
  --destroy ID      Destroy service
  --execute ID      Execute command in service
  --command CMD     Command to execute (with --execute)
  --dump-bootstrap ID   Dump bootstrap script
  --dump-file FILE      File to save bootstrap (with --dump-bootstrap)

Key options:
  --extend          Open browser to extend key
  -k KEY            API key to validate
'''
}

// Main execution
try {
    def args = parseArgs(this.args as List)

    if (args.command == 'session') {
        cmdSession(args)
    } else if (args.command == 'service') {
        cmdService(args)
    } else if (args.command == 'key') {
        cmdKey(args)
    } else if (args.sourceFile) {
        cmdExecute(args)
    } else {
        printHelp()
        System.exit(1)
    }
} catch (Exception e) {
    System.err.println("${RED}Error: ${e.message}${RESET}")
    System.exit(1)
}
