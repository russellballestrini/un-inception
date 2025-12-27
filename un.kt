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


// un.kt - Unsandbox CLI Client (Kotlin Implementation)
// Compile: kotlinc un.kt -include-runtime -d un.jar
// Run: java -jar un.jar [options] <source_file>
// Requires: UNSANDBOX_API_KEY environment variable

import java.io.File
import java.net.HttpURLConnection
import java.net.URL
import java.util.Base64
import kotlin.system.exitProcess

val API_BASE = "https://api.unsandbox.com"
val PORTAL_BASE = "https://unsandbox.com"
val BLUE = "\u001B[34m"
val RED = "\u001B[31m"
val GREEN = "\u001B[32m"
val YELLOW = "\u001B[33m"
val RESET = "\u001B[0m"

val EXT_MAP = mapOf(
    ".py" to "python", ".js" to "javascript", ".ts" to "typescript",
    ".rb" to "ruby", ".php" to "php", ".pl" to "perl", ".lua" to "lua",
    ".sh" to "bash", ".go" to "go", ".rs" to "rust", ".c" to "c",
    ".cpp" to "cpp", ".cc" to "cpp", ".cxx" to "cpp",
    ".java" to "java", ".kt" to "kotlin", ".cs" to "csharp", ".fs" to "fsharp",
    ".hs" to "haskell", ".ml" to "ocaml", ".clj" to "clojure", ".scm" to "scheme",
    ".lisp" to "commonlisp", ".erl" to "erlang", ".ex" to "elixir", ".exs" to "elixir",
    ".jl" to "julia", ".r" to "r", ".R" to "r", ".cr" to "crystal",
    ".d" to "d", ".nim" to "nim", ".zig" to "zig", ".v" to "v",
    ".dart" to "dart", ".groovy" to "groovy", ".scala" to "scala",
    ".f90" to "fortran", ".f95" to "fortran", ".cob" to "cobol",
    ".pro" to "prolog", ".forth" to "forth", ".4th" to "forth",
    ".tcl" to "tcl", ".raku" to "raku", ".m" to "objc"
)

data class Args(
    var command: String? = null,
    var sourceFile: String? = null,
    var apiKey: String? = null,
    var network: String? = null,
    var vcpu: Int = 0,
    val env: MutableList<String> = mutableListOf(),
    val files: MutableList<String> = mutableListOf(),
    var artifacts: Boolean = false,
    var outputDir: String? = null,
    var sessionList: Boolean = false,
    var sessionShell: String? = null,
    var sessionKill: String? = null,
    var serviceList: Boolean = false,
    var serviceName: String? = null,
    var servicePorts: String? = null,
    var serviceType: String? = null,
    var serviceBootstrap: String? = null,
    var serviceInfo: String? = null,
    var serviceLogs: String? = null,
    var serviceTail: String? = null,
    var serviceSleep: String? = null,
    var serviceWake: String? = null,
    var serviceDestroy: String? = null,
    var serviceExecute: String? = null,
    var serviceCommand: String? = null,
    var serviceDumpBootstrap: String? = null,
    var serviceDumpFile: String? = null,
    var keyExtend: Boolean = false
)

fun main(args: Array<String>) {
    try {
        val parsedArgs = parseArgs(args)

        when (parsedArgs.command) {
            "session" -> cmdSession(parsedArgs)
            "service" -> cmdService(parsedArgs)
            "key" -> cmdKey(parsedArgs)
            else -> if (parsedArgs.sourceFile != null) {
                cmdExecute(parsedArgs)
            } else {
                printHelp()
                exitProcess(1)
            }
        }
    } catch (e: Exception) {
        System.err.println("${RED}Error: ${e.message}${RESET}")
        exitProcess(1)
    }
}

fun cmdExecute(args: Args) {
    val apiKey = getApiKey(args.apiKey)
    val code = File(args.sourceFile!!).readText()
    val language = detectLanguage(args.sourceFile!!)

    val payload = mutableMapOf<String, Any>(
        "language" to language,
        "code" to code
    )

    if (args.env.isNotEmpty()) {
        val envVars = mutableMapOf<String, String>()
        for (e in args.env) {
            val parts = e.split("=", limit = 2)
            if (parts.size == 2) {
                envVars[parts[0]] = parts[1]
            }
        }
        if (envVars.isNotEmpty()) {
            payload["env"] = envVars
        }
    }

    if (args.files.isNotEmpty()) {
        val inputFiles = mutableListOf<Map<String, String>>()
        for (filepath in args.files) {
            val content = File(filepath).readBytes()
            inputFiles.add(mapOf(
                "filename" to File(filepath).name,
                "content_base64" to Base64.getEncoder().encodeToString(content)
            ))
        }
        payload["input_files"] = inputFiles
    }

    if (args.artifacts) {
        payload["return_artifacts"] = true
    }
    if (args.network != null) {
        payload["network"] = args.network!!
    }
    if (args.vcpu > 0) {
        payload["vcpu"] = args.vcpu
    }

    val result = apiRequest("/execute", "POST", payload, apiKey)

    val stdout = result["stdout"] as? String
    val stderr = result["stderr"] as? String
    if (!stdout.isNullOrEmpty()) {
        print("$BLUE$stdout$RESET")
    }
    if (!stderr.isNullOrEmpty()) {
        System.err.print("$RED$stderr$RESET")
    }

    if (args.artifacts && result.containsKey("artifacts")) {
        @Suppress("UNCHECKED_CAST")
        val artifacts = result["artifacts"] as? List<Map<String, String>>
        val outDir = args.outputDir ?: "."
        File(outDir).mkdirs()
        artifacts?.forEach { artifact ->
            val filename = artifact["filename"] ?: "artifact"
            val content = Base64.getDecoder().decode(artifact["content_base64"])
            val file = File(outDir, filename)
            file.writeBytes(content)
            file.setExecutable(true)
            System.err.println("${GREEN}Saved: ${file.path}${RESET}")
        }
    }

    val exitCode = (result["exit_code"] as? Number)?.toInt() ?: 0
    exitProcess(exitCode)
}

fun cmdSession(args: Args) {
    val apiKey = getApiKey(args.apiKey)

    if (args.sessionList) {
        val result = apiRequest("/sessions", "GET", null, apiKey)
        @Suppress("UNCHECKED_CAST")
        val sessions = result["sessions"] as? List<Map<String, Any>>
        if (sessions.isNullOrEmpty()) {
            println("No active sessions")
        } else {
            println("%-40s %-10s %-10s %s".format("ID", "Shell", "Status", "Created"))
            for (s in sessions) {
                println("%-40s %-10s %-10s %s".format(
                    s["id"] ?: "N/A",
                    s["shell"] ?: "N/A",
                    s["status"] ?: "N/A",
                    s["created_at"] ?: "N/A"
                ))
            }
        }
        return
    }

    if (args.sessionKill != null) {
        apiRequest("/sessions/${args.sessionKill}", "DELETE", null, apiKey)
        println("${GREEN}Session terminated: ${args.sessionKill}${RESET}")
        return
    }

    val payload = mutableMapOf<String, Any>(
        "shell" to (args.sessionShell ?: "bash")
    )
    if (args.network != null) {
        payload["network"] = args.network!!
    }
    if (args.vcpu > 0) {
        payload["vcpu"] = args.vcpu
    }

    println("${YELLOW}Creating session...${RESET}")
    val result = apiRequest("/sessions", "POST", payload, apiKey)
    println("${GREEN}Session created: ${result["id"] ?: "N/A"}${RESET}")
    println("${YELLOW}(Interactive sessions require WebSocket - use un2 for full support)${RESET}")
}

fun cmdService(args: Args) {
    val apiKey = getApiKey(args.apiKey)

    if (args.serviceList) {
        val result = apiRequest("/services", "GET", null, apiKey)
        @Suppress("UNCHECKED_CAST")
        val services = result["services"] as? List<Map<String, Any>>
        if (services.isNullOrEmpty()) {
            println("No services")
        } else {
            println("%-20s %-15s %-10s %-15s %s".format("ID", "Name", "Status", "Ports", "Domains"))
            for (s in services) {
                @Suppress("UNCHECKED_CAST")
                val ports = (s["ports"] as? List<Int>)?.joinToString(",") ?: ""
                @Suppress("UNCHECKED_CAST")
                val domains = (s["domains"] as? List<String>)?.joinToString(",") ?: ""
                println("%-20s %-15s %-10s %-15s %s".format(
                    s["id"] ?: "N/A",
                    s["name"] ?: "N/A",
                    s["status"] ?: "N/A",
                    ports, domains
                ))
            }
        }
        return
    }

    if (args.serviceInfo != null) {
        val result = apiRequest("/services/${args.serviceInfo}", "GET", null, apiKey)
        println(toJson(result))
        return
    }

    if (args.serviceLogs != null) {
        val result = apiRequest("/services/${args.serviceLogs}/logs", "GET", null, apiKey)
        println(result["logs"] ?: "")
        return
    }

    if (args.serviceTail != null) {
        val result = apiRequest("/services/${args.serviceTail}/logs?lines=9000", "GET", null, apiKey)
        println(result["logs"] ?: "")
        return
    }

    if (args.serviceSleep != null) {
        apiRequest("/services/${args.serviceSleep}/sleep", "POST", null, apiKey)
        println("${GREEN}Service sleeping: ${args.serviceSleep}${RESET}")
        return
    }

    if (args.serviceWake != null) {
        apiRequest("/services/${args.serviceWake}/wake", "POST", null, apiKey)
        println("${GREEN}Service waking: ${args.serviceWake}${RESET}")
        return
    }

    if (args.serviceDestroy != null) {
        apiRequest("/services/${args.serviceDestroy}", "DELETE", null, apiKey)
        println("${GREEN}Service destroyed: ${args.serviceDestroy}${RESET}")
        return
    }

    if (args.serviceExecute != null) {
        val payload = mutableMapOf<String, Any>("command" to args.serviceCommand!!)
        val result = apiRequest("/services/${args.serviceExecute}/execute", "POST", payload, apiKey)
        if (result.containsKey("stdout")) {
            val stdout = result["stdout"] as? String
            if (stdout != null && stdout.isNotEmpty()) {
                print("$BLUE$stdout$RESET")
            }
        }
        if (result.containsKey("stderr")) {
            val stderr = result["stderr"] as? String
            if (stderr != null && stderr.isNotEmpty()) {
                System.err.print("$RED$stderr$RESET")
            }
        }
        return
    }

    if (args.serviceDumpBootstrap != null) {
        System.err.println("Fetching bootstrap script from ${args.serviceDumpBootstrap}...")
        val payload = mutableMapOf<String, Any>("command" to "cat /tmp/bootstrap.sh")
        val result = apiRequest("/services/${args.serviceDumpBootstrap}/execute", "POST", payload, apiKey)

        val bootstrap = result["stdout"] as? String
        if (bootstrap != null && bootstrap.isNotEmpty()) {
            if (args.serviceDumpFile != null) {
                try {
                    val file = java.io.File(args.serviceDumpFile!!)
                    file.writeText(bootstrap)
                    file.setExecutable(true)
                    println("Bootstrap saved to ${args.serviceDumpFile}")
                } catch (e: Exception) {
                    System.err.println("${RED}Error: Could not write to ${args.serviceDumpFile}: ${e.message}${RESET}")
                    exitProcess(1)
                }
            } else {
                print(bootstrap)
            }
        } else {
            System.err.println("${RED}Error: Failed to fetch bootstrap (service not running or no bootstrap file)${RESET}")
            exitProcess(1)
        }
        return
    }

    if (args.serviceName != null) {
        val payload = mutableMapOf<String, Any>("name" to args.serviceName!!)
        if (args.servicePorts != null) {
            payload["ports"] = args.servicePorts!!.split(",").map { it.trim().toInt() }
        }
        if (args.serviceType != null) {
            payload["service_type"] = args.serviceType!!
        }
        if (args.serviceBootstrap != null) {
            payload["bootstrap"] = args.serviceBootstrap!!
        }
        if (args.network != null) {
            payload["network"] = args.network!!
        }
        if (args.vcpu > 0) {
            payload["vcpu"] = args.vcpu
        }

        val result = apiRequest("/services", "POST", payload, apiKey)
        println("${GREEN}Service created: ${result["id"] ?: "N/A"}${RESET}")
        println("Name: ${result["name"] ?: "N/A"}")
        if (result.containsKey("url")) {
            println("URL: ${result["url"]}")
        }
        return
    }

    System.err.println("${RED}Error: Specify --name to create a service, or use --list, --info, etc.${RESET}")
    exitProcess(1)
}

fun cmdKey(args: Args) {
    val apiKey = getApiKey(args.apiKey)

    val result = validateKey(apiKey)
    val valid = result["valid"] as? Boolean ?: false
    val expired = result["expired"] as? Boolean ?: false
    val publicKey = result["public_key"] as? String ?: ""
    val tier = result["tier"] as? String ?: ""
    val expiresAt = result["expires_at"] as? String ?: ""

    if (args.keyExtend) {
        if (publicKey.isEmpty()) {
            System.err.println("${RED}Error: Could not retrieve public key${RESET}")
            exitProcess(1)
        }
        val extendUrl = "$PORTAL_BASE/keys/extend?pk=$publicKey"
        println("${YELLOW}Opening browser to extend key...${RESET}")
        println(extendUrl)

        // Try to open browser using common commands
        val osName = System.getProperty("os.name").lowercase()
        val openCmd = when {
            osName.contains("mac") || osName.contains("darwin") -> "open"
            osName.contains("win") -> "start"
            else -> "xdg-open"
        }

        try {
            Runtime.getRuntime().exec(arrayOf(openCmd, extendUrl))
        } catch (e: Exception) {
            println("${YELLOW}Could not open browser automatically. Please visit the URL above.${RESET}")
        }
        return
    }

    if (expired) {
        println("${RED}Status: Expired${RESET}")
        println("Public Key: $publicKey")
        println("Tier: $tier")
        println("Expired: $expiresAt")
        println("${YELLOW}To renew: Visit $PORTAL_BASE/keys/extend${RESET}")
    } else if (valid) {
        println("${GREEN}Status: Valid${RESET}")
        println("Public Key: $publicKey")
        println("Tier: $tier")
        println("Expires: $expiresAt")
    } else {
        println("${RED}Status: Invalid${RESET}")
        exitProcess(1)
    }
}

fun validateKey(apiKey: String): Map<String, Any> {
    val url = URL("$PORTAL_BASE/keys/validate")
    val connection = url.openConnection() as HttpURLConnection

    connection.requestMethod = "POST"
    connection.setRequestProperty("Authorization", "Bearer $apiKey")
    connection.setRequestProperty("Content-Type", "application/json")
    connection.connectTimeout = 30000
    connection.readTimeout = 30000

    if (connection.responseCode !in 200..299) {
        val error = connection.errorStream?.bufferedReader()?.readText() ?: ""
        throw RuntimeException("HTTP ${connection.responseCode} - $error")
    }

    val response = connection.inputStream.bufferedReader().readText()
    return parseJson(response)
}

fun getApiKey(argsKey: String?): String {
    val key = argsKey ?: System.getenv("UNSANDBOX_API_KEY")
    if (key.isNullOrEmpty()) {
        System.err.println("${RED}Error: UNSANDBOX_API_KEY not set${RESET}")
        exitProcess(1)
    }
    return key
}

fun detectLanguage(filename: String): String {
    val ext = filename.substringAfterLast('.', "")
    if (ext.isEmpty()) {
        throw RuntimeException("Cannot detect language: no file extension")
    }
    return EXT_MAP[".$ext"] ?: throw RuntimeException("Unsupported file extension: .$ext")
}

fun apiRequest(endpoint: String, method: String, data: Map<String, Any>?, apiKey: String): Map<String, Any> {
    val url = URL(API_BASE + endpoint)
    val connection = url.openConnection() as HttpURLConnection

    connection.requestMethod = method
    connection.setRequestProperty("Authorization", "Bearer $apiKey")
    connection.setRequestProperty("Content-Type", "application/json")
    connection.connectTimeout = 30000
    connection.readTimeout = 300000

    if (data != null) {
        connection.doOutput = true
        val json = toJson(data)
        connection.outputStream.use { it.write(json.toByteArray()) }
    }

    if (connection.responseCode !in 200..299) {
        val error = connection.errorStream?.bufferedReader()?.readText() ?: ""
        throw RuntimeException("HTTP ${connection.responseCode} - $error")
    }

    val response = connection.inputStream.bufferedReader().readText()
    return parseJson(response)
}

fun toJson(obj: Any?): String = when (obj) {
    null -> "null"
    is String -> "\"${obj.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t")}\""
    is Number -> obj.toString()
    is Boolean -> obj.toString()
    is Map<*, *> -> {
        val entries = obj.entries.joinToString(",") { (k, v) ->
            "\"$k\":${toJson(v)}"
        }
        "{$entries}"
    }
    is List<*> -> {
        val items = obj.joinToString(",") { toJson(it) }
        "[$items]"
    }
    else -> toJson(obj.toString())
}

fun parseJson(json: String): Map<String, Any> {
    val trimmed = json.trim()
    if (!trimmed.startsWith("{")) return emptyMap()

    val result = mutableMapOf<String, Any>()
    var i = 1

    while (i < trimmed.length) {
        while (i < trimmed.length && trimmed[i].isWhitespace()) i++
        if (trimmed[i] == '}') break

        if (trimmed[i] == '"') {
            val keyStart = ++i
            while (i < trimmed.length && trimmed[i] != '"') {
                if (trimmed[i] == '\\') i++
                i++
            }
            val key = trimmed.substring(keyStart, i).replace("\\\"", "\"").replace("\\\\", "\\")
            i++

            while (i < trimmed.length && (trimmed[i].isWhitespace() || trimmed[i] == ':')) i++

            val (value, endIdx) = parseJsonValue(trimmed, i)
            result[key] = value
            i = endIdx

            while (i < trimmed.length && (trimmed[i].isWhitespace() || trimmed[i] == ',')) i++
        } else {
            i++
        }
    }
    return result
}

fun parseJsonValue(json: String, start: Int): Pair<Any, Int> {
    var i = start
    while (i < json.length && json[i].isWhitespace()) i++

    return when {
        json[i] == '"' -> {
            i++
            val sb = StringBuilder()
            var escaped = false
            while (i < json.length) {
                val c = json[i]
                when {
                    escaped -> {
                        when (c) {
                            'n' -> sb.append('\n')
                            'r' -> sb.append('\r')
                            't' -> sb.append('\t')
                            '"' -> sb.append('"')
                            '\\' -> sb.append('\\')
                            else -> sb.append(c)
                        }
                        escaped = false
                    }
                    c == '\\' -> escaped = true
                    c == '"' -> return Pair(sb.toString(), i + 1)
                    else -> sb.append(c)
                }
                i++
            }
            Pair(sb.toString(), i)
        }
        json[i] == '{' -> {
            var depth = 1
            val objStart = i++
            while (i < json.length && depth > 0) {
                if (json[i] == '{') depth++
                else if (json[i] == '}') depth--
                i++
            }
            Pair(parseJson(json.substring(objStart, i)), i)
        }
        json[i] == '[' -> {
            val list = mutableListOf<Any>()
            i++
            while (i < json.length) {
                while (i < json.length && json[i].isWhitespace()) i++
                if (json[i] == ']') {
                    i++
                    break
                }
                val (item, endIdx) = parseJsonValue(json, i)
                list.add(item)
                i = endIdx
                while (i < json.length && (json[i].isWhitespace() || json[i] == ',')) i++
            }
            Pair(list, i)
        }
        json[i].isDigit() || json[i] == '-' -> {
            val numStart = i
            while (i < json.length && (json[i].isDigit() || json[i] == '.' || json[i] == '-')) i++
            val num = json.substring(numStart, i)
            Pair(if (num.contains(".")) num.toDouble() else num.toInt(), i)
        }
        json.startsWith("true", i) -> Pair(true, i + 4)
        json.startsWith("false", i) -> Pair(false, i + 5)
        json.startsWith("null", i) -> Pair("null", i + 4)
        else -> Pair("null", i)
    }
}

fun parseArgs(args: Array<String>): Args {
    val result = Args()
    var i = 0
    while (i < args.size) {
        when (args[i]) {
            "session" -> result.command = "session"
            "service" -> result.command = "service"
            "key" -> result.command = "key"
            "-k", "--api-key" -> result.apiKey = args[++i]
            "-n", "--network" -> result.network = args[++i]
            "-v", "--vcpu" -> result.vcpu = args[++i].toInt()
            "-e", "--env" -> result.env.add(args[++i])
            "-f", "--files" -> result.files.add(args[++i])
            "-a", "--artifacts" -> result.artifacts = true
            "-o", "--output-dir" -> result.outputDir = args[++i]
            "-l", "--list" -> {
                when (result.command) {
                    "session" -> result.sessionList = true
                    "service" -> result.serviceList = true
                }
            }
            "-s", "--shell" -> result.sessionShell = args[++i]
            "--kill" -> result.sessionKill = args[++i]
            "--name" -> result.serviceName = args[++i]
            "--ports" -> result.servicePorts = args[++i]
            "--type" -> result.serviceType = args[++i]
            "--bootstrap" -> result.serviceBootstrap = args[++i]
            "--info" -> result.serviceInfo = args[++i]
            "--logs" -> result.serviceLogs = args[++i]
            "--tail" -> result.serviceTail = args[++i]
            "--freeze" -> result.serviceSleep = args[++i]
            "--unfreeze" -> result.serviceWake = args[++i]
            "--destroy" -> result.serviceDestroy = args[++i]
            "--execute" -> result.serviceExecute = args[++i]
            "--command" -> result.serviceCommand = args[++i]
            "--dump-bootstrap" -> result.serviceDumpBootstrap = args[++i]
            "--dump-file" -> result.serviceDumpFile = args[++i]
            "--extend" -> result.keyExtend = true
            else -> if (!args[i].startsWith("-")) result.sourceFile = args[i]
        }
        i++
    }
    return result
}

fun printHelp() {
    println("""
Usage: kotlin UnKt [options] <source_file>
       kotlin UnKt session [options]
       kotlin UnKt service [options]
       kotlin UnKt key [options]

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
    """.trimIndent())
}
