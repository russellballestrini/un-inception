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
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

val API_BASE = "https://api.unsandbox.com"
val PORTAL_BASE = "https://unsandbox.com"
val LANGUAGES_CACHE_TTL = 3600L  // 1 hour in seconds
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
    var serviceBootstrapFile: String? = null,
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
    var serviceResize: String? = null,
    var serviceUnfreezeOnDemand: String? = null,
    var serviceUnfreezeOnDemandValue: Boolean? = null,
    var serviceShowFreezePage: String? = null,
    var serviceshowFreezePageValue: Boolean? = null,
    var keyExtend: Boolean = false,
    var envFile: String? = null,
    var envAction: String? = null,
    var envTarget: String? = null,
    var jsonOutput: Boolean = false,
    var imageList: Boolean = false,
    var imageInfo: String? = null,
    var imageDelete: String? = null,
    var imageLock: String? = null,
    var imageUnlock: String? = null,
    var imagePublish: String? = null,
    var imageSourceType: String? = null,
    var imageVisibility: String? = null,
    var imageVisibilityMode: String? = null,
    var imageSpawn: String? = null,
    var imageClone: String? = null,
    var imageName: String? = null,
    var imagePorts: String? = null
)

fun main(args: Array<String>) {
    try {
        val parsedArgs = parseArgs(args)

        when (parsedArgs.command) {
            "session" -> cmdSession(parsedArgs)
            "service" -> cmdService(parsedArgs)
            "languages" -> cmdLanguages(parsedArgs)
            "key" -> cmdKey(parsedArgs)
            "image" -> cmdImage(parsedArgs)
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
    val (publicKey, secretKey) = getApiKeys(args.apiKey)
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

    val result = apiRequest("/execute", "POST", payload, publicKey, secretKey)

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
    val (publicKey, secretKey) = getApiKeys(args.apiKey)

    if (args.sessionList) {
        val result = apiRequest("/sessions", "GET", null, publicKey, secretKey)
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
        apiRequest("/sessions/${args.sessionKill}", "DELETE", null, publicKey, secretKey)
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

    // Add input files
    if (args.files.isNotEmpty()) {
        val inputFiles = args.files.map { filepath ->
            val file = java.io.File(filepath)
            if (!file.exists()) {
                System.err.println("${RED}Error: Input file not found: $filepath${RESET}")
                exitProcess(1)
            }
            mapOf(
                "filename" to file.name,
                "content_base64" to java.util.Base64.getEncoder().encodeToString(file.readBytes())
            )
        }
        payload["input_files"] = inputFiles
    }

    println("${YELLOW}Creating session...${RESET}")
    val result = apiRequest("/sessions", "POST", payload, publicKey, secretKey)
    println("${GREEN}Session created: ${result["id"] ?: "N/A"}${RESET}")
    println("${YELLOW}(Interactive sessions require WebSocket - use un2 for full support)${RESET}")
}

fun cmdService(args: Args) {
    val (publicKey, secretKey) = getApiKeys(args.apiKey)

    // Handle env subcommand
    if (args.envAction != null) {
        cmdServiceEnv(args)
        return
    }

    if (args.serviceList) {
        val result = apiRequest("/services", "GET", null, publicKey, secretKey)
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
        val result = apiRequest("/services/${args.serviceInfo}", "GET", null, publicKey, secretKey)
        println(toJson(result))
        return
    }

    if (args.serviceLogs != null) {
        val result = apiRequest("/services/${args.serviceLogs}/logs", "GET", null, publicKey, secretKey)
        println(result["logs"] ?: "")
        return
    }

    if (args.serviceTail != null) {
        val result = apiRequest("/services/${args.serviceTail}/logs?lines=9000", "GET", null, publicKey, secretKey)
        println(result["logs"] ?: "")
        return
    }

    if (args.serviceSleep != null) {
        apiRequest("/services/${args.serviceSleep}/freeze", "POST", null, publicKey, secretKey)
        println("${GREEN}Service frozen: ${args.serviceSleep}${RESET}")
        return
    }

    if (args.serviceWake != null) {
        apiRequest("/services/${args.serviceWake}/unfreeze", "POST", null, publicKey, secretKey)
        println("${GREEN}Service unfreezing: ${args.serviceWake}${RESET}")
        return
    }

    if (args.serviceDestroy != null) {
        apiRequestDestructive("/services/${args.serviceDestroy}", "DELETE", null, publicKey, secretKey)
        println("${GREEN}Service destroyed: ${args.serviceDestroy}${RESET}")
        return
    }

    if (args.serviceResize != null) {
        if (args.vcpu <= 0) {
            System.err.println("${RED}Error: --resize requires --vcpu N (1-8)${RESET}")
            exitProcess(1)
        }
        val payload = mapOf("vcpu" to args.vcpu)
        apiRequestPatch("/services/${args.serviceResize}", payload, publicKey, secretKey)
        val ram = args.vcpu * 2
        println("${GREEN}Service resized to ${args.vcpu} vCPU, ${ram} GB RAM${RESET}")
        return
    }

    if (args.serviceUnfreezeOnDemand != null) {
        if (args.serviceUnfreezeOnDemandValue == null) {
            System.err.println("${RED}Error: --unfreeze-on-demand requires true or false${RESET}")
            exitProcess(1)
        }
        val payload = mapOf("unfreeze_on_demand" to args.serviceUnfreezeOnDemandValue!!)
        apiRequestPatch("/services/${args.serviceUnfreezeOnDemand}", payload, publicKey, secretKey)
        println("${GREEN}Service unfreeze_on_demand set to ${args.serviceUnfreezeOnDemandValue}: ${args.serviceUnfreezeOnDemand}${RESET}")
        return
    }

    if (args.serviceShowFreezePage != null) {
        if (args.serviceshowFreezePageValue == null) {
            System.err.println("${RED}Error: --show-freeze-page requires true or false${RESET}")
            exitProcess(1)
        }
        val payload = mapOf("show_freeze_page" to args.serviceshowFreezePageValue!!)
        apiRequestPatch("/services/${args.serviceShowFreezePage}", payload, publicKey, secretKey)
        println("${GREEN}Service show_freeze_page set to ${args.serviceshowFreezePageValue}: ${args.serviceShowFreezePage}${RESET}")
        return
    }

    if (args.serviceExecute != null) {
        val payload = mutableMapOf<String, Any>("command" to args.serviceCommand!!)
        val result = apiRequest("/services/${args.serviceExecute}/execute", "POST", payload, publicKey, secretKey)
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
        val result = apiRequest("/services/${args.serviceDumpBootstrap}/execute", "POST", payload, publicKey, secretKey)

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
        if (args.serviceBootstrapFile != null) {
            val file = File(args.serviceBootstrapFile!!)
            if (file.exists()) {
                payload["bootstrap_content"] = file.readText()
            } else {
                System.err.println("${RED}Error: Bootstrap file not found: ${args.serviceBootstrapFile}${RESET}")
                exitProcess(1)
            }
        }
        // Add input files
        if (args.files.isNotEmpty()) {
            val inputFiles = args.files.map { filepath ->
                val file = java.io.File(filepath)
                if (!file.exists()) {
                    System.err.println("${RED}Error: Input file not found: $filepath${RESET}")
                    exitProcess(1)
                }
                mapOf(
                    "filename" to file.name,
                    "content_base64" to java.util.Base64.getEncoder().encodeToString(file.readBytes())
                )
            }
            payload["input_files"] = inputFiles
        }
        if (args.network != null) {
            payload["network"] = args.network!!
        }
        if (args.vcpu > 0) {
            payload["vcpu"] = args.vcpu
        }

        val result = apiRequest("/services", "POST", payload, publicKey, secretKey)
        val serviceId = result["id"] as? String
        println("${GREEN}Service created: ${serviceId ?: "N/A"}${RESET}")
        println("Name: ${result["name"] ?: "N/A"}")
        if (result.containsKey("url")) {
            println("URL: ${result["url"]}")
        }

        // Auto-set vault if env vars were provided
        if (serviceId != null && (args.env.isNotEmpty() || args.envFile != null)) {
            val envContent = buildEnvContent(args.env, args.envFile)
            if (envContent.isNotEmpty()) {
                if (serviceEnvSet(serviceId, envContent, publicKey, secretKey)) {
                    println("${GREEN}Vault configured with environment variables${RESET}")
                } else {
                    println("${YELLOW}Warning: Failed to set vault${RESET}")
                }
            }
        }
        return
    }

    System.err.println("${RED}Error: Specify --name to create a service, or use --list, --info, etc.${RESET}")
    exitProcess(1)
}

fun getLanguagesCachePath(): String {
    val home = System.getenv("HOME") ?: System.getProperty("user.home") ?: "."
    return "$home/.unsandbox/languages.json"
}

fun loadLanguagesCache(): List<String>? {
    try {
        val cacheFile = File(getLanguagesCachePath())
        if (!cacheFile.exists()) return null

        // Check if cache is fresh (< 1 hour old)
        val ageSeconds = (System.currentTimeMillis() - cacheFile.lastModified()) / 1000
        if (ageSeconds >= LANGUAGES_CACHE_TTL) return null

        val content = cacheFile.readText()
        val parsed = parseJson(content)
        @Suppress("UNCHECKED_CAST")
        return parsed["languages"] as? List<String>
    } catch (e: Exception) {
        return null
    }
}

fun saveLanguagesCache(languages: List<String>) {
    try {
        val cachePath = getLanguagesCachePath()
        val cacheDir = File(cachePath).parentFile
        if (!cacheDir.exists()) {
            cacheDir.mkdirs()
        }
        val timestamp = System.currentTimeMillis() / 1000
        val data = mapOf("languages" to languages, "timestamp" to timestamp)
        File(cachePath).writeText(toJson(data))
    } catch (e: Exception) {
        // Cache failures are non-fatal
    }
}

fun cmdLanguages(args: Args) {
    val (publicKey, secretKey) = getApiKeys(args.apiKey)

    // Try cache first
    var languages = loadLanguagesCache()
    if (languages == null) {
        val result = apiRequest("/languages", "GET", null, publicKey, secretKey)
        @Suppress("UNCHECKED_CAST")
        languages = result["languages"] as? List<String> ?: emptyList()
        // Save to cache
        saveLanguagesCache(languages)
    }

    if (args.jsonOutput) {
        println(toJson(languages))
    } else {
        for (lang in languages) {
            println(lang)
        }
    }
}

fun cmdImage(args: Args) {
    val (publicKey, secretKey) = getApiKeys(args.apiKey)

    if (args.imageList) {
        val result = apiRequest("/images", "GET", null, publicKey, secretKey)
        println(toJson(result))
        return
    }

    if (args.imageInfo != null) {
        val result = apiRequest("/images/${args.imageInfo}", "GET", null, publicKey, secretKey)
        println(toJson(result))
        return
    }

    if (args.imageDelete != null) {
        apiRequestDestructive("/images/${args.imageDelete}", "DELETE", null, publicKey, secretKey)
        println("${GREEN}Image deleted: ${args.imageDelete}${RESET}")
        return
    }

    if (args.imageLock != null) {
        apiRequest("/images/${args.imageLock}/lock", "POST", null, publicKey, secretKey)
        println("${GREEN}Image locked: ${args.imageLock}${RESET}")
        return
    }

    if (args.imageUnlock != null) {
        apiRequestDestructive("/images/${args.imageUnlock}/unlock", "POST", null, publicKey, secretKey)
        println("${GREEN}Image unlocked: ${args.imageUnlock}${RESET}")
        return
    }

    if (args.imagePublish != null) {
        if (args.imageSourceType == null) {
            System.err.println("${RED}Error: --source-type required (service or snapshot)${RESET}")
            exitProcess(1)
        }
        val payload = mutableMapOf<String, Any>(
            "source_type" to args.imageSourceType!!,
            "source_id" to args.imagePublish!!
        )
        if (args.imageName != null) {
            payload["name"] = args.imageName!!
        }
        val result = apiRequest("/images/publish", "POST", payload, publicKey, secretKey)
        println("${GREEN}Image published${RESET}")
        println(toJson(result))
        return
    }

    if (args.imageVisibility != null) {
        if (args.imageVisibilityMode == null) {
            System.err.println("${RED}Error: --visibility requires MODE (private, unlisted, or public)${RESET}")
            exitProcess(1)
        }
        val payload = mapOf("visibility" to args.imageVisibilityMode!!)
        apiRequest("/images/${args.imageVisibility}/visibility", "POST", payload, publicKey, secretKey)
        println("${GREEN}Image visibility set to ${args.imageVisibilityMode}: ${args.imageVisibility}${RESET}")
        return
    }

    if (args.imageSpawn != null) {
        val payload = mutableMapOf<String, Any>()
        if (args.imageName != null) {
            payload["name"] = args.imageName!!
        }
        if (args.imagePorts != null) {
            payload["ports"] = args.imagePorts!!.split(",").map { it.trim().toInt() }
        }
        val result = apiRequest("/images/${args.imageSpawn}/spawn", "POST", payload, publicKey, secretKey)
        println("${GREEN}Service spawned from image${RESET}")
        println(toJson(result))
        return
    }

    if (args.imageClone != null) {
        val payload = mutableMapOf<String, Any>()
        if (args.imageName != null) {
            payload["name"] = args.imageName!!
        }
        val result = apiRequest("/images/${args.imageClone}/clone", "POST", payload, publicKey, secretKey)
        println("${GREEN}Image cloned${RESET}")
        println(toJson(result))
        return
    }

    System.err.println("${RED}Error: Use --list, --info ID, --delete ID, --lock ID, --unlock ID, --publish ID, --visibility ID MODE, --spawn ID, or --clone ID${RESET}")
    exitProcess(1)
}

fun cmdKey(args: Args) {
    val (publicKey, secretKey) = getApiKeys(args.apiKey)

    val result = validateKey(publicKey, secretKey)
    val valid = result["valid"] as? Boolean ?: false
    val expired = result["expired"] as? Boolean ?: false
    val pubKey = result["public_key"] as? String ?: ""
    val tier = result["tier"] as? String ?: ""
    val expiresAt = result["expires_at"] as? String ?: ""

    if (args.keyExtend) {
        if (pubKey.isEmpty()) {
            System.err.println("${RED}Error: Could not retrieve public key${RESET}")
            exitProcess(1)
        }
        val extendUrl = "$PORTAL_BASE/keys/extend?pk=$pubKey"
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
        println("Public Key: $pubKey")
        println("Tier: $tier")
        println("Expired: $expiresAt")
        println("${YELLOW}To renew: Visit $PORTAL_BASE/keys/extend${RESET}")
    } else if (valid) {
        println("${GREEN}Status: Valid${RESET}")
        println("Public Key: $pubKey")
        println("Tier: $tier")
        println("Expires: $expiresAt")
    } else {
        println("${RED}Status: Invalid${RESET}")
        exitProcess(1)
    }
}

fun validateKey(publicKey: String?, secretKey: String): Map<String, Any> {
    val timestamp = System.currentTimeMillis() / 1000
    val method = "POST"
    val path = "/keys/validate"
    val body = ""
    val signatureData = "$timestamp:$method:$path:$body"
    val signature = hmacSha256(secretKey, signatureData)

    val url = URL("$PORTAL_BASE$path")
    val connection = url.openConnection() as HttpURLConnection

    connection.requestMethod = method
    connection.setRequestProperty("Authorization", "Bearer ${publicKey ?: secretKey}")
    connection.setRequestProperty("X-Timestamp", timestamp.toString())
    connection.setRequestProperty("X-Signature", signature)
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

fun getApiKeys(argsKey: String?): Pair<String?, String> {
    var publicKey: String? = null
    var secretKey: String? = null

    if (argsKey != null) {
        secretKey = argsKey
        publicKey = System.getenv("UNSANDBOX_PUBLIC_KEY")
    } else {
        publicKey = System.getenv("UNSANDBOX_PUBLIC_KEY")
        secretKey = System.getenv("UNSANDBOX_SECRET_KEY")

        if (publicKey == null || secretKey == null) {
            val apiKey = System.getenv("UNSANDBOX_API_KEY")
            if (apiKey != null && apiKey.isNotEmpty()) {
                secretKey = apiKey
            }
        }
    }

    if (secretKey.isNullOrEmpty()) {
        System.err.println("${RED}Error: UNSANDBOX_SECRET_KEY or UNSANDBOX_API_KEY not set${RESET}")
        exitProcess(1)
    }

    return Pair(publicKey, secretKey)
}

fun hmacSha256(secretKey: String, data: String): String {
    val mac = Mac.getInstance("HmacSHA256")
    val keySpec = SecretKeySpec(secretKey.toByteArray(Charsets.UTF_8), "HmacSHA256")
    mac.init(keySpec)
    val hash = mac.doFinal(data.toByteArray(Charsets.UTF_8))
    return hash.joinToString("") { "%02x".format(it) }
}

fun detectLanguage(filename: String): String {
    val ext = filename.substringAfterLast('.', "")
    if (ext.isEmpty()) {
        throw RuntimeException("Cannot detect language: no file extension")
    }
    return EXT_MAP[".$ext"] ?: throw RuntimeException("Unsupported file extension: .$ext")
}

// Exception for 428 Sudo Challenge
class SudoChallengeException(val challengeId: String?, val responseBody: String) : RuntimeException("Sudo challenge required")

fun apiRequest(endpoint: String, method: String, data: Map<String, Any>?, publicKey: String?, secretKey: String): Map<String, Any> {
    val timestamp = System.currentTimeMillis() / 1000
    val body = if (data != null) toJson(data) else ""
    val signatureData = "$timestamp:$method:$endpoint:$body"
    val signature = hmacSha256(secretKey, signatureData)

    val url = URL(API_BASE + endpoint)
    val connection = url.openConnection() as HttpURLConnection

    connection.requestMethod = method
    connection.setRequestProperty("Authorization", "Bearer ${publicKey ?: secretKey}")
    connection.setRequestProperty("X-Timestamp", timestamp.toString())
    connection.setRequestProperty("X-Signature", signature)
    connection.setRequestProperty("Content-Type", "application/json")
    connection.connectTimeout = 30000
    connection.readTimeout = 300000

    if (data != null) {
        connection.doOutput = true
        connection.outputStream.use { it.write(body.toByteArray()) }
    }

    if (connection.responseCode !in 200..299) {
        val error = connection.errorStream?.bufferedReader()?.readText() ?: ""
        if (connection.responseCode == 428) {
            // Extract challenge_id from response
            var challengeId: String? = null
            try {
                val errorJson = parseJson(error)
                challengeId = errorJson["challenge_id"]?.toString()
            } catch (e: Exception) {
                // Ignore parse errors
            }
            throw SudoChallengeException(challengeId, error)
        }
        if (connection.responseCode == 401 && error.lowercase().contains("timestamp")) {
            System.err.println("${RED}Error: Request timestamp expired (must be within 5 minutes of server time)${RESET}")
            System.err.println("${YELLOW}Your computer's clock may have drifted.${RESET}")
            System.err.println("Check your system time and sync with NTP if needed:")
            System.err.println("  Linux:   sudo ntpdate -s time.nist.gov")
            System.err.println("  macOS:   sudo sntp -sS time.apple.com")
            System.err.println("  Windows: w32tm /resync")
            exitProcess(1)
        }
        throw RuntimeException("HTTP ${connection.responseCode} - $error")
    }

    val response = connection.inputStream.bufferedReader().readText()
    return parseJson(response)
}

fun apiRequestWithSudo(endpoint: String, method: String, data: Map<String, Any>?, publicKey: String?, secretKey: String, otp: String, challengeId: String?): Map<String, Any> {
    val timestamp = System.currentTimeMillis() / 1000
    val body = if (data != null) toJson(data) else ""
    val signatureData = "$timestamp:$method:$endpoint:$body"
    val signature = hmacSha256(secretKey, signatureData)

    val url = URL(API_BASE + endpoint)
    val connection = url.openConnection() as HttpURLConnection

    connection.requestMethod = method
    connection.setRequestProperty("Authorization", "Bearer ${publicKey ?: secretKey}")
    connection.setRequestProperty("X-Timestamp", timestamp.toString())
    connection.setRequestProperty("X-Signature", signature)
    connection.setRequestProperty("Content-Type", "application/json")
    connection.setRequestProperty("X-Sudo-OTP", otp)
    if (challengeId != null) {
        connection.setRequestProperty("X-Sudo-Challenge", challengeId)
    }
    connection.connectTimeout = 30000
    connection.readTimeout = 300000

    if (data != null) {
        connection.doOutput = true
        connection.outputStream.use { it.write(body.toByteArray()) }
    }

    if (connection.responseCode !in 200..299) {
        val error = connection.errorStream?.bufferedReader()?.readText() ?: ""
        throw RuntimeException("HTTP ${connection.responseCode} - $error")
    }

    val response = connection.inputStream.bufferedReader().readText()
    return parseJson(response)
}

fun handleSudoChallenge(challengeId: String?, method: String, endpoint: String, data: Map<String, Any>?, publicKey: String?, secretKey: String): Map<String, Any> {
    System.err.println("${YELLOW}Confirmation required. Check your email for a one-time code.${RESET}")
    System.err.print("Enter OTP: ")
    System.err.flush()

    val reader = java.io.BufferedReader(java.io.InputStreamReader(System.`in`))
    val otp = reader.readLine()?.trim()

    if (otp.isNullOrEmpty()) {
        throw RuntimeException("Operation cancelled - no OTP provided")
    }

    return apiRequestWithSudo(endpoint, method, data, publicKey, secretKey, otp, challengeId)
}

fun apiRequestDestructive(endpoint: String, method: String, data: Map<String, Any>?, publicKey: String?, secretKey: String): Map<String, Any> {
    return try {
        apiRequest(endpoint, method, data, publicKey, secretKey)
    } catch (e: SudoChallengeException) {
        handleSudoChallenge(e.challengeId, method, endpoint, data, publicKey, secretKey)
    }
}

fun apiRequestPatch(endpoint: String, data: Map<String, Any>, publicKey: String?, secretKey: String): Map<String, Any> {
    val timestamp = System.currentTimeMillis() / 1000
    val body = toJson(data)
    val signatureData = "$timestamp:PATCH:$endpoint:$body"
    val signature = hmacSha256(secretKey, signatureData)

    val url = URL(API_BASE + endpoint)
    val connection = url.openConnection() as HttpURLConnection

    connection.requestMethod = "PATCH"
    connection.setRequestProperty("Authorization", "Bearer ${publicKey ?: secretKey}")
    connection.setRequestProperty("X-Timestamp", timestamp.toString())
    connection.setRequestProperty("X-Signature", signature)
    connection.setRequestProperty("Content-Type", "application/json")
    connection.connectTimeout = 30000
    connection.readTimeout = 300000

    connection.doOutput = true
    connection.outputStream.use { it.write(body.toByteArray()) }

    if (connection.responseCode !in 200..299) {
        val error = connection.errorStream?.bufferedReader()?.readText() ?: ""
        if (connection.responseCode == 401 && error.lowercase().contains("timestamp")) {
            System.err.println("${RED}Error: Request timestamp expired (must be within 5 minutes of server time)${RESET}")
            System.err.println("${YELLOW}Your computer's clock may have drifted.${RESET}")
            System.err.println("Check your system time and sync with NTP if needed:")
            System.err.println("  Linux:   sudo ntpdate -s time.nist.gov")
            System.err.println("  macOS:   sudo sntp -sS time.apple.com")
            System.err.println("  Windows: w32tm /resync")
            exitProcess(1)
        }
        throw RuntimeException("HTTP ${connection.responseCode} - $error")
    }

    val response = connection.inputStream.bufferedReader().readText()
    return parseJson(response)
}

fun apiRequestText(endpoint: String, method: String, body: String, publicKey: String?, secretKey: String): Pair<Boolean, String> {
    val timestamp = System.currentTimeMillis() / 1000
    val signatureData = "$timestamp:$method:$endpoint:$body"
    val signature = hmacSha256(secretKey, signatureData)

    val url = URL(API_BASE + endpoint)
    val connection = url.openConnection() as HttpURLConnection

    connection.requestMethod = method
    connection.setRequestProperty("Authorization", "Bearer ${publicKey ?: secretKey}")
    connection.setRequestProperty("X-Timestamp", timestamp.toString())
    connection.setRequestProperty("X-Signature", signature)
    connection.setRequestProperty("Content-Type", "text/plain")
    connection.connectTimeout = 30000
    connection.readTimeout = 300000

    connection.doOutput = true
    connection.outputStream.use { it.write(body.toByteArray()) }

    return if (connection.responseCode in 200..299) {
        Pair(true, connection.inputStream.bufferedReader().readText())
    } else {
        Pair(false, connection.errorStream?.bufferedReader()?.readText() ?: "")
    }
}

const val MAX_ENV_CONTENT_SIZE = 65536

fun readEnvFile(path: String): String {
    val file = File(path)
    if (!file.exists()) {
        System.err.println("${RED}Error: Env file not found: $path${RESET}")
        exitProcess(1)
    }
    return file.readText()
}

fun buildEnvContent(envs: List<String>, envFile: String?): String {
    val lines = mutableListOf<String>()
    lines.addAll(envs)
    if (envFile != null) {
        val content = readEnvFile(envFile)
        for (line in content.lines()) {
            val trimmed = line.trim()
            if (trimmed.isNotEmpty() && !trimmed.startsWith("#")) {
                lines.add(trimmed)
            }
        }
    }
    return lines.joinToString("\n")
}

fun serviceEnvStatus(serviceId: String, publicKey: String?, secretKey: String): Map<String, Any> {
    return apiRequest("/services/$serviceId/env", "GET", null, publicKey, secretKey)
}

fun serviceEnvSet(serviceId: String, envContent: String, publicKey: String?, secretKey: String): Boolean {
    if (envContent.length > MAX_ENV_CONTENT_SIZE) {
        System.err.println("${RED}Error: Env content exceeds maximum size of 64KB${RESET}")
        return false
    }
    val (success, _) = apiRequestText("/services/$serviceId/env", "PUT", envContent, publicKey, secretKey)
    return success
}

fun serviceEnvExport(serviceId: String, publicKey: String?, secretKey: String): Map<String, Any> {
    return apiRequest("/services/$serviceId/env/export", "POST", emptyMap(), publicKey, secretKey)
}

fun serviceEnvDelete(serviceId: String, publicKey: String?, secretKey: String): Boolean {
    return try {
        apiRequest("/services/$serviceId/env", "DELETE", null, publicKey, secretKey)
        true
    } catch (e: Exception) {
        false
    }
}

fun cmdServiceEnv(args: Args) {
    val (publicKey, secretKey) = getApiKeys(args.apiKey)
    val action = args.envAction
    val target = args.envTarget

    when (action) {
        "status" -> {
            if (target == null) {
                System.err.println("${RED}Error: service env status requires service ID${RESET}")
                exitProcess(1)
            }
            val result = serviceEnvStatus(target, publicKey, secretKey)
            val hasVault = result["has_vault"] as? Boolean ?: false
            if (hasVault) {
                println("${GREEN}Vault: configured${RESET}")
                val envCount = result["env_count"]
                if (envCount != null) println("Variables: $envCount")
                val updatedAt = result["updated_at"]
                if (updatedAt != null) println("Updated: $updatedAt")
            } else {
                println("${YELLOW}Vault: not configured${RESET}")
            }
        }
        "set" -> {
            if (target == null) {
                System.err.println("${RED}Error: service env set requires service ID${RESET}")
                exitProcess(1)
            }
            if (args.env.isEmpty() && args.envFile == null) {
                System.err.println("${RED}Error: service env set requires -e or --env-file${RESET}")
                exitProcess(1)
            }
            val envContent = buildEnvContent(args.env, args.envFile)
            if (serviceEnvSet(target, envContent, publicKey, secretKey)) {
                println("${GREEN}Vault updated for service $target${RESET}")
            } else {
                System.err.println("${RED}Error: Failed to update vault${RESET}")
                exitProcess(1)
            }
        }
        "export" -> {
            if (target == null) {
                System.err.println("${RED}Error: service env export requires service ID${RESET}")
                exitProcess(1)
            }
            val result = serviceEnvExport(target, publicKey, secretKey)
            val content = result["content"] as? String
            if (content != null) print(content)
        }
        "delete" -> {
            if (target == null) {
                System.err.println("${RED}Error: service env delete requires service ID${RESET}")
                exitProcess(1)
            }
            if (serviceEnvDelete(target, publicKey, secretKey)) {
                println("${GREEN}Vault deleted for service $target${RESET}")
            } else {
                System.err.println("${RED}Error: Failed to delete vault${RESET}")
                exitProcess(1)
            }
        }
        else -> {
            System.err.println("${RED}Error: Unknown env action: $action${RESET}")
            System.err.println("Usage: kotlin UnKt service env <status|set|export|delete> <service_id>")
            exitProcess(1)
        }
    }
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
            "languages" -> result.command = "languages"
            "key" -> result.command = "key"
            "image" -> result.command = "image"
            "--json" -> result.jsonOutput = true
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
                    "image" -> result.imageList = true
                }
            }
            "-s", "--shell" -> result.sessionShell = args[++i]
            "--kill" -> result.sessionKill = args[++i]
            "--name" -> {
                when (result.command) {
                    "image" -> result.imageName = args[++i]
                    else -> result.serviceName = args[++i]
                }
            }
            "--ports" -> {
                when (result.command) {
                    "image" -> result.imagePorts = args[++i]
                    else -> result.servicePorts = args[++i]
                }
            }
            "--type" -> result.serviceType = args[++i]
            "--bootstrap" -> result.serviceBootstrap = args[++i]
            "--bootstrap-file" -> result.serviceBootstrapFile = args[++i]
            "--logs" -> result.serviceLogs = args[++i]
            "--tail" -> result.serviceTail = args[++i]
            "--freeze" -> result.serviceSleep = args[++i]
            "--unfreeze" -> result.serviceWake = args[++i]
            "--destroy" -> result.serviceDestroy = args[++i]
            "--resize" -> result.serviceResize = args[++i]
            "--unfreeze-on-demand" -> {
                result.serviceUnfreezeOnDemand = args[++i]
                if (i + 1 < args.size && (args[i + 1] == "true" || args[i + 1] == "false")) {
                    result.serviceUnfreezeOnDemandValue = args[++i].toBoolean()
                }
            }
            "--show-freeze-page" -> {
                result.serviceShowFreezePage = args[++i]
                if (i + 1 < args.size && (args[i + 1] == "true" || args[i + 1] == "false")) {
                    result.serviceshowFreezePageValue = args[++i].toBoolean()
                }
            }
            "--execute" -> result.serviceExecute = args[++i]
            "--command" -> result.serviceCommand = args[++i]
            "--dump-bootstrap" -> result.serviceDumpBootstrap = args[++i]
            "--dump-file" -> result.serviceDumpFile = args[++i]
            "--extend" -> result.keyExtend = true
            "--env-file" -> result.envFile = args[++i]
            "--info" -> {
                when (result.command) {
                    "service" -> result.serviceInfo = args[++i]
                    "image" -> result.imageInfo = args[++i]
                }
            }
            "--delete" -> {
                if (result.command == "image") result.imageDelete = args[++i]
            }
            "--lock" -> {
                if (result.command == "image") result.imageLock = args[++i]
            }
            "--unlock" -> {
                if (result.command == "image") result.imageUnlock = args[++i]
            }
            "--publish" -> {
                if (result.command == "image") result.imagePublish = args[++i]
            }
            "--source-type" -> result.imageSourceType = args[++i]
            "--visibility" -> {
                if (result.command == "image") {
                    result.imageVisibility = args[++i]
                    if (i + 1 < args.size && !args[i + 1].startsWith("-")) {
                        result.imageVisibilityMode = args[++i]
                    }
                }
            }
            "--spawn" -> {
                if (result.command == "image") result.imageSpawn = args[++i]
            }
            "--clone" -> {
                if (result.command == "image") result.imageClone = args[++i]
            }
            "env" -> {
                if (result.command == "service" && i + 1 < args.size) {
                    result.envAction = args[++i]
                    if (i + 1 < args.size && !args[i + 1].startsWith("-")) {
                        result.envTarget = args[++i]
                    }
                }
            }
            else -> {
                if (args[i].startsWith("-")) {
                    System.err.println("${RED}Unknown option: ${args[i]}${RESET}")
                    kotlin.system.exitProcess(1)
                } else {
                    result.sourceFile = args[i]
                }
            }
        }
        i++
    }
    return result
}

// ============================================================================
// Library Functions (for programmatic use)
// ============================================================================

/**
 * SDK version string.
 */
fun version(): String = "4.2.0"

/**
 * Check API health status.
 */
fun healthCheck(): Boolean {
    return try {
        val url = URL("$API_BASE/health")
        val connection = url.openConnection() as HttpURLConnection
        connection.requestMethod = "GET"
        connection.connectTimeout = 5000
        connection.readTimeout = 5000
        connection.responseCode == 200
    } catch (e: Exception) {
        false
    }
}

/**
 * Generate HMAC-SHA256 signature.
 */
fun hmacSign(secretKey: String, message: String): String {
    val mac = Mac.getInstance("HmacSHA256")
    val keySpec = SecretKeySpec(secretKey.toByteArray(Charsets.UTF_8), "HmacSHA256")
    mac.init(keySpec)
    return mac.doFinal(message.toByteArray(Charsets.UTF_8)).joinToString("") { "%02x".format(it) }
}

/**
 * Execute code synchronously.
 */
fun execute(language: String, code: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) {
        Pair(publicKey, secretKey)
    } else {
        getApiKeys(null)
    }
    val payload = mapOf("language" to language, "code" to code)
    return apiRequest("/execute", "POST", payload, pk, sk)
}

/**
 * Execute code asynchronously, returns job ID.
 */
fun executeAsync(language: String, code: String, publicKey: String? = null, secretKey: String? = null): String? {
    val result = execute(language, code, publicKey, secretKey)
    return result["job_id"]?.toString()
}

/**
 * Get job status and result.
 */
fun getJob(jobId: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) {
        Pair(publicKey, secretKey)
    } else {
        getApiKeys(null)
    }
    return apiRequest("/jobs/$jobId", "GET", null, pk, sk)
}

/**
 * Wait for job completion with polling.
 */
fun waitJob(jobId: String, publicKey: String? = null, secretKey: String? = null, timeoutMs: Long = 0): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) {
        Pair(publicKey, secretKey)
    } else {
        getApiKeys(null)
    }
    val pollDelays = intArrayOf(300, 450, 700, 900, 650, 1600, 2000)
    val terminalStates = setOf("completed", "failed", "timeout", "cancelled")
    val startTime = System.currentTimeMillis()
    var pollCount = 0

    while (true) {
        val delayIdx = minOf(pollCount, pollDelays.size - 1)
        Thread.sleep(pollDelays[delayIdx].toLong())
        pollCount++

        if (timeoutMs > 0 && System.currentTimeMillis() - startTime > timeoutMs) {
            throw RuntimeException("Timeout waiting for job $jobId")
        }

        val result = getJob(jobId, pk, sk)
        val status = result["status"]?.toString() ?: ""
        if (status in terminalStates) {
            return result
        }
    }
}

/**
 * Cancel a running job.
 */
fun cancelJob(jobId: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) {
        Pair(publicKey, secretKey)
    } else {
        getApiKeys(null)
    }
    return apiRequest("/jobs/$jobId", "DELETE", null, pk, sk)
}

/**
 * List all jobs.
 */
fun listJobs(publicKey: String? = null, secretKey: String? = null): List<Map<String, Any>> {
    val (pk, sk) = if (publicKey != null && secretKey != null) {
        Pair(publicKey, secretKey)
    } else {
        getApiKeys(null)
    }
    val result = apiRequest("/jobs", "GET", null, pk, sk)
    @Suppress("UNCHECKED_CAST")
    return result["jobs"] as? List<Map<String, Any>> ?: emptyList()
}

/**
 * Get supported languages.
 */
fun getLanguages(publicKey: String? = null, secretKey: String? = null): List<String> {
    val cached = loadLanguagesCache()
    if (cached != null) return cached

    val (pk, sk) = if (publicKey != null && secretKey != null) {
        Pair(publicKey, secretKey)
    } else {
        getApiKeys(null)
    }
    val result = apiRequest("/languages", "GET", null, pk, sk)
    @Suppress("UNCHECKED_CAST")
    val languages = result["languages"] as? List<String> ?: emptyList()
    saveLanguagesCache(languages)
    return languages
}

// Session functions
fun sessionList(publicKey: String? = null, secretKey: String? = null): List<Map<String, Any>> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    val result = apiRequest("/sessions", "GET", null, pk, sk)
    @Suppress("UNCHECKED_CAST")
    return result["sessions"] as? List<Map<String, Any>> ?: emptyList()
}

fun sessionGet(sessionId: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequest("/sessions/$sessionId", "GET", null, pk, sk)
}

fun sessionCreate(networkMode: String? = null, shell: String? = null, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    val payload = mutableMapOf<String, Any>("shell" to (shell ?: "bash"))
    if (networkMode != null) payload["network_mode"] = networkMode
    return apiRequest("/sessions", "POST", payload, pk, sk)
}

fun sessionDestroy(sessionId: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequest("/sessions/$sessionId", "DELETE", null, pk, sk)
}

fun sessionFreeze(sessionId: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequest("/sessions/$sessionId/freeze", "POST", null, pk, sk)
}

fun sessionUnfreeze(sessionId: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequest("/sessions/$sessionId/unfreeze", "POST", null, pk, sk)
}

fun sessionBoost(sessionId: String, vcpu: Int = 2, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequest("/sessions/$sessionId/boost", "POST", mapOf("vcpu" to vcpu), pk, sk)
}

fun sessionUnboost(sessionId: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequest("/sessions/$sessionId/unboost", "POST", null, pk, sk)
}

fun sessionExecute(sessionId: String, command: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequest("/sessions/$sessionId/shell", "POST", mapOf("command" to command), pk, sk)
}

// Service functions
fun serviceList(publicKey: String? = null, secretKey: String? = null): List<Map<String, Any>> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    val result = apiRequest("/services", "GET", null, pk, sk)
    @Suppress("UNCHECKED_CAST")
    return result["services"] as? List<Map<String, Any>> ?: emptyList()
}

fun serviceGet(serviceId: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequest("/services/$serviceId", "GET", null, pk, sk)
}

fun serviceCreate(name: String, ports: String? = null, domains: String? = null, bootstrap: String? = null, networkMode: String? = null, publicKey: String? = null, secretKey: String? = null): String? {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    val payload = mutableMapOf<String, Any>("name" to name)
    if (ports != null) payload["ports"] = ports.split(",").map { it.trim().toInt() }
    if (domains != null) payload["domains"] = domains
    if (bootstrap != null) payload["bootstrap"] = bootstrap
    if (networkMode != null) payload["network_mode"] = networkMode
    val result = apiRequest("/services", "POST", payload, pk, sk)
    return result["id"]?.toString()
}

fun serviceDestroy(serviceId: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequestDestructive("/services/$serviceId", "DELETE", null, pk, sk)
}

fun serviceFreeze(serviceId: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequest("/services/$serviceId/freeze", "POST", null, pk, sk)
}

fun serviceUnfreeze(serviceId: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequest("/services/$serviceId/unfreeze", "POST", null, pk, sk)
}

fun serviceLock(serviceId: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequest("/services/$serviceId/lock", "POST", null, pk, sk)
}

fun serviceUnlock(serviceId: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequestDestructive("/services/$serviceId/unlock", "POST", null, pk, sk)
}

fun serviceSetUnfreezeOnDemand(serviceId: String, enabled: Boolean, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequestPatch("/services/$serviceId", mapOf("unfreeze_on_demand" to enabled), pk, sk)
}

fun serviceRedeploy(serviceId: String, bootstrap: String? = null, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    val payload = if (bootstrap != null) mapOf("bootstrap" to bootstrap) else emptyMap()
    return apiRequest("/services/$serviceId/redeploy", "POST", payload, pk, sk)
}

fun serviceLogs(serviceId: String, allLogs: Boolean = false, publicKey: String? = null, secretKey: String? = null): String? {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    val path = if (allLogs) "/services/$serviceId/logs?all=true" else "/services/$serviceId/logs"
    val result = apiRequest(path, "GET", null, pk, sk)
    return result["logs"]?.toString()
}

fun serviceExecute(serviceId: String, command: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequest("/services/$serviceId/execute", "POST", mapOf("command" to command), pk, sk)
}

fun serviceEnvGet(serviceId: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequest("/services/$serviceId/env", "GET", null, pk, sk)
}

fun serviceEnvSet(serviceId: String, envContent: String, publicKey: String? = null, secretKey: String? = null): Boolean {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    val (success, _) = apiRequestText("/services/$serviceId/env", "PUT", envContent, pk, sk)
    return success
}

fun serviceEnvDelete(serviceId: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequest("/services/$serviceId/env", "DELETE", null, pk, sk)
}

fun serviceEnvExport(serviceId: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequest("/services/$serviceId/env/export", "POST", emptyMap<String, Any>(), pk, sk)
}

fun serviceResize(serviceId: String, vcpu: Int, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequestPatch("/services/$serviceId", mapOf("vcpu" to vcpu), pk, sk)
}

// Snapshot functions
fun snapshotList(publicKey: String? = null, secretKey: String? = null): List<Map<String, Any>> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    val result = apiRequest("/snapshots", "GET", null, pk, sk)
    @Suppress("UNCHECKED_CAST")
    return result["snapshots"] as? List<Map<String, Any>> ?: emptyList()
}

fun snapshotGet(snapshotId: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequest("/snapshots/$snapshotId", "GET", null, pk, sk)
}

fun snapshotSession(sessionId: String, name: String? = null, hot: Boolean = false, publicKey: String? = null, secretKey: String? = null): String? {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    val payload = mutableMapOf<String, Any>("session_id" to sessionId, "hot" to hot)
    if (name != null) payload["name"] = name
    val result = apiRequest("/snapshots", "POST", payload, pk, sk)
    return result["snapshot_id"]?.toString()
}

fun snapshotService(serviceId: String, name: String? = null, hot: Boolean = false, publicKey: String? = null, secretKey: String? = null): String? {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    val payload = mutableMapOf<String, Any>("service_id" to serviceId, "hot" to hot)
    if (name != null) payload["name"] = name
    val result = apiRequest("/snapshots", "POST", payload, pk, sk)
    return result["snapshot_id"]?.toString()
}

fun snapshotRestore(snapshotId: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequest("/snapshots/$snapshotId/restore", "POST", emptyMap<String, Any>(), pk, sk)
}

fun snapshotDelete(snapshotId: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequestDestructive("/snapshots/$snapshotId", "DELETE", null, pk, sk)
}

fun snapshotLock(snapshotId: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequest("/snapshots/$snapshotId/lock", "POST", null, pk, sk)
}

fun snapshotUnlock(snapshotId: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequestDestructive("/snapshots/$snapshotId/unlock", "POST", null, pk, sk)
}

fun snapshotClone(snapshotId: String, cloneType: String, name: String? = null, ports: String? = null, shell: String? = null, publicKey: String? = null, secretKey: String? = null): String? {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    val payload = mutableMapOf<String, Any>("type" to cloneType)
    if (name != null) payload["name"] = name
    if (ports != null) payload["ports"] = ports.split(",").map { it.trim().toInt() }
    if (shell != null) payload["shell"] = shell
    val result = apiRequest("/snapshots/$snapshotId/clone", "POST", payload, pk, sk)
    return (result["session_id"] ?: result["service_id"])?.toString()
}

// Image functions
fun imageList(filter: String? = null, publicKey: String? = null, secretKey: String? = null): List<Map<String, Any>> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    val path = if (filter != null) "/images/$filter" else "/images"
    val result = apiRequest(path, "GET", null, pk, sk)
    @Suppress("UNCHECKED_CAST")
    return result["images"] as? List<Map<String, Any>> ?: emptyList()
}

fun imageGet(imageId: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequest("/images/$imageId", "GET", null, pk, sk)
}

fun imagePublish(sourceType: String, sourceId: String, name: String? = null, description: String? = null, publicKey: String? = null, secretKey: String? = null): String? {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    val payload = mutableMapOf<String, Any>("source_type" to sourceType, "source_id" to sourceId)
    if (name != null) payload["name"] = name
    if (description != null) payload["description"] = description
    val result = apiRequest("/images", "POST", payload, pk, sk)
    return result["image_id"]?.toString()
}

fun imageDelete(imageId: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequestDestructive("/images/$imageId", "DELETE", null, pk, sk)
}

fun imageLock(imageId: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequest("/images/$imageId/lock", "POST", null, pk, sk)
}

fun imageUnlock(imageId: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequestDestructive("/images/$imageId/unlock", "POST", null, pk, sk)
}

fun imageSetVisibility(imageId: String, visibility: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequest("/images/$imageId/visibility", "POST", mapOf("visibility" to visibility), pk, sk)
}

fun imageGrantAccess(imageId: String, trustedApiKey: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequest("/images/$imageId/grant", "POST", mapOf("trusted_api_key" to trustedApiKey), pk, sk)
}

fun imageRevokeAccess(imageId: String, trustedApiKey: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequest("/images/$imageId/revoke", "POST", mapOf("trusted_api_key" to trustedApiKey), pk, sk)
}

fun imageListTrusted(imageId: String, publicKey: String? = null, secretKey: String? = null): List<String> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    val result = apiRequest("/images/$imageId/trusted", "GET", null, pk, sk)
    @Suppress("UNCHECKED_CAST")
    return result["trusted"] as? List<String> ?: emptyList()
}

fun imageTransfer(imageId: String, toApiKey: String, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return apiRequest("/images/$imageId/transfer", "POST", mapOf("to_api_key" to toApiKey), pk, sk)
}

fun imageSpawn(imageId: String, name: String? = null, ports: String? = null, bootstrap: String? = null, networkMode: String? = null, publicKey: String? = null, secretKey: String? = null): String? {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    val payload = mutableMapOf<String, Any>()
    if (name != null) payload["name"] = name
    if (ports != null) payload["ports"] = ports.split(",").map { it.trim().toInt() }
    if (bootstrap != null) payload["bootstrap"] = bootstrap
    if (networkMode != null) payload["network_mode"] = networkMode
    val result = apiRequest("/images/$imageId/spawn", "POST", payload, pk, sk)
    return result["service_id"]?.toString()
}

fun imageClone(imageId: String, name: String? = null, description: String? = null, publicKey: String? = null, secretKey: String? = null): String? {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    val payload = mutableMapOf<String, Any>()
    if (name != null) payload["name"] = name
    if (description != null) payload["description"] = description
    val result = apiRequest("/images/$imageId/clone", "POST", payload, pk, sk)
    return result["image_id"]?.toString()
}

// PaaS Logs functions
fun logsFetch(source: String = "all", lines: Int = 100, since: String? = null, grep: String? = null, publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    val params = mutableListOf("source=$source", "lines=$lines")
    if (since != null) params.add("since=$since")
    if (grep != null) params.add("grep=${java.net.URLEncoder.encode(grep, "UTF-8")}")
    return apiRequest("/paas/logs?${params.joinToString("&")}", "GET", null, pk, sk)
}

// Validate keys
fun validateKeys(publicKey: String? = null, secretKey: String? = null): Map<String, Any> {
    val (pk, sk) = if (publicKey != null && secretKey != null) Pair(publicKey, secretKey) else getApiKeys(null)
    return validateKey(pk, sk)
}

fun printHelp() {
    println("""
Usage: kotlin UnKt [options] <source_file>
       kotlin UnKt session [options]
       kotlin UnKt service [options]
       kotlin UnKt image [options]
       kotlin UnKt languages [--json]
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
  -e KEY=VALUE      Environment variable for vault
  --env-file FILE   Load vault variables from file
  --info ID         Get service details
  --logs ID         Get all logs
  --tail ID         Get last 9000 lines
  --freeze ID       Freeze service
  --unfreeze ID     Unfreeze service
  --destroy ID      Destroy service
  --resize ID       Resize service (requires --vcpu N)
  --unfreeze-on-demand ID true|false  Enable/disable auto-unfreeze on HTTP request
  --show-freeze-page ID true|false  Enable/disable showing freeze page when frozen
  --execute ID      Execute command in service
  --command CMD     Command to execute (with --execute)
  --dump-bootstrap ID   Dump bootstrap script
  --dump-file FILE      File to save bootstrap (with --dump-bootstrap)

Service env commands:
  env status ID     Show vault status
  env set ID        Set vault (-e KEY=VALUE or --env-file FILE)
  env export ID     Export vault contents
  env delete ID     Delete vault

Key options:
  --extend          Open browser to extend key

Image options:
  --list            List all images
  --info ID         Get image details
  --delete ID       Delete an image
  --lock ID         Lock image to prevent deletion
  --unlock ID       Unlock image
  --publish ID      Publish image from service/snapshot (requires --source-type)
  --source-type TYPE  Source type: service or snapshot
  --visibility ID MODE  Set visibility: private, unlisted, or public
  --spawn ID        Spawn new service from image
  --clone ID        Clone an image
  --name NAME       Name for spawned service or cloned image
  --ports PORTS     Ports for spawned service

Languages options:
  --json            Output as JSON array
    """.trimIndent())
}
