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


// un.dart - Unsandbox CLI Client (Dart Implementation)
// Run: dart un.dart [options] <source_file>
// Compile: dart compile exe un.dart -o un
// Requires: UNSANDBOX_API_KEY environment variable

import 'dart:io';
import 'dart:convert';
import 'package:crypto/crypto.dart';

const String apiBase = 'https://api.unsandbox.com';
const String portalBase = 'https://unsandbox.com';
const String blue = '\x1B[34m';
const String red = '\x1B[31m';
const String green = '\x1B[32m';
const String yellow = '\x1B[33m';
const String reset = '\x1B[0m';
const int languagesCacheTtl = 3600; // 1 hour in seconds

const Map<String, String> extMap = {
  '.py': 'python', '.js': 'javascript', '.ts': 'typescript',
  '.rb': 'ruby', '.php': 'php', '.pl': 'perl', '.lua': 'lua',
  '.sh': 'bash', '.go': 'go', '.rs': 'rust', '.c': 'c',
  '.cpp': 'cpp', '.cc': 'cpp', '.cxx': 'cpp',
  '.java': 'java', '.kt': 'kotlin', '.cs': 'csharp', '.fs': 'fsharp',
  '.hs': 'haskell', '.ml': 'ocaml', '.clj': 'clojure', '.scm': 'scheme',
  '.lisp': 'commonlisp', '.erl': 'erlang', '.ex': 'elixir', '.exs': 'elixir',
  '.jl': 'julia', '.r': 'r', '.R': 'r', '.cr': 'crystal',
  '.d': 'd', '.nim': 'nim', '.zig': 'zig', '.v': 'v',
  '.dart': 'dart', '.groovy': 'groovy', '.scala': 'scala',
  '.f90': 'fortran', '.f95': 'fortran', '.cob': 'cobol',
  '.pro': 'prolog', '.forth': 'forth', '.4th': 'forth',
  '.tcl': 'tcl', '.raku': 'raku', '.m': 'objc',
};

class Args {
  String? command;
  String? sourceFile;
  String? apiKey;
  String? network;
  int vcpu = 0;
  List<String> env = [];
  List<String> files = [];
  bool artifacts = false;
  String? outputDir;
  bool sessionList = false;
  String? sessionShell;
  String? sessionKill;
  bool serviceList = false;
  bool languagesJson = false;
  String? serviceName;
  String? servicePorts;
  String? serviceType;
  String? serviceBootstrap;
  String? serviceBootstrapFile;
  String? serviceInfo;
  String? serviceLogs;
  String? serviceTail;
  String? serviceSleep;
  String? serviceWake;
  String? serviceDestroy;
  String? serviceResize;
  int serviceResizeVcpu = 0;
  String? serviceExecute;
  String? serviceCommand;
  String? serviceDumpBootstrap;
  String? serviceDumpFile;
  String? serviceUnfreezeOnDemand;
  bool serviceUnfreezeOnDemandEnabled = true;
  bool serviceCreateUnfreezeOnDemand = false;
  String? serviceShowFreezePage;
  bool serviceShowFreezePageEnabled = true;
  bool keyExtend = false;
  String? envFile;
  String? envAction;
  String? envTarget;
  // Image command options
  bool imageList = false;
  String? imageInfo;
  String? imageDelete;
  String? imageLock;
  String? imageUnlock;
  String? imagePublish;
  String? imageSourceType;
  String? imageVisibility;
  String? imageVisibilityMode;
  String? imageSpawn;
  String? imageClone;
  String? imageName;
  String? imagePorts;
  // Snapshot command options
  bool snapshotList = false;
  String? snapshotInfo;
  String? snapshotSession;
  String? snapshotService;
  String? snapshotRestore;
  String? snapshotDelete;
  String? snapshotLock;
  String? snapshotUnlock;
  String? snapshotClone;
  String? snapshotCloneType;
  String? snapshotName;
  String? snapshotPorts;
  String? snapshotShell;
  bool snapshotHot = false;
}

List<String?> getApiKeys(String? argsKey) {
  final publicKey = Platform.environment['UNSANDBOX_PUBLIC_KEY'];
  final secretKey = Platform.environment['UNSANDBOX_SECRET_KEY'];

  // Fall back to UNSANDBOX_API_KEY for backwards compatibility
  if (publicKey == null || publicKey.isEmpty || secretKey == null || secretKey.isEmpty) {
    final legacyKey = argsKey ?? Platform.environment['UNSANDBOX_API_KEY'];
    if (legacyKey == null || legacyKey.isEmpty) {
      stderr.writeln('${red}Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set$reset');
      exit(1);
    }
    return [legacyKey, null];
  }

  return [publicKey, secretKey];
}

String detectLanguage(String filename) {
  final dotIndex = filename.lastIndexOf('.');
  if (dotIndex == -1) {
    throw Exception('Cannot detect language: no file extension');
  }
  final ext = filename.substring(dotIndex).toLowerCase();
  final lang = extMap[ext];
  if (lang == null) {
    throw Exception('Unsupported file extension: $ext');
  }
  return lang;
}

String? getLanguagesCachePath() {
  final home = Platform.environment['HOME'];
  if (home == null || home.isEmpty) return null;
  return '$home/.unsandbox/languages.json';
}

Future<Map<String, dynamic>?> loadLanguagesCache() async {
  final cachePath = getLanguagesCachePath();
  if (cachePath == null) return null;

  final cacheFile = File(cachePath);
  if (!await cacheFile.exists()) return null;

  try {
    final content = await cacheFile.readAsString();
    final data = jsonDecode(content) as Map<String, dynamic>;

    final cachedTime = data['timestamp'] as int?;
    if (cachedTime == null) return null;

    final currentTime = DateTime.now().millisecondsSinceEpoch ~/ 1000;

    // Check if cache is still valid (within TTL)
    if (currentTime - cachedTime < languagesCacheTtl) {
      return data;
    }
  } catch (e) {
    // Cache read failed, return null to fetch fresh
  }

  return null;
}

Future<void> saveLanguagesCache(Map<String, dynamic> response) async {
  final cachePath = getLanguagesCachePath();
  if (cachePath == null) return;

  try {
    final cacheFile = File(cachePath);

    // Ensure directory exists
    final cacheDir = cacheFile.parent;
    if (!await cacheDir.exists()) {
      await cacheDir.create(recursive: true);
    }

    // Extract languages from response
    final languages = response['languages'];
    if (languages == null) return;

    // Build cache JSON with timestamp
    final timestamp = DateTime.now().millisecondsSinceEpoch ~/ 1000;
    final cacheData = {
      'languages': languages,
      'timestamp': timestamp,
    };

    await cacheFile.writeAsString(jsonEncode(cacheData));
  } catch (e) {
    // Cache write failed, ignore
  }
}

Future<Map<String, dynamic>> apiRequestCurl(String endpoint, String method, String? jsonData, String publicKey, String? secretKey, {String? baseUrl}) async {
  final base = baseUrl ?? apiBase;
  final tempFile = await File('${Directory.systemTemp.path}/un_request_${DateTime.now().millisecondsSinceEpoch}.json').create();

  try {
    final body = jsonData ?? '';
    if (jsonData != null) {
      await tempFile.writeAsString(jsonData);
    }

    final args = ['curl', '-s', '-X', method, '$base$endpoint',
                  '-H', 'Content-Type: application/json'];

    // Add HMAC authentication headers if secretKey is provided
    if (secretKey != null && secretKey.isNotEmpty) {
      final timestamp = (DateTime.now().millisecondsSinceEpoch ~/ 1000).toString();
      final message = '$timestamp:$method:$endpoint:$body';

      final key = utf8.encode(secretKey);
      final bytes = utf8.encode(message);
      final hmacSha256 = Hmac(sha256, key);
      final digest = hmacSha256.convert(bytes);
      final signature = digest.toString();

      args.addAll(['-H', 'Authorization: Bearer $publicKey']);
      args.addAll(['-H', 'X-Timestamp: $timestamp']);
      args.addAll(['-H', 'X-Signature: $signature']);
    } else {
      // Legacy API key authentication
      args.addAll(['-H', 'Authorization: Bearer $publicKey']);
    }

    if (jsonData != null) {
      args.addAll(['-d', '@${tempFile.path}']);
    }

    final result = await Process.run(args[0], args.sublist(1));

    if (result.exitCode != 0) {
      throw Exception('curl failed: ${result.stderr}');
    }

    final response = result.stdout as String;

    // Check for timestamp authentication errors
    if (response.toLowerCase().contains('timestamp') &&
        (response.contains('401') || response.toLowerCase().contains('expired') || response.toLowerCase().contains('invalid'))) {
      stderr.writeln('${red}Error: Request timestamp expired (must be within 5 minutes of server time)$reset');
      stderr.writeln('${yellow}Your computer\'s clock may have drifted.$reset');
      stderr.writeln('Check your system time and sync with NTP if needed:');
      stderr.writeln('  Linux:   sudo ntpdate -s time.nist.gov');
      stderr.writeln('  macOS:   sudo sntp -sS time.apple.com');
      stderr.writeln('  Windows: w32tm /resync');
      exit(1);
    }

    return jsonDecode(response) as Map<String, dynamic>;
  } finally {
    await tempFile.delete();
  }
}

/// Make authenticated API request returning status code and body
Future<(int, String)> apiRequestCurlWithStatus(String endpoint, String method, String? jsonData, String publicKey, String? secretKey, {String? baseUrl, String? sudoOtp, String? sudoChallenge}) async {
  final base = baseUrl ?? apiBase;
  final tempFile = await File('${Directory.systemTemp.path}/un_request_${DateTime.now().millisecondsSinceEpoch}.json').create();

  try {
    final body = jsonData ?? '';
    if (jsonData != null) {
      await tempFile.writeAsString(jsonData);
    }

    final args = ['curl', '-s', '-w', '%{http_code}', '-X', method, '$base$endpoint',
                  '-H', 'Content-Type: application/json'];

    // Add HMAC authentication headers if secretKey is provided
    if (secretKey != null && secretKey.isNotEmpty) {
      final timestamp = (DateTime.now().millisecondsSinceEpoch ~/ 1000).toString();
      final message = '$timestamp:$method:$endpoint:$body';

      final key = utf8.encode(secretKey);
      final bytes = utf8.encode(message);
      final hmacSha256 = Hmac(sha256, key);
      final digest = hmacSha256.convert(bytes);
      final signature = digest.toString();

      args.addAll(['-H', 'Authorization: Bearer $publicKey']);
      args.addAll(['-H', 'X-Timestamp: $timestamp']);
      args.addAll(['-H', 'X-Signature: $signature']);
    } else {
      // Legacy API key authentication
      args.addAll(['-H', 'Authorization: Bearer $publicKey']);
    }

    // Add sudo OTP headers if provided
    if (sudoOtp != null && sudoChallenge != null) {
      args.addAll(['-H', 'X-Sudo-OTP: $sudoOtp']);
      args.addAll(['-H', 'X-Sudo-Challenge: $sudoChallenge']);
    }

    if (jsonData != null) {
      args.addAll(['-d', '@${tempFile.path}']);
    }

    final result = await Process.run(args[0], args.sublist(1));

    if (result.exitCode != 0) {
      throw Exception('curl failed: ${result.stderr}');
    }

    final output = result.stdout as String;

    // Extract status code from end of output (last 3 chars)
    int statusCode = 0;
    String responseBody = output;
    if (output.length >= 3) {
      final codeStr = output.substring(output.length - 3);
      statusCode = int.tryParse(codeStr) ?? 0;
      responseBody = output.substring(0, output.length - 3);
    }

    return (statusCode, responseBody);
  } finally {
    await tempFile.delete();
  }
}

/// Handle HTTP 428 sudo OTP challenge
/// Returns true if retry succeeded, false otherwise
Future<bool> handleSudoChallenge(String responseBody, String endpoint, String method, String? jsonData, String publicKey, String? secretKey) async {
  // Extract challenge_id from response
  String? challengeId;
  try {
    final resp = jsonDecode(responseBody) as Map<String, dynamic>;
    challengeId = resp['challenge_id'] as String?;
  } catch (e) {
    stderr.writeln('${red}Error: Could not parse challenge response$reset');
    return false;
  }

  if (challengeId == null || challengeId.isEmpty) {
    stderr.writeln('${red}Error: Could not extract challenge_id from response$reset');
    return false;
  }

  // Prompt user for OTP
  stderr.writeln('${yellow}Confirmation required. Check your email for a one-time code.$reset');
  stderr.write('Enter OTP: ');
  final otp = stdin.readLineSync()?.trim();

  if (otp == null || otp.isEmpty) {
    stderr.writeln('${red}Error: No OTP provided$reset');
    return false;
  }

  // Retry request with sudo headers
  final (statusCode, _) = await apiRequestCurlWithStatus(
    endpoint, method, jsonData, publicKey, secretKey,
    sudoOtp: otp, sudoChallenge: challengeId
  );

  if (statusCode >= 200 && statusCode < 300) {
    return true;
  } else {
    stderr.writeln('${red}Error: OTP verification failed$reset');
    return false;
  }
}

Future<Map<String, dynamic>?> apiRequestTextCurl(String endpoint, String method, String body, String publicKey, String? secretKey) async {
  final tempFile = await File('${Directory.systemTemp.path}/un_request_${DateTime.now().millisecondsSinceEpoch}.txt').create();

  try {
    await tempFile.writeAsString(body);

    final args = ['curl', '-s', '-X', method, '$apiBase$endpoint',
                  '-H', 'Content-Type: text/plain'];

    if (secretKey != null && secretKey.isNotEmpty) {
      final timestamp = (DateTime.now().millisecondsSinceEpoch ~/ 1000).toString();
      final message = '$timestamp:$method:$endpoint:$body';

      final key = utf8.encode(secretKey);
      final bytes = utf8.encode(message);
      final hmacSha256 = Hmac(sha256, key);
      final digest = hmacSha256.convert(bytes);
      final signature = digest.toString();

      args.addAll(['-H', 'Authorization: Bearer $publicKey']);
      args.addAll(['-H', 'X-Timestamp: $timestamp']);
      args.addAll(['-H', 'X-Signature: $signature']);
    } else {
      args.addAll(['-H', 'Authorization: Bearer $publicKey']);
    }

    args.addAll(['-d', '@${tempFile.path}', '-w', '%{http_code}']);

    final result = await Process.run(args[0], args.sublist(1));
    final output = result.stdout as String;

    // Last 3 characters are the status code
    if (output.length >= 3) {
      final statusCode = int.tryParse(output.substring(output.length - 3)) ?? 0;
      final responseBody = output.substring(0, output.length - 3);
      if (statusCode >= 200 && statusCode < 300) {
        if (responseBody.isNotEmpty) {
          try {
            return jsonDecode(responseBody) as Map<String, dynamic>;
          } catch (e) {
            return {'success': true};
          }
        }
        return {'success': true};
      }
    }
    return null;
  } finally {
    await tempFile.delete();
  }
}

const int maxEnvContentSize = 65536;

Future<String> readEnvFile(String path) async {
  final file = File(path);
  if (!await file.exists()) {
    stderr.writeln('${red}Error: Env file not found: $path$reset');
    exit(1);
  }
  return await file.readAsString();
}

Future<String> buildEnvContent(List<String> envs, String? envFile) async {
  final lines = <String>[];
  lines.addAll(envs);
  if (envFile != null) {
    final content = await readEnvFile(envFile);
    for (final line in content.split('\n')) {
      final trimmed = line.trim();
      if (trimmed.isNotEmpty && !trimmed.startsWith('#')) {
        lines.add(trimmed);
      }
    }
  }
  return lines.join('\n');
}

Future<Map<String, dynamic>> serviceEnvStatus(String serviceId, String publicKey, String? secretKey) async {
  return await apiRequestCurl('/services/$serviceId/env', 'GET', null, publicKey, secretKey);
}

Future<bool> serviceEnvSet(String serviceId, String envContent, String publicKey, String? secretKey) async {
  if (envContent.length > maxEnvContentSize) {
    stderr.writeln('${red}Error: Env content exceeds maximum size of 64KB$reset');
    return false;
  }
  final result = await apiRequestTextCurl('/services/$serviceId/env', 'PUT', envContent, publicKey, secretKey);
  return result != null;
}

Future<Map<String, dynamic>> serviceEnvExport(String serviceId, String publicKey, String? secretKey) async {
  return await apiRequestCurl('/services/$serviceId/env/export', 'POST', '{}', publicKey, secretKey);
}

Future<bool> serviceEnvDelete(String serviceId, String publicKey, String? secretKey) async {
  try {
    await apiRequestCurl('/services/$serviceId/env', 'DELETE', null, publicKey, secretKey);
    return true;
  } catch (e) {
    return false;
  }
}

Future<void> cmdServiceEnv(Args args) async {
  final keys = getApiKeys(args.apiKey);
  final publicKey = keys[0]!;
  final secretKey = keys[1];
  final action = args.envAction;
  final target = args.envTarget;

  switch (action) {
    case 'status':
      if (target == null) {
        stderr.writeln('${red}Error: service env status requires service ID$reset');
        exit(1);
      }
      final result = await serviceEnvStatus(target, publicKey, secretKey);
      final hasVault = result['has_vault'] as bool? ?? false;
      if (hasVault) {
        print('${green}Vault: configured$reset');
        final envCount = result['env_count'];
        if (envCount != null) print('Variables: $envCount');
        final updatedAt = result['updated_at'];
        if (updatedAt != null) print('Updated: $updatedAt');
      } else {
        print('${yellow}Vault: not configured$reset');
      }
      break;
    case 'set':
      if (target == null) {
        stderr.writeln('${red}Error: service env set requires service ID$reset');
        exit(1);
      }
      if (args.env.isEmpty && args.envFile == null) {
        stderr.writeln('${red}Error: service env set requires -e or --env-file$reset');
        exit(1);
      }
      final envContent = await buildEnvContent(args.env, args.envFile);
      if (await serviceEnvSet(target, envContent, publicKey, secretKey)) {
        print('${green}Vault updated for service $target$reset');
      } else {
        stderr.writeln('${red}Error: Failed to update vault$reset');
        exit(1);
      }
      break;
    case 'export':
      if (target == null) {
        stderr.writeln('${red}Error: service env export requires service ID$reset');
        exit(1);
      }
      final result = await serviceEnvExport(target, publicKey, secretKey);
      final content = result['content'] as String?;
      if (content != null) stdout.write(content);
      break;
    case 'delete':
      if (target == null) {
        stderr.writeln('${red}Error: service env delete requires service ID$reset');
        exit(1);
      }
      if (await serviceEnvDelete(target, publicKey, secretKey)) {
        print('${green}Vault deleted for service $target$reset');
      } else {
        stderr.writeln('${red}Error: Failed to delete vault$reset');
        exit(1);
      }
      break;
    default:
      stderr.writeln('${red}Error: Unknown env action: $action$reset');
      stderr.writeln('Usage: dart un.dart service env <status|set|export|delete> <service_id>');
      exit(1);
  }
}

Future<void> cmdExecute(Args args) async {
  final keys = getApiKeys(args.apiKey);
  final publicKey = keys[0]!;
  final secretKey = keys[1];
  final code = await File(args.sourceFile!).readAsString();
  final language = detectLanguage(args.sourceFile!);

  final payload = <String, dynamic>{
    'language': language,
    'code': code,
  };

  if (args.env.isNotEmpty) {
    final envVars = <String, String>{};
    for (final e in args.env) {
      final parts = e.split('=');
      if (parts.length == 2) {
        envVars[parts[0]] = parts[1];
      }
    }
    if (envVars.isNotEmpty) {
      payload['env'] = envVars;
    }
  }

  if (args.files.isNotEmpty) {
    final inputFiles = <Map<String, String>>[];
    for (final filepath in args.files) {
      final content = await File(filepath).readAsBytes();
      inputFiles.add({
        'filename': filepath.split('/').last,
        'content_base64': base64Encode(content),
      });
    }
    payload['input_files'] = inputFiles;
  }

  if (args.artifacts) {
    payload['return_artifacts'] = true;
  }
  if (args.network != null) {
    payload['network'] = args.network;
  }
  if (args.vcpu > 0) {
    payload['vcpu'] = args.vcpu;
  }

  final result = await apiRequestCurl('/execute', 'POST', jsonEncode(payload), publicKey, secretKey);

  final stdoutText = result['stdout'] as String?;
  final stderrText = result['stderr'] as String?;

  if (stdoutText != null && stdoutText.isNotEmpty) {
    stdout.write('$blue$stdoutText$reset');
  }
  if (stderrText != null && stderrText.isNotEmpty) {
    stderr.write('$red$stderrText$reset');
  }

  if (args.artifacts && result.containsKey('artifacts')) {
    final artifacts = result['artifacts'] as List;
    final outDir = args.outputDir ?? '.';
    await Directory(outDir).create(recursive: true);
    for (final artifact in artifacts) {
      final artifactMap = artifact as Map<String, dynamic>;
      final filename = artifactMap['filename'] as String? ?? 'artifact';
      final content = base64Decode(artifactMap['content_base64'] as String);
      final file = File('$outDir/$filename');
      await file.writeAsBytes(content);
      await Process.run('chmod', ['+x', file.path]);
      stderr.writeln('${green}Saved: ${file.path}$reset');
    }
  }

  final exitCode = result['exit_code'] as int? ?? 0;
  exit(exitCode);
}

Future<void> cmdSession(Args args) async {
  final keys = getApiKeys(args.apiKey);
  final publicKey = keys[0]!;
  final secretKey = keys[1];

  if (args.sessionList) {
    final result = await apiRequestCurl('/sessions', 'GET', null, publicKey, secretKey);
    final sessions = result['sessions'] as List? ?? [];
    if (sessions.isEmpty) {
      print('No active sessions');
    } else {
      print('${'ID'.padRight(40)} ${'Shell'.padRight(10)} ${'Status'.padRight(10)} Created');
      for (final s in sessions) {
        final session = s as Map<String, dynamic>;
        print('${(session['id'] ?? 'N/A').toString().padRight(40)} ${(session['shell'] ?? 'N/A').toString().padRight(10)} ${(session['status'] ?? 'N/A').toString().padRight(10)} ${session['created_at'] ?? 'N/A'}');
      }
    }
    return;
  }

  if (args.sessionKill != null) {
    await apiRequestCurl('/sessions/${args.sessionKill}', 'DELETE', null, publicKey, secretKey);
    print('${green}Session terminated: ${args.sessionKill}$reset');
    return;
  }

  final payload = <String, dynamic>{
    'shell': args.sessionShell ?? 'bash',
  };
  if (args.network != null) {
    payload['network'] = args.network;
  }
  if (args.vcpu > 0) {
    payload['vcpu'] = args.vcpu;
  }

  // Add input files
  if (args.files.isNotEmpty) {
    final inputFiles = <Map<String, String>>[];
    for (final filepath in args.files) {
      final file = File(filepath);
      if (!await file.exists()) {
        stderr.writeln('${red}Error: Input file not found: $filepath$reset');
        exit(1);
      }
      final content = await file.readAsBytes();
      inputFiles.add({
        'filename': filepath.split('/').last,
        'content_base64': base64Encode(content),
      });
    }
    payload['input_files'] = inputFiles;
  }

  print('${yellow}Creating session...$reset');
  final result = await apiRequestCurl('/sessions', 'POST', jsonEncode(payload), publicKey, secretKey);
  print('${green}Session created: ${result['id'] ?? 'N/A'}$reset');
  print('${yellow}(Interactive sessions require WebSocket - use un2 for full support)$reset');
}

Future<void> cmdService(Args args) async {
  final keys = getApiKeys(args.apiKey);
  final publicKey = keys[0]!;
  final secretKey = keys[1];

  // Handle env subcommand
  if (args.envAction != null) {
    await cmdServiceEnv(args);
    return;
  }

  if (args.serviceList) {
    final result = await apiRequestCurl('/services', 'GET', null, publicKey, secretKey);
    final services = result['services'] as List? ?? [];
    if (services.isEmpty) {
      print('No services');
    } else {
      print('${'ID'.padRight(20)} ${'Name'.padRight(15)} ${'Status'.padRight(10)} ${'Ports'.padRight(15)} Domains');
      for (final s in services) {
        final service = s as Map<String, dynamic>;
        final ports = (service['ports'] as List?)?.join(',') ?? '';
        final domains = (service['domains'] as List?)?.join(',') ?? '';
        print('${(service['id'] ?? 'N/A').toString().padRight(20)} ${(service['name'] ?? 'N/A').toString().padRight(15)} ${(service['status'] ?? 'N/A').toString().padRight(10)} ${ports.padRight(15)} $domains');
      }
    }
    return;
  }

  if (args.serviceInfo != null) {
    final result = await apiRequestCurl('/services/${args.serviceInfo}', 'GET', null, publicKey, secretKey);
    print(jsonEncode(result));
    return;
  }

  if (args.serviceLogs != null) {
    final result = await apiRequestCurl('/services/${args.serviceLogs}/logs', 'GET', null, publicKey, secretKey);
    print(result['logs'] ?? '');
    return;
  }

  if (args.serviceTail != null) {
    final result = await apiRequestCurl('/services/${args.serviceTail}/logs?lines=9000', 'GET', null, publicKey, secretKey);
    print(result['logs'] ?? '');
    return;
  }

  if (args.serviceSleep != null) {
    await apiRequestCurl('/services/${args.serviceSleep}/freeze', 'POST', null, publicKey, secretKey);
    print('${green}Service frozen: ${args.serviceSleep}$reset');
    return;
  }

  if (args.serviceWake != null) {
    await apiRequestCurl('/services/${args.serviceWake}/unfreeze', 'POST', null, publicKey, secretKey);
    print('${green}Service unfreezing: ${args.serviceWake}$reset');
    return;
  }

  if (args.serviceUnfreezeOnDemand != null) {
    final payload = {'unfreeze_on_demand': args.serviceUnfreezeOnDemandEnabled};
    await apiRequestCurl('/services/${args.serviceUnfreezeOnDemand}', 'PATCH', jsonEncode(payload), publicKey, secretKey);
    final status = args.serviceUnfreezeOnDemandEnabled ? 'enabled' : 'disabled';
    print('${green}Unfreeze-on-demand $status for service: ${args.serviceUnfreezeOnDemand}$reset');
    return;
  }

  if (args.serviceShowFreezePage != null) {
    final payload = {'show_freeze_page': args.serviceShowFreezePageEnabled};
    await apiRequestCurl('/services/${args.serviceShowFreezePage}', 'PATCH', jsonEncode(payload), publicKey, secretKey);
    final status = args.serviceShowFreezePageEnabled ? 'enabled' : 'disabled';
    print('${green}Show-freeze-page $status for service: ${args.serviceShowFreezePage}$reset');
    return;
  }

  if (args.serviceDestroy != null) {
    final (statusCode, responseBody) = await apiRequestCurlWithStatus('/services/${args.serviceDestroy}', 'DELETE', null, publicKey, secretKey);
    if (statusCode == 428) {
      if (await handleSudoChallenge(responseBody, '/services/${args.serviceDestroy}', 'DELETE', null, publicKey, secretKey)) {
        print('${green}Service destroyed: ${args.serviceDestroy}$reset');
      } else {
        stderr.writeln('${red}Error: Failed to destroy service (OTP verification failed)$reset');
        exit(1);
      }
    } else if (statusCode >= 200 && statusCode < 300) {
      print('${green}Service destroyed: ${args.serviceDestroy}$reset');
    } else {
      stderr.writeln('${red}Error: Failed to destroy service (HTTP $statusCode)$reset');
      exit(1);
    }
    return;
  }

  if (args.serviceResize != null) {
    if (args.serviceResizeVcpu < 1 || args.serviceResizeVcpu > 8) {
      stderr.writeln('${red}Error: --vcpu must be between 1 and 8$reset');
      exit(1);
    }
    final payload = {'vcpu': args.serviceResizeVcpu};
    await apiRequestCurl('/services/${args.serviceResize}', 'PATCH', jsonEncode(payload), publicKey, secretKey);
    final ram = args.serviceResizeVcpu * 2;
    print('${green}Service resized to ${args.serviceResizeVcpu} vCPU, $ram GB RAM$reset');
    return;
  }

  if (args.serviceExecute != null) {
    final payload = <String, dynamic>{
      'command': args.serviceCommand,
    };
    final result = await apiRequestCurl('/services/${args.serviceExecute}/execute', 'POST', jsonEncode(payload), publicKey, secretKey);
    final stdoutText = result['stdout'] as String?;
    final stderrText = result['stderr'] as String?;
    if (stdoutText != null && stdoutText.isNotEmpty) {
      stdout.write('$blue$stdoutText$reset');
    }
    if (stderrText != null && stderrText.isNotEmpty) {
      stderr.write('$red$stderrText$reset');
    }
    return;
  }

  if (args.serviceDumpBootstrap != null) {
    stderr.writeln('Fetching bootstrap script from ${args.serviceDumpBootstrap}...');
    final payload = <String, dynamic>{
      'command': 'cat /tmp/bootstrap.sh',
    };
    final result = await apiRequestCurl('/services/${args.serviceDumpBootstrap}/execute', 'POST', jsonEncode(payload), publicKey, secretKey);

    final bootstrap = result['stdout'] as String?;
    if (bootstrap != null && bootstrap.isNotEmpty) {
      if (args.serviceDumpFile != null) {
        try {
          await File(args.serviceDumpFile!).writeAsString(bootstrap);
          await Process.run('chmod', ['755', args.serviceDumpFile!]);
          print('Bootstrap saved to ${args.serviceDumpFile}');
        } catch (e) {
          stderr.writeln('${red}Error: Could not write to ${args.serviceDumpFile}: $e$reset');
          exit(1);
        }
      } else {
        stdout.write(bootstrap);
      }
    } else {
      stderr.writeln('${red}Error: Failed to fetch bootstrap (service not running or no bootstrap file)$reset');
      exit(1);
    }
    return;
  }

  if (args.serviceName != null) {
    final payload = <String, dynamic>{
      'name': args.serviceName!,
    };
    if (args.servicePorts != null) {
      payload['ports'] = args.servicePorts!.split(',').map((p) => int.parse(p.trim())).toList();
    }
    if (args.serviceType != null) {
      payload['service_type'] = args.serviceType;
    }
    if (args.serviceBootstrap != null) {
      payload['bootstrap'] = args.serviceBootstrap;
    }
    if (args.serviceBootstrapFile != null) {
      final file = File(args.serviceBootstrapFile!);
      if (await file.exists()) {
        payload['bootstrap_content'] = await file.readAsString();
      } else {
        stderr.writeln('${red}Error: Bootstrap file not found: ${args.serviceBootstrapFile}$reset');
        exit(1);
      }
    }
    if (args.network != null) {
      payload['network'] = args.network;
    }
    if (args.vcpu > 0) {
      payload['vcpu'] = args.vcpu;
    }
    if (args.serviceCreateUnfreezeOnDemand) {
      payload['unfreeze_on_demand'] = true;
    }

    // Add input files
    if (args.files.isNotEmpty) {
      final inputFiles = <Map<String, String>>[];
      for (final filepath in args.files) {
        final file = File(filepath);
        if (!await file.exists()) {
          stderr.writeln('${red}Error: Input file not found: $filepath$reset');
          exit(1);
        }
        final content = await file.readAsBytes();
        inputFiles.add({
          'filename': filepath.split('/').last,
          'content_base64': base64Encode(content),
        });
      }
      payload['input_files'] = inputFiles;
    }

    final result = await apiRequestCurl('/services', 'POST', jsonEncode(payload), publicKey, secretKey);
    final serviceId = result['id'] as String?;
    print('${green}Service created: ${serviceId ?? 'N/A'}$reset');
    print('Name: ${result['name'] ?? 'N/A'}');
    if (result.containsKey('url')) {
      print('URL: ${result['url']}');
    }

    // Auto-set vault if env vars were provided
    if (serviceId != null && (args.env.isNotEmpty || args.envFile != null)) {
      final envContent = await buildEnvContent(args.env, args.envFile);
      if (envContent.isNotEmpty) {
        if (await serviceEnvSet(serviceId, envContent, publicKey, secretKey)) {
          print('${green}Vault configured with environment variables$reset');
        } else {
          print('${yellow}Warning: Failed to set vault$reset');
        }
      }
    }
    return;
  }

  stderr.writeln('${red}Error: Specify --name to create a service, or use --list, --info, etc.$reset');
  exit(1);
}

Future<void> cmdLanguages(Args args) async {
  final keys = getApiKeys(args.apiKey);
  final publicKey = keys[0]!;
  final secretKey = keys[1];

  // Try to load from cache first
  var cachedData = await loadLanguagesCache();
  Map<String, dynamic> result;

  if (cachedData != null) {
    result = cachedData;
  } else {
    // Fetch from API
    result = await apiRequestCurl('/languages', 'GET', null, publicKey, secretKey);

    // Save to cache
    await saveLanguagesCache(result);
  }

  final languages = result['languages'] as List? ?? [];

  if (args.languagesJson) {
    // JSON output: print as array
    print(jsonEncode(languages));
  } else {
    // Default: one language per line
    for (final lang in languages) {
      print(lang.toString());
    }
  }
}

Future<void> cmdImage(Args args) async {
  final keys = getApiKeys(args.apiKey);
  final publicKey = keys[0]!;
  final secretKey = keys[1];

  if (args.imageList) {
    final result = await apiRequestCurl('/images', 'GET', null, publicKey, secretKey);
    print(jsonEncode(result));
    return;
  }

  if (args.imageInfo != null) {
    final result = await apiRequestCurl('/images/${args.imageInfo}', 'GET', null, publicKey, secretKey);
    print(jsonEncode(result));
    return;
  }

  if (args.imageDelete != null) {
    final (statusCode, responseBody) = await apiRequestCurlWithStatus('/images/${args.imageDelete}', 'DELETE', null, publicKey, secretKey);
    if (statusCode == 428) {
      if (await handleSudoChallenge(responseBody, '/images/${args.imageDelete}', 'DELETE', null, publicKey, secretKey)) {
        print('${green}Image deleted: ${args.imageDelete}$reset');
      } else {
        stderr.writeln('${red}Error: Failed to delete image (OTP verification failed)$reset');
        exit(1);
      }
    } else if (statusCode >= 200 && statusCode < 300) {
      print('${green}Image deleted: ${args.imageDelete}$reset');
    } else {
      stderr.writeln('${red}Error: Failed to delete image (HTTP $statusCode)$reset');
      exit(1);
    }
    return;
  }

  if (args.imageLock != null) {
    await apiRequestCurl('/images/${args.imageLock}/lock', 'POST', null, publicKey, secretKey);
    print('${green}Image locked: ${args.imageLock}$reset');
    return;
  }

  if (args.imageUnlock != null) {
    final (statusCode, responseBody) = await apiRequestCurlWithStatus('/images/${args.imageUnlock}/unlock', 'POST', null, publicKey, secretKey);
    if (statusCode == 428) {
      if (await handleSudoChallenge(responseBody, '/images/${args.imageUnlock}/unlock', 'POST', null, publicKey, secretKey)) {
        print('${green}Image unlocked: ${args.imageUnlock}$reset');
      } else {
        stderr.writeln('${red}Error: Failed to unlock image (OTP verification failed)$reset');
        exit(1);
      }
    } else if (statusCode >= 200 && statusCode < 300) {
      print('${green}Image unlocked: ${args.imageUnlock}$reset');
    } else {
      stderr.writeln('${red}Error: Failed to unlock image (HTTP $statusCode)$reset');
      exit(1);
    }
    return;
  }

  if (args.imagePublish != null) {
    if (args.imageSourceType == null) {
      stderr.writeln('${red}Error: --source-type required (service or snapshot)$reset');
      exit(1);
    }
    final payload = <String, dynamic>{
      'source_type': args.imageSourceType,
      'source_id': args.imagePublish,
    };
    if (args.imageName != null) {
      payload['name'] = args.imageName;
    }
    final result = await apiRequestCurl('/images/publish', 'POST', jsonEncode(payload), publicKey, secretKey);
    print('${green}Image published$reset');
    print(jsonEncode(result));
    return;
  }

  if (args.imageVisibility != null) {
    if (args.imageVisibilityMode == null) {
      stderr.writeln('${red}Error: --visibility requires MODE (private, unlisted, or public)$reset');
      exit(1);
    }
    final payload = {'visibility': args.imageVisibilityMode};
    await apiRequestCurl('/images/${args.imageVisibility}/visibility', 'POST', jsonEncode(payload), publicKey, secretKey);
    print('${green}Image visibility set to ${args.imageVisibilityMode}$reset');
    return;
  }

  if (args.imageSpawn != null) {
    final payload = <String, dynamic>{};
    if (args.imageName != null) {
      payload['name'] = args.imageName;
    }
    if (args.imagePorts != null) {
      payload['ports'] = args.imagePorts!.split(',').map((p) => int.parse(p.trim())).toList();
    }
    final result = await apiRequestCurl('/images/${args.imageSpawn}/spawn', 'POST', jsonEncode(payload), publicKey, secretKey);
    print('${green}Service spawned from image$reset');
    print(jsonEncode(result));
    return;
  }

  if (args.imageClone != null) {
    final payload = <String, dynamic>{};
    if (args.imageName != null) {
      payload['name'] = args.imageName;
    }
    final result = await apiRequestCurl('/images/${args.imageClone}/clone', 'POST', jsonEncode(payload), publicKey, secretKey);
    print('${green}Image cloned$reset');
    print(jsonEncode(result));
    return;
  }

  stderr.writeln('${red}Error: Use --list, --info ID, --delete ID, --lock ID, --unlock ID, --publish ID, --visibility ID MODE, --spawn ID, or --clone ID$reset');
  exit(1);
}

// Image access management functions
Future<void> imageGrantAccess(String id, String trustedKey, String publicKey, String? secretKey) async {
  final payload = {'trusted_api_key': trustedKey};
  await apiRequestCurl('/images/$id/grant-access', 'POST', jsonEncode(payload), publicKey, secretKey);
  print('${green}Access granted to: $trustedKey$reset');
}

Future<void> imageRevokeAccess(String id, String trustedKey, String publicKey, String? secretKey) async {
  final payload = {'trusted_api_key': trustedKey};
  await apiRequestCurl('/images/$id/revoke-access', 'POST', jsonEncode(payload), publicKey, secretKey);
  print('${green}Access revoked from: $trustedKey$reset');
}

Future<void> imageListTrusted(String id, String publicKey, String? secretKey) async {
  final result = await apiRequestCurl('/images/$id/trusted', 'GET', null, publicKey, secretKey);
  print(jsonEncode(result));
}

Future<void> imageTransfer(String id, String toKey, String publicKey, String? secretKey) async {
  final payload = {'to_api_key': toKey};
  await apiRequestCurl('/images/$id/transfer', 'POST', jsonEncode(payload), publicKey, secretKey);
  print('${green}Image transferred to: $toKey$reset');
}

// Snapshot functions
Future<void> cmdSnapshot(Args args) async {
  final keys = getApiKeys(args.apiKey);
  final publicKey = keys[0]!;
  final secretKey = keys[1];

  if (args.snapshotList) {
    final result = await apiRequestCurl('/snapshots', 'GET', null, publicKey, secretKey);
    print(jsonEncode(result));
    return;
  }

  if (args.snapshotInfo != null) {
    final result = await apiRequestCurl('/snapshots/${args.snapshotInfo}', 'GET', null, publicKey, secretKey);
    print(jsonEncode(result));
    return;
  }

  if (args.snapshotSession != null) {
    final payload = <String, dynamic>{
      'session_id': args.snapshotSession,
    };
    if (args.snapshotName != null) {
      payload['name'] = args.snapshotName;
    }
    if (args.snapshotHot) {
      payload['hot'] = true;
    }
    final result = await apiRequestCurl('/snapshots', 'POST', jsonEncode(payload), publicKey, secretKey);
    print('${green}Snapshot created$reset');
    print(jsonEncode(result));
    return;
  }

  if (args.snapshotService != null) {
    final payload = <String, dynamic>{
      'service_id': args.snapshotService,
    };
    if (args.snapshotName != null) {
      payload['name'] = args.snapshotName;
    }
    if (args.snapshotHot) {
      payload['hot'] = true;
    }
    final result = await apiRequestCurl('/snapshots', 'POST', jsonEncode(payload), publicKey, secretKey);
    print('${green}Snapshot created$reset');
    print(jsonEncode(result));
    return;
  }

  if (args.snapshotRestore != null) {
    await apiRequestCurl('/snapshots/${args.snapshotRestore}/restore', 'POST', '{}', publicKey, secretKey);
    print('${green}Snapshot restored: ${args.snapshotRestore}$reset');
    return;
  }

  if (args.snapshotDelete != null) {
    final (statusCode, responseBody) = await apiRequestCurlWithStatus('/snapshots/${args.snapshotDelete}', 'DELETE', null, publicKey, secretKey);
    if (statusCode == 428) {
      if (await handleSudoChallenge(responseBody, '/snapshots/${args.snapshotDelete}', 'DELETE', null, publicKey, secretKey)) {
        print('${green}Snapshot deleted: ${args.snapshotDelete}$reset');
      } else {
        stderr.writeln('${red}Error: Failed to delete snapshot (OTP verification failed)$reset');
        exit(1);
      }
    } else if (statusCode >= 200 && statusCode < 300) {
      print('${green}Snapshot deleted: ${args.snapshotDelete}$reset');
    } else {
      stderr.writeln('${red}Error: Failed to delete snapshot (HTTP $statusCode)$reset');
      exit(1);
    }
    return;
  }

  if (args.snapshotLock != null) {
    await apiRequestCurl('/snapshots/${args.snapshotLock}/lock', 'POST', '{}', publicKey, secretKey);
    print('${green}Snapshot locked: ${args.snapshotLock}$reset');
    return;
  }

  if (args.snapshotUnlock != null) {
    final (statusCode, responseBody) = await apiRequestCurlWithStatus('/snapshots/${args.snapshotUnlock}/unlock', 'POST', '{}', publicKey, secretKey);
    if (statusCode == 428) {
      if (await handleSudoChallenge(responseBody, '/snapshots/${args.snapshotUnlock}/unlock', 'POST', '{}', publicKey, secretKey)) {
        print('${green}Snapshot unlocked: ${args.snapshotUnlock}$reset');
      } else {
        stderr.writeln('${red}Error: Failed to unlock snapshot (OTP verification failed)$reset');
        exit(1);
      }
    } else if (statusCode >= 200 && statusCode < 300) {
      print('${green}Snapshot unlocked: ${args.snapshotUnlock}$reset');
    } else {
      stderr.writeln('${red}Error: Failed to unlock snapshot (HTTP $statusCode)$reset');
      exit(1);
    }
    return;
  }

  if (args.snapshotClone != null) {
    final payload = <String, dynamic>{
      'clone_type': args.snapshotCloneType ?? 'session',
    };
    if (args.snapshotName != null) {
      payload['name'] = args.snapshotName;
    }
    if (args.snapshotPorts != null) {
      payload['ports'] = args.snapshotPorts!.split(',').map((p) => int.parse(p.trim())).toList();
    }
    if (args.snapshotShell != null) {
      payload['shell'] = args.snapshotShell;
    }
    final result = await apiRequestCurl('/snapshots/${args.snapshotClone}/clone', 'POST', jsonEncode(payload), publicKey, secretKey);
    print('${green}Snapshot cloned$reset');
    print(jsonEncode(result));
    return;
  }

  stderr.writeln('${red}Error: Use --list, --info ID, --session ID, --service ID, --restore ID, --delete ID, --lock ID, --unlock ID, or --clone ID$reset');
  exit(1);
}

// Session additional functions
Future<void> sessionInfo(String id, String publicKey, String? secretKey) async {
  final result = await apiRequestCurl('/sessions/$id', 'GET', null, publicKey, secretKey);
  print(jsonEncode(result));
}

Future<void> sessionBoost(String id, int vcpu, String publicKey, String? secretKey) async {
  final payload = {'vcpu': vcpu};
  await apiRequestCurl('/sessions/$id', 'PATCH', jsonEncode(payload), publicKey, secretKey);
  print('${green}Session boosted to $vcpu vCPU$reset');
}

Future<void> sessionUnboost(String id, String publicKey, String? secretKey) async {
  final payload = {'vcpu': 1};
  await apiRequestCurl('/sessions/$id', 'PATCH', jsonEncode(payload), publicKey, secretKey);
  print('${green}Session unboosted to 1 vCPU$reset');
}

Future<void> sessionExecuteCmd(String id, String command, String publicKey, String? secretKey) async {
  final payload = {'command': command};
  final result = await apiRequestCurl('/sessions/$id/execute', 'POST', jsonEncode(payload), publicKey, secretKey);
  final stdoutText = result['stdout'] as String?;
  final stderrText = result['stderr'] as String?;
  if (stdoutText != null && stdoutText.isNotEmpty) {
    stdout.write('$blue$stdoutText$reset');
  }
  if (stderrText != null && stderrText.isNotEmpty) {
    stderr.write('$red$stderrText$reset');
  }
}

// Service additional functions
Future<void> serviceLock(String id, String publicKey, String? secretKey) async {
  await apiRequestCurl('/services/$id/lock', 'POST', '{}', publicKey, secretKey);
  print('${green}Service locked: $id$reset');
}

Future<void> serviceUnlock(String id, String publicKey, String? secretKey) async {
  final (statusCode, responseBody) = await apiRequestCurlWithStatus('/services/$id/unlock', 'POST', '{}', publicKey, secretKey);
  if (statusCode == 428) {
    if (await handleSudoChallenge(responseBody, '/services/$id/unlock', 'POST', '{}', publicKey, secretKey)) {
      print('${green}Service unlocked: $id$reset');
    } else {
      stderr.writeln('${red}Error: Failed to unlock service (OTP verification failed)$reset');
      exit(1);
    }
  } else if (statusCode >= 200 && statusCode < 300) {
    print('${green}Service unlocked: $id$reset');
  } else {
    stderr.writeln('${red}Error: Failed to unlock service (HTTP $statusCode)$reset');
    exit(1);
  }
}

Future<void> serviceRedeploy(String id, String? bootstrap, String publicKey, String? secretKey) async {
  final payload = bootstrap != null ? {'bootstrap': bootstrap} : <String, dynamic>{};
  await apiRequestCurl('/services/$id/redeploy', 'POST', jsonEncode(payload), publicKey, secretKey);
  print('${green}Service redeploying: $id$reset');
}

// PaaS logs functions
Future<void> logsFetch(String source, int lines, String? since, String? grepPattern, String publicKey, String? secretKey) async {
  var params = '?source=$source&lines=$lines';
  if (since != null) params += '&since=$since';
  if (grepPattern != null) params += '&grep=${Uri.encodeComponent(grepPattern)}';
  final result = await apiRequestCurl('/logs$params', 'GET', null, publicKey, secretKey);
  print(jsonEncode(result));
}

// Utility functions
Future<bool> healthCheck() async {
  try {
    final result = await Process.run('curl', ['-s', 'https://api.unsandbox.com/health']);
    print(result.stdout);
    return result.stdout.toString().contains('ok');
  } catch (e) {
    return false;
  }
}

String sdkVersion() {
  return '4.2.0';
}

Future<void> cmdKey(Args args) async {
  final keys = getApiKeys(args.apiKey);
  final publicKey = keys[0]!;
  final secretKey = keys[1];

  try {
    final result = await apiRequestCurl('/keys/validate', 'POST', null, publicKey, secretKey, baseUrl: portalBase);

    // Handle --extend flag
    if (args.keyExtend) {
      final publicKey = result['public_key'] as String?;
      if (publicKey != null) {
        final url = '$portalBase/keys/extend?pk=$publicKey';
        print('${blue}Opening browser to extend key...$reset');
        if (Platform.isMacOS) {
          await Process.run('open', [url]);
        } else if (Platform.isLinux) {
          await Process.run('xdg-open', [url]);
        } else if (Platform.isWindows) {
          await Process.run('cmd', ['/c', 'start', url]);
        } else {
          print('${yellow}Please open manually: $url$reset');
        }
        return;
      } else {
        stderr.writeln('${red}Error: Could not retrieve public key$reset');
        exit(1);
      }
    }

    // Check if key is expired
    final expired = result['expired'] as bool? ?? false;
    if (expired) {
      print('${red}Expired$reset');
      print('Public Key: ${result['public_key'] ?? 'N/A'}');
      print('Tier: ${result['tier'] ?? 'N/A'}');
      print('Expired: ${result['expires_at'] ?? 'N/A'}');
      print('${yellow}To renew: Visit $portalBase/keys/extend$reset');
      exit(1);
    }

    // Valid key
    print('${green}Valid$reset');
    print('Public Key: ${result['public_key'] ?? 'N/A'}');
    print('Tier: ${result['tier'] ?? 'N/A'}');
    print('Status: ${result['status'] ?? 'N/A'}');
    print('Expires: ${result['expires_at'] ?? 'N/A'}');
    print('Time Remaining: ${result['time_remaining'] ?? 'N/A'}');
    print('Rate Limit: ${result['rate_limit'] ?? 'N/A'}');
    print('Burst: ${result['burst'] ?? 'N/A'}');
    print('Concurrency: ${result['concurrency'] ?? 'N/A'}');
  } catch (e) {
    print('${red}Invalid$reset');
    print('Reason: $e');
    exit(1);
  }
}

Args parseArgs(List<String> argv) {
  final args = Args();
  var i = 0;
  while (i < argv.length) {
    switch (argv[i]) {
      case 'session':
        args.command = 'session';
        break;
      case 'service':
        args.command = 'service';
        break;
      case 'image':
        args.command = 'image';
        break;
      case 'snapshot':
        args.command = 'snapshot';
        break;
      case 'key':
        args.command = 'key';
        break;
      case 'languages':
        args.command = 'languages';
        break;
      case '--json':
        if (args.command == 'languages') {
          args.languagesJson = true;
        }
        break;
      case '-k':
      case '--api-key':
        args.apiKey = argv[++i];
        break;
      case '-n':
      case '--network':
        args.network = argv[++i];
        break;
      case '-v':
      case '--vcpu':
        args.vcpu = int.parse(argv[++i]);
        break;
      case '-e':
      case '--env':
        args.env.add(argv[++i]);
        break;
      case '-f':
      case '--files':
        args.files.add(argv[++i]);
        break;
      case '-a':
      case '--artifacts':
        args.artifacts = true;
        break;
      case '-o':
      case '--output-dir':
        args.outputDir = argv[++i];
        break;
      case '-l':
      case '--list':
        if (args.command == 'session') {
          args.sessionList = true;
        } else if (args.command == 'service') {
          args.serviceList = true;
        } else if (args.command == 'image') {
          args.imageList = true;
        } else if (args.command == 'snapshot') {
          args.snapshotList = true;
        }
        break;
      case '-s':
      case '--shell':
        if (args.command == 'snapshot') {
          args.snapshotShell = argv[++i];
        } else {
          args.sessionShell = argv[++i];
        }
        break;
      case '--kill':
        args.sessionKill = argv[++i];
        break;
      case '--name':
        if (args.command == 'image') {
          args.imageName = argv[++i];
        } else if (args.command == 'snapshot') {
          args.snapshotName = argv[++i];
        } else {
          args.serviceName = argv[++i];
        }
        break;
      case '--ports':
        if (args.command == 'image') {
          args.imagePorts = argv[++i];
        } else if (args.command == 'snapshot') {
          args.snapshotPorts = argv[++i];
        } else {
          args.servicePorts = argv[++i];
        }
        break;
      case '--type':
        args.serviceType = argv[++i];
        break;
      case '--bootstrap':
        args.serviceBootstrap = argv[++i];
        break;
      case '--bootstrap-file':
        args.serviceBootstrapFile = argv[++i];
        break;
      // --info handled below in the image command section
      case '--logs':
        args.serviceLogs = argv[++i];
        break;
      case '--tail':
        args.serviceTail = argv[++i];
        break;
      case '--freeze':
        args.serviceSleep = argv[++i];
        break;
      case '--unfreeze':
        args.serviceWake = argv[++i];
        break;
      case '--destroy':
        args.serviceDestroy = argv[++i];
        break;
      case '--resize':
        args.serviceResize = argv[++i];
        break;
      case '--vcpu':
        args.serviceResizeVcpu = int.parse(argv[++i]);
        break;
      case '--execute':
        args.serviceExecute = argv[++i];
        break;
      case '--command':
        args.serviceCommand = argv[++i];
        break;
      case '--dump-bootstrap':
        args.serviceDumpBootstrap = argv[++i];
        break;
      case '--dump-file':
        args.serviceDumpFile = argv[++i];
        break;
      case '--unfreeze-on-demand':
        args.serviceUnfreezeOnDemand = argv[++i];
        break;
      case '--unfreeze-on-demand-enabled':
        args.serviceUnfreezeOnDemandEnabled = argv[++i].toLowerCase() == 'true';
        break;
      case '--with-unfreeze-on-demand':
        args.serviceCreateUnfreezeOnDemand = true;
        break;
      case '--show-freeze-page':
        args.serviceShowFreezePage = argv[++i];
        break;
      case '--show-freeze-page-enabled':
        args.serviceShowFreezePageEnabled = argv[++i].toLowerCase() == 'true';
        break;
      case '--extend':
        args.keyExtend = true;
        break;
      case '--env-file':
        args.envFile = argv[++i];
        break;
      case '--info':
        if (args.command == 'service') {
          args.serviceInfo = argv[++i];
        } else if (args.command == 'image') {
          args.imageInfo = argv[++i];
        } else if (args.command == 'snapshot') {
          args.snapshotInfo = argv[++i];
        }
        break;
      case '--delete':
        if (args.command == 'image') {
          args.imageDelete = argv[++i];
        } else if (args.command == 'snapshot') {
          args.snapshotDelete = argv[++i];
        }
        break;
      case '--lock':
        if (args.command == 'image') {
          args.imageLock = argv[++i];
        } else if (args.command == 'snapshot') {
          args.snapshotLock = argv[++i];
        }
        break;
      case '--unlock':
        if (args.command == 'image') {
          args.imageUnlock = argv[++i];
        } else if (args.command == 'snapshot') {
          args.snapshotUnlock = argv[++i];
        }
        break;
      case '--publish':
        if (args.command == 'image') {
          args.imagePublish = argv[++i];
        }
        break;
      case '--source-type':
        args.imageSourceType = argv[++i];
        break;
      case '--visibility':
        if (args.command == 'image') {
          args.imageVisibility = argv[++i];
          if (i + 1 < argv.length && !argv[i + 1].startsWith('-')) {
            args.imageVisibilityMode = argv[++i];
          }
        }
        break;
      case '--spawn':
        if (args.command == 'image') {
          args.imageSpawn = argv[++i];
        }
        break;
      case '--clone':
        if (args.command == 'image') {
          args.imageClone = argv[++i];
        } else if (args.command == 'snapshot') {
          args.snapshotClone = argv[++i];
        }
        break;
      case '--session':
        if (args.command == 'snapshot') {
          args.snapshotSession = argv[++i];
        }
        break;
      case '--service':
        if (args.command == 'snapshot') {
          args.snapshotService = argv[++i];
        }
        break;
      case '--restore':
        if (args.command == 'snapshot') {
          args.snapshotRestore = argv[++i];
        }
        break;
      case '--clone-type':
        if (args.command == 'snapshot') {
          args.snapshotCloneType = argv[++i];
        }
        break;
      case '--hot':
        if (args.command == 'snapshot') {
          args.snapshotHot = true;
        }
        break;
      case 'env':
        if (args.command == 'service' && i + 1 < argv.length) {
          args.envAction = argv[++i];
          if (i + 1 < argv.length && !argv[i + 1].startsWith('-')) {
            args.envTarget = argv[++i];
          }
        }
        break;
      default:
        if (argv[i].startsWith('-')) {
          stderr.writeln('${red}Unknown option: ${argv[i]}$reset');
          exit(1);
        } else {
          args.sourceFile = argv[i];
        }
    }
    i++;
  }
  return args;
}

void printHelp() {
  print('''
Usage: dart un.dart [options] <source_file>
       dart un.dart session [options]
       dart un.dart service [options]
       dart un.dart snapshot [options]
       dart un.dart image [options]
       dart un.dart key [options]
       dart un.dart languages [--json]

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
  --unfreeze-on-demand ID   Set unfreeze-on-demand for service
  --unfreeze-on-demand-enabled BOOL   Enable/disable (default: true)
  --with-unfreeze-on-demand   Enable unfreeze-on-demand when creating service
  --show-freeze-page ID   Set show-freeze-page for service
  --show-freeze-page-enabled BOOL   Enable/disable (default: true)
  --destroy ID      Destroy service
  --execute ID      Execute command in service
  --command CMD     Command to execute (with --execute)
  --dump-bootstrap ID   Dump bootstrap script
  --dump-file FILE      File to save bootstrap (with --dump-bootstrap)

Service env commands:
  env status ID     Show vault status
  env set ID        Set vault (-e KEY=VALUE or --env-file FILE)
  env export ID     Export vault contents
  env delete ID     Delete vault

Image options:
  -l, --list        List all images
  --info ID         Get image details
  --delete ID       Delete an image
  --lock ID         Lock image to prevent deletion
  --unlock ID       Unlock image
  --publish ID      Publish image from service/snapshot (requires --source-type)
  --source-type TYPE    Source type: service or snapshot
  --visibility ID MODE  Set visibility: private, unlisted, or public
  --spawn ID        Spawn new service from image
  --clone ID        Clone an image
  --name NAME       Name for spawned service or cloned image
  --ports PORTS     Ports for spawned service

Snapshot options:
  -l, --list        List all snapshots
  --info ID         Get snapshot details
  --session ID      Create snapshot from session
  --service ID      Create snapshot from service
  --restore ID      Restore a snapshot
  --delete ID       Delete a snapshot
  --lock ID         Lock snapshot to prevent deletion
  --unlock ID       Unlock snapshot
  --clone ID        Clone snapshot to session/service
  --clone-type TYPE Clone target: session (default) or service
  --name NAME       Name for new snapshot or cloned resource
  --ports PORTS     Ports for service (with --clone --clone-type service)
  --shell NAME      Shell for session (with --clone --clone-type session)
  --hot             Hot snapshot (without stopping)

Key options:
  --extend          Open browser to extend key

Languages options:
  --json            Output as JSON array
''');
}

void main(List<String> arguments) async {
  try {
    final args = parseArgs(arguments);

    if (args.command == 'session') {
      await cmdSession(args);
    } else if (args.command == 'service') {
      await cmdService(args);
    } else if (args.command == 'image') {
      await cmdImage(args);
    } else if (args.command == 'snapshot') {
      await cmdSnapshot(args);
    } else if (args.command == 'key') {
      await cmdKey(args);
    } else if (args.command == 'languages') {
      await cmdLanguages(args);
    } else if (args.sourceFile != null) {
      await cmdExecute(args);
    } else {
      printHelp();
      exit(1);
    }
  } catch (e) {
    stderr.writeln('${red}Error: $e$reset');
    exit(1);
  }
}
