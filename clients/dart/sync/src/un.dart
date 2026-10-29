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

// SHA-256 and HMAC-SHA256 in plain Dart: no package:crypto, so un.dart runs
// with no pubspec, and the secret never leaves this process.
const List<int> _k256 = [
  0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
  0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
  0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
  0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
  0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
  0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
  0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
  0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
];

int _rotr(int x, int n) => ((x >> n) | (x << (32 - n))) & 0xffffffff;

List<int> sha256Bytes(List<int> data) {
  final h = [0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19];
  final bitLen = data.length * 8;
  final msg = List<int>.from(data)..add(0x80);
  while (msg.length % 64 != 56) {
    msg.add(0);
  }
  for (var i = 7; i >= 0; i--) {
    msg.add((bitLen >> (i * 8)) & 0xff);
  }
  final w = List<int>.filled(64, 0);
  for (var off = 0; off < msg.length; off += 64) {
    for (var i = 0; i < 16; i++) {
      w[i] = (msg[off + i * 4] << 24) | (msg[off + i * 4 + 1] << 16) | (msg[off + i * 4 + 2] << 8) | msg[off + i * 4 + 3];
    }
    for (var i = 16; i < 64; i++) {
      final s0 = _rotr(w[i - 15], 7) ^ _rotr(w[i - 15], 18) ^ (w[i - 15] >> 3);
      final s1 = _rotr(w[i - 2], 17) ^ _rotr(w[i - 2], 19) ^ (w[i - 2] >> 10);
      w[i] = (w[i - 16] + s0 + w[i - 7] + s1) & 0xffffffff;
    }
    var a = h[0], b = h[1], c = h[2], d = h[3], e = h[4], f = h[5], g = h[6], hh = h[7];
    for (var i = 0; i < 64; i++) {
      final s1 = _rotr(e, 6) ^ _rotr(e, 11) ^ _rotr(e, 25);
      final ch = (e & f) ^ ((~e & 0xffffffff) & g);
      final t1 = (hh + s1 + ch + _k256[i] + w[i]) & 0xffffffff;
      final s0 = _rotr(a, 2) ^ _rotr(a, 13) ^ _rotr(a, 22);
      final maj = (a & b) ^ (a & c) ^ (b & c);
      final t2 = (s0 + maj) & 0xffffffff;
      hh = g; g = f; f = e; e = (d + t1) & 0xffffffff;
      d = c; c = b; b = a; a = (t1 + t2) & 0xffffffff;
    }
    h[0] = (h[0] + a) & 0xffffffff; h[1] = (h[1] + b) & 0xffffffff;
    h[2] = (h[2] + c) & 0xffffffff; h[3] = (h[3] + d) & 0xffffffff;
    h[4] = (h[4] + e) & 0xffffffff; h[5] = (h[5] + f) & 0xffffffff;
    h[6] = (h[6] + g) & 0xffffffff; h[7] = (h[7] + hh) & 0xffffffff;
  }
  final out = <int>[];
  for (final v in h) {
    out.addAll([(v >> 24) & 0xff, (v >> 16) & 0xff, (v >> 8) & 0xff, v & 0xff]);
  }
  return out;
}

String _hex(List<int> bytes) => bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

String hmacSha256Hex(List<int> key, List<int> message) {
  var k = key.length > 64 ? sha256Bytes(key) : List<int>.from(key);
  k = k + List<int>.filled(64 - k.length, 0);
  final inner = sha256Bytes([...k.map((b) => b ^ 0x36), ...message]);
  return _hex(sha256Bytes([...k.map((b) => b ^ 0x5c), ...inner]));
}

// API base URL: UNSANDBOX_API_URL (default https://api.unsandbox.com), no trailing slash
final String apiBase = (() {
  final url = Platform.environment['UNSANDBOX_API_URL'] ?? '';
  return url.isEmpty ? 'https://api.unsandbox.com' : url.replaceAll(RegExp(r'/+$'), '');
})();
// Portal base URL: UNSANDBOX_PORTAL_URL (default https://unsandbox.com), no trailing slash.
// Serves /logs/{source} (partner log access) and /keys/extend.
final String portalBase = (() {
  final url = Platform.environment['UNSANDBOX_PORTAL_URL'] ?? '';
  return url.isEmpty ? 'https://unsandbox.com' : url.replaceAll(RegExp(r'/+$'), '');
})();
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
  String? publicKey;
  int? account;
  String? network;
  String? targetOs;
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
  bool keyRotate = false;
  String? keyOtp;
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

Map<String, String>? loadAccountsCSV(String path, int index) {
  try {
    final file = File(path);
    if (!file.existsSync()) return null;
    final lines = file.readAsLinesSync().where((l) {
      final t = l.trim();
      return t.isNotEmpty && !t.startsWith('#');
    }).toList();
    if (index < 0 || index >= lines.length) return null;
    final parts = lines[index].split(',');
    if (parts.length < 2) return null;
    final pk = parts[0].trim();
    final sk = parts[1].trim();
    if (pk.isEmpty || sk.isEmpty) return null;
    return {'pk': pk, 'sk': sk};
  } catch (e) {
    return null;
  }
}

List<String?> getApiKeys(String? argsKey, {String? argsPublicKey, int? account}) {
  // Tier 1: explicit -p/-k flags
  if (argsPublicKey != null && argsPublicKey.isNotEmpty && argsKey != null && argsKey.isNotEmpty) {
    return [argsPublicKey, argsKey];
  }

  // Tier 2: --account N → accounts.csv row N (bypasses env vars)
  if (account != null) {
    final home = Platform.environment['HOME'] ?? '';
    if (home.isNotEmpty) {
      final fromHome = loadAccountsCSV('$home/.unsandbox/accounts.csv', account);
      if (fromHome != null) return [fromHome['pk'], fromHome['sk']];
    }
    final fromLocal = loadAccountsCSV('./accounts.csv', account);
    if (fromLocal != null) return [fromLocal['pk'], fromLocal['sk']];
    stderr.writeln('${red}Error: --account $account not found in accounts.csv$reset');
    exit(1);
  }

  // Tier 3: env vars
  final envPk = Platform.environment['UNSANDBOX_PUBLIC_KEY'];
  final envSk = Platform.environment['UNSANDBOX_SECRET_KEY'];
  if (envPk != null && envPk.isNotEmpty && envSk != null && envSk.isNotEmpty) {
    return [envPk, envSk];
  }

  // Tier 4: ~/.unsandbox/accounts.csv row 0 (or UNSANDBOX_ACCOUNT env var)
  final home = Platform.environment['HOME'] ?? '';
  final defIndexStr = Platform.environment['UNSANDBOX_ACCOUNT'] ?? '0';
  final defIndex = int.tryParse(defIndexStr) ?? 0;
  if (home.isNotEmpty) {
    final fromHome = loadAccountsCSV('$home/.unsandbox/accounts.csv', defIndex);
    if (fromHome != null) return [fromHome['pk'], fromHome['sk']];
  }

  // Tier 5: ./accounts.csv row 0
  final fromLocal = loadAccountsCSV('./accounts.csv', defIndex);
  if (fromLocal != null) return [fromLocal['pk'], fromLocal['sk']];

  // Legacy UNSANDBOX_API_KEY fallback
  final legacyKey = argsKey ?? Platform.environment['UNSANDBOX_API_KEY'];
  if (legacyKey != null && legacyKey.isNotEmpty) {
    return [legacyKey, null];
  }

  stderr.writeln('${red}Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set$reset');
  exit(1);
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
      // Server verifies HMAC against our path without its query string
      final message = '$timestamp:$method:${endpoint.split('?').first}:$body';

      final signature = hmacSha256Hex(utf8.encode(secretKey), utf8.encode(message));

      args.addAll(['-H', 'Authorization: Bearer $publicKey']);
      args.addAll(['-H', 'X-Timestamp: $timestamp']);
      args.addAll(['-H', 'X-Signature: $signature']);
    } else {
      // Legacy API key authentication
      args.addAll(['-H', 'Authorization: Bearer $publicKey']);
    }

    if (jsonData != null) {
      args.addAll(['--data-binary', '@${tempFile.path}']);
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
      // Server verifies HMAC against our path without its query string
      final message = '$timestamp:$method:${endpoint.split('?').first}:$body';

      final signature = hmacSha256Hex(utf8.encode(secretKey), utf8.encode(message));

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
      args.addAll(['--data-binary', '@${tempFile.path}']);
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
      // Server verifies HMAC against our path without its query string
      final message = '$timestamp:$method:${endpoint.split('?').first}:$body';

      final signature = hmacSha256Hex(utf8.encode(secretKey), utf8.encode(message));

      args.addAll(['-H', 'Authorization: Bearer $publicKey']);
      args.addAll(['-H', 'X-Timestamp: $timestamp']);
      args.addAll(['-H', 'X-Signature: $signature']);
    } else {
      args.addAll(['-H', 'Authorization: Bearer $publicKey']);
    }

    args.addAll(['--data-binary', '@${tempFile.path}', '-w', '%{http_code}']);

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
  final keys = getApiKeys(args.apiKey, argsPublicKey: args.publicKey, account: args.account);
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
  final keys = getApiKeys(args.apiKey, argsPublicKey: args.publicKey, account: args.account);
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
  if (args.targetOs != null) {
    payload['os'] = args.targetOs;
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
  final keys = getApiKeys(args.apiKey, argsPublicKey: args.publicKey, account: args.account);
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
  if (args.targetOs != null) {
    payload['os'] = args.targetOs;
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
  final keys = getApiKeys(args.apiKey, argsPublicKey: args.publicKey, account: args.account);
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
    if (args.targetOs != null) {
      payload['os'] = args.targetOs;
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
  final keys = getApiKeys(args.apiKey, argsPublicKey: args.publicKey, account: args.account);
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
  final keys = getApiKeys(args.apiKey, argsPublicKey: args.publicKey, account: args.account);
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
  final keys = getApiKeys(args.apiKey, argsPublicKey: args.publicKey, account: args.account);
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

// PaaS logs: GET {portalBase}/logs/{source}[/stream], HMAC over the path only.
// Partner keys with log access; anything else answers 401/403. In-process HTTP.
int _unixNow() => DateTime.now().millisecondsSinceEpoch ~/ 1000;

String _logsQuery(String base, List<List<String?>> params) {
  final parts = <String>[];
  for (final p in params) {
    final v = p[1];
    if (v == null || v.isEmpty) continue;
    parts.add('${p[0]}=${Uri.encodeComponent(v)}');
  }
  return parts.isEmpty ? base : '$base?${parts.join('&')}';
}

String _logCursorPath() => '${Platform.environment['HOME'] ?? '.'}/.unsandbox/paas_log_cursor';

String? _readLogCursor() {
  try {
    final ts = int.parse(File(_logCursorPath()).readAsStringSync().trim());
    if (ts <= 0) return null;
    final ago = _unixNow() - ts;
    return '${ago < 5 ? 5 : ago}s';
  } catch (_) {
    return null;
  }
}

void _writeLogCursor() {
  try {
    final dir = Directory('${Platform.environment['HOME'] ?? '.'}/.unsandbox');
    if (!dir.existsSync()) {
      dir.createSync(recursive: true);
      Process.runSync('chmod', ['700', dir.path]);
    }
    File(_logCursorPath()).writeAsStringSync('${_unixNow()}\n');
  } catch (_) {}
}

Future<HttpClientResponse> _logsOpen(HttpClient client, String publicKey, String? secretKey, String path, String url, bool stream) async {
  final req = await client.getUrl(Uri.parse(url));
  req.headers.set('User-Agent', 'un-cli/2.0');
  req.headers.set('Authorization', 'Bearer $publicKey');
  if (stream) req.headers.set('Accept', 'text/event-stream');
  if (secretKey != null && secretKey.isNotEmpty) {
    // the secret only signs in-process; it never leaves this process
    final ts = _unixNow().toString();
    req.headers.set('X-Timestamp', ts);
    req.headers.set('X-Signature', hmacSha256Hex(utf8.encode(secretKey), utf8.encode('$ts:GET:$path:')));
  }
  return req.close();
}

void _logsPrintLine(String? source, String line) {
  if (source != null && source.isNotEmpty) {
    stdout.writeln('${source.padRight(12)} $line');
  } else {
    stdout.writeln(line);
  }
}

String _logsErrorText(String body) {
  try {
    final e = (jsonDecode(body) as Map)['error'];
    return e is String ? ' — $e' : '';
  } catch (_) {
    return '';
  }
}

/// Batch fetch from the portal; prints lines (or raw JSON). Returns an exit code.
Future<int> paasLogsFetch(String publicKey, String? secretKey, String source, int lines, String? since, String? grepPattern, String? level, bool jsonOutput) async {
  final path = '/logs/$source';
  final url = _logsQuery('$portalBase$path', [['lines', '$lines'], ['since', since], ['grep', grepPattern], ['level', level]]);
  final client = HttpClient()..connectionTimeout = const Duration(seconds: 30);
  try {
    int code;
    String body;
    try {
      final resp = await _logsOpen(client, publicKey, secretKey, path, url, false);
      code = resp.statusCode;
      body = await resp.transform(utf8.decoder).join();
    } catch (e) {
      stderr.writeln('Error: Network error fetching logs: $e');
      return 1;
    }
    if (code != 200) {
      stderr.writeln('Error: HTTP $code${_logsErrorText(body)}');
      return 1;
    }
    _writeLogCursor();
    if (jsonOutput) {
      stdout.writeln(body);
      return 0;
    }
    dynamic j;
    try { j = jsonDecode(body); } catch (_) {}
    final ls = j is Map ? j['lines'] : null;
    if (ls is! List) {
      stderr.writeln('Error: Invalid response format');
      return 1;
    }
    final isAll = (j as Map).containsKey('sources');
    for (final e in ls) {
      if (e is String) {
        _logsPrintLine(null, e);
      } else if (e is Map && e['line'] is String) {
        _logsPrintLine(isAll ? e['source'] as String? : null, e['line'] as String);
      }
    }
    return 0;
  } finally {
    client.close(force: true);
  }
}

/// Follow via SSE; reconnects with backoff (2 s doubling to 30 s), re-signing each time.
Future<int> paasLogsStream(String publicKey, String? secretKey, String source, String? grepPattern, String? level) async {
  final path = '/logs/$source/stream';
  stderr.writeln('Streaming $source logs... (Ctrl+C to stop)');
  var delay = 2;
  var lastRecv = 0;
  var reconnect = false;
  while (true) {
    String? since;
    if (reconnect && lastRecv > 0) {
      final gap = _unixNow() - lastRecv + 5;
      since = '${gap < 10 ? 10 : gap}s';
    }
    final url = _logsQuery('$portalBase$path', [['grep', grepPattern], ['level', level], ['since', since]]);
    final started = _unixNow();
    var code = 0;
    final client = HttpClient()..connectionTimeout = const Duration(seconds: 30);
    try {
      final resp = await _logsOpen(client, publicKey, secretKey, path, url, true);
      code = resp.statusCode;
      if (code == 200) {
        await for (final line in resp.transform(utf8.decoder).transform(const LineSplitter()).timeout(const Duration(seconds: 120))) {
          if (!line.startsWith('data: ')) continue;
          final payload = line.substring(6);
          if (payload == ':keepalive') continue;
          dynamic j;
          try { j = jsonDecode(payload); } catch (_) {}
          if (j is Map && j['line'] is String) {
            _logsPrintLine(j['source'] as String?, j['line'] as String);
            lastRecv = _unixNow();
          } else if (j is Map && j.containsKey('event')) {
            // lifecycle event (timeout/exit): nothing to print
          } else {
            _logsPrintLine(null, payload);
            lastRecv = _unixNow();
          }
        }
      } else {
        await resp.drain<void>();
      }
    } catch (_) {
      // dropped connection / timeout: reconnect below
    } finally {
      client.close(force: true);
    }
    if (code == 401 || code == 403) {
      stderr.writeln('Error: HTTP $code (auth failed)');
      return 1;
    } else if (code == 0 || code == 200 || code >= 500) {
      if (_unixNow() - started > 30) delay = 2;
      await Future.delayed(Duration(seconds: delay));
      delay = delay * 2 > 30 ? 30 : delay * 2;
      reconnect = true;
    } else {
      stderr.writeln('Error: HTTP $code');
      return 1;
    }
  }
}

const String _paasLogsHelp = '''Usage: un paas logs [options]

Fetch or stream PaaS production logs.

Sources:
  --all              All sources (default)
  --api              API server logs
  --portal           Portal server logs
  --pool NODE        Pool node logs (e.g., cammy, ai)
  --source SOURCE    Source string (api, portal, pool/cammy, all)

Options:
  --lines N, -n N    Number of lines (default: 100, max: 10000)
  --since TIME       Time window: 1m, 5m, 15m, 1h, 6h, 1d (default: 5m)
  --grep PATTERN     Filter log lines by pattern
  --level LVL, -l    Min log level: debug, info, notice, warning, err, crit
  --follow, -f       Follow logs in real-time (Ctrl+C to stop)
  --json             Output raw JSON response
  -p KEY             Public key
  -k KEY             Secret key
  --account N        Account index in ~/.unsandbox/accounts.csv
  -h                 Show this help

Requires a partner API key with log access enabled.''';

Future<int> cmdPaas(List<String> argv) async {
  if (argv.isEmpty) {
    stderr.writeln('Usage: un paas <command> [options]\n\nCommands:\n  logs    Fetch or stream production logs');
    return 1;
  }
  if (argv[0] != 'logs') {
    stderr.writeln("Error: Unknown paas command '${argv[0]}'");
    return 1;
  }
  var source = 'all';
  var lines = 100;
  String? since, grepPattern, level, pk, sk;
  int? account;
  var follow = false, jsonOutput = false;
  for (var i = 1; i < argv.length; i++) {
    final opt = argv[i];
    final val = i + 1 < argv.length ? argv[i + 1] : null;
    if (opt == '-h' || opt == '--help') {
      stderr.writeln(_paasLogsHelp);
      return 0;
    } else if (opt == '--all') {
      source = 'all';
    } else if (opt == '--api') {
      source = 'api';
    } else if (opt == '--portal') {
      source = 'portal';
    } else if (opt == '--follow' || opt == '-f') {
      follow = true;
    } else if (opt == '--json') {
      jsonOutput = true;
    } else if (val == null) {
      continue;
    } else if (opt == '--pool') {
      source = 'pool/$val'; i++;
    } else if (opt == '--source') {
      source = val; i++;
    } else if (opt == '--lines' || opt == '-n') {
      final n = int.tryParse(val) ?? 1;
      lines = n < 1 ? 1 : (n > 10000 ? 10000 : n);
      i++;
    } else if (opt == '--since') {
      since = val; i++;
    } else if (opt == '--grep') {
      grepPattern = val; i++;
    } else if (opt == '--level' || opt == '-l') {
      level = val; i++;
    } else if (opt == '-p') {
      pk = val; i++;
    } else if (opt == '-k') {
      sk = val; i++;
    } else if (opt == '--account') {
      account = int.tryParse(val); i++;
    }
  }
  final keys = getApiKeys(sk, argsPublicKey: pk, account: account);
  final publicKey = keys[0]!;
  final secretKey = keys[1];
  if (follow) return paasLogsStream(publicKey, secretKey, source, grepPattern, level);
  return paasLogsFetch(publicKey, secretKey, source, lines, since ?? _readLogCursor() ?? '5m', grepPattern, level, jsonOutput);
}

// Library form kept for callers: prints the raw JSON response.
Future<void> logsFetch(String source, int lines, String? since, String? grepPattern, String publicKey, String? secretKey, {String? level}) async {
  await paasLogsFetch(publicKey, secretKey, source, lines, since ?? '5m', grepPattern, level, true);
}

// Utility functions
Future<bool> healthCheck() async {
  try {
    final result = await Process.run('curl', ['-s', '$apiBase/health']);
    print(result.stdout);
    return result.stdout.toString().contains('ok');
  } catch (e) {
    return false;
  }
}

String sdkVersion() {
  return '4.2.0';
}

/// The accounts.csv the credentials came from, or null (flags / environment
/// variables). Mirrors getApiKeys tiers.
String? credentialsCsvPath(Args args) {
  if (args.publicKey != null && args.publicKey!.isNotEmpty && args.apiKey != null && args.apiKey!.isNotEmpty) {
    return null;
  }
  var index = args.account;
  if (index == null) {
    final envPk = Platform.environment['UNSANDBOX_PUBLIC_KEY'] ?? '';
    final envSk = Platform.environment['UNSANDBOX_SECRET_KEY'] ?? '';
    if (envPk.isNotEmpty && envSk.isNotEmpty) return null;
    index = int.tryParse(Platform.environment['UNSANDBOX_ACCOUNT'] ?? '0') ?? 0;
  }
  final home = Platform.environment['HOME'] ?? '';
  final paths = [if (home.isNotEmpty) '$home/.unsandbox/accounts.csv', './accounts.csv'];
  for (final path in paths) {
    if (loadAccountsCSV(path, index) != null) return path;
  }
  return null;
}

/// Replace the secret on the line whose first field equals [publicKey]. Every other
/// line, field, comment and the order stay as they are. Temp file (0600) + rename.
Future<bool> rewriteAccountsCsv(String path, String publicKey, String newSecret) async {
  final file = File(path);
  final lines = (await file.readAsString()).split('\n');
  final idx = lines.indexWhere((line) {
    final t = line.trim();
    final fields = line.split(',');
    return t.isNotEmpty && !t.startsWith('#') && fields.length >= 2 && fields[0].trim() == publicKey;
  });
  if (idx < 0) return false;
  final fields = lines[idx].split(',');
  fields[1] = newSecret;
  lines[idx] = fields.join(',');
  final tmp = File('${file.absolute.parent.path}/.${file.uri.pathSegments.last}.$pid.tmp');
  await tmp.writeAsString('');
  await Process.run('chmod', ['600', tmp.path]);
  await tmp.writeAsString(lines.join('\n'));
  await tmp.rename(path);
  return true;
}

/// POST /keys/self/rotate-secret with body {}, HMAC computed in-process.
Future<List<dynamic>> rotatePost(String publicKey, String secretKey, String? otp, String? challenge) async {
  const path = '/keys/self/rotate-secret';
  const body = '{}';
  final timestamp = (DateTime.now().millisecondsSinceEpoch ~/ 1000).toString();
  final signature = hmacSha256Hex(utf8.encode(secretKey), utf8.encode('$timestamp:POST:$path:$body'));
  final client = HttpClient();
  try {
    final req = await client.postUrl(Uri.parse('$apiBase$path'));
    req.headers.set('Authorization', 'Bearer $publicKey');
    req.headers.set('X-Timestamp', timestamp);
    req.headers.set('X-Signature', signature);
    req.headers.contentType = ContentType('application', 'json');
    if (otp != null) {
      req.headers.set('X-Sudo-OTP', otp);
      req.headers.set('X-Sudo-Challenge', challenge ?? '');
    }
    final bodyBytes = utf8.encode(body);
    req.contentLength = bodyBytes.length;
    req.add(bodyBytes);
    final resp = await req.close();
    final text = await resp.transform(utf8.decoder).join();
    Map<String, dynamic> data = {};
    try {
      final decoded = jsonDecode(text);
      if (decoded is Map<String, dynamic>) data = decoded;
    } catch (_) {}
    return [resp.statusCode, data];
  } finally {
    client.close();
  }
}

/// stdin.hasTerminal reports any character device (such as /dev/null) as a
/// terminal; reading the echo mode needs a real tty and throws otherwise.
bool stdinIsTerminal() {
  try {
    stdin.echoMode;
    return stdin.hasTerminal;
  } catch (_) {
    return false;
  }
}

/// un key --rotate [--otp CODE]: rotate this key's secret (see CLI_SPEC.md)
Future<void> cmdKeyRotate(Args args) async {
  final keys = getApiKeys(args.apiKey, argsPublicKey: args.publicKey, account: args.account);
  final publicKey = keys[0]!;
  final secretKey = keys[1];
  if (secretKey == null || secretKey.isEmpty) {
    stderr.writeln('Error: authentication failed');
    exit(3);
  }
  final home = Platform.environment['HOME'] ?? '.';
  final dir = Directory('$home/.unsandbox');
  final hash = _hex(sha256Bytes(utf8.encode(publicKey))).substring(0, 16);
  final challengeFile = File('${dir.path}/rotate-$hash.challenge');

  List<dynamic> result;
  if (args.keyOtp != null) {
    if (!challengeFile.existsSync()) {
      stderr.writeln('Error: no rotation in progress; run: un key --rotate');
      exit(2);
    }
    final challenge = (await challengeFile.readAsString()).trim();
    result = await rotatePost(publicKey, secretKey, args.keyOtp!.trim(), challenge);
    if (challengeFile.existsSync()) await challengeFile.delete();
  } else {
    result = await rotatePost(publicKey, secretKey, null, null);
    if (result[0] == 428) {
      final challenge = ((result[1] as Map<String, dynamic>)['challenge_id'] ?? '').toString();
      if (stdinIsTerminal()) {
        stderr.writeln('One-time code emailed to the key owner.');
        stderr.write('Code: ');
        await stderr.flush();
        final otp = (stdin.readLineSync() ?? '').trim();
        if (otp.isEmpty) {
          stderr.writeln('Error: cancelled');
          exit(1);
        }
        result = await rotatePost(publicKey, secretKey, otp, challenge);
      } else {
        await dir.create(recursive: true);
        await Process.run('chmod', ['700', dir.path]);
        await challengeFile.writeAsString('');
        await Process.run('chmod', ['600', challengeFile.path]);
        await challengeFile.writeAsString(challenge);
        print('One-time code emailed to the key owner. Finish with: un key --rotate --otp CODE');
        await stdout.flush();
        exit(0);
      }
    }
  }

  final code = result[0] as int;
  final data = result[1] as Map<String, dynamic>;
  if (code != 200) {
    if (code == 403 && data['error'] == 'email_required') {
      stderr.writeln('Error: this key has no email; add one or use POST /keys/recover');
      exit(3);
    }
    if (code == 401) {
      stderr.writeln('Error: authentication failed');
      exit(3);
    }
    stderr.writeln('Error: ${data['message'] ?? data['error'] ?? 'HTTP $code'}');
    exit(4);
  }
  final newSecret = (data['secret'] ?? '').toString();
  if (newSecret.isEmpty) {
    stderr.writeln('Error: API returned no secret');
    exit(4);
  }

  final csvPath = credentialsCsvPath(args);
  if (csvPath != null && await rewriteAccountsCsv(csvPath, publicKey, newSecret)) {
    print('Secret rotated. Updated $csvPath (public key $publicKey).');
  } else {
    print('New secret (shown once, store it now): $newSecret');
    print('Warning: UNSANDBOX_SECRET_KEY / -k still hold the old, now invalid, secret.');
  }
}

Future<void> cmdKey(Args args) async {
  if (args.keyRotate) {
    await cmdKeyRotate(args);
    return;
  }
  final keys = getApiKeys(args.apiKey, argsPublicKey: args.publicKey, account: args.account);
  final publicKey = keys[0]!;
  final secretKey = keys[1];

  // Handle --extend flag
  if (args.keyExtend) {
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
  }

  int code;
  String text;
  try {
    (code, text) = await apiRequestCurlWithStatus('/keys/validate', 'POST', null, publicKey, secretKey);
  } catch (e) {
    stderr.writeln('${red}Error: request to $apiBase/keys/validate failed: $e$reset');
    exit(1);
  }
  if (code == 0) {
    stderr.writeln('${red}Error: request to $apiBase/keys/validate failed$reset');
    exit(1);
  }
  Map<String, dynamic> result = {};
  try {
    final d = jsonDecode(text);
    if (d is Map<String, dynamic>) result = d;
  } catch (_) {}

  String state;
  String reason;
  if (code == 200) {
    state = result['status']?.toString() ?? (result['valid'] == true ? 'active' : 'invalid');
    reason = 'status $state';
  } else if (code == 401) {
    reason = result['error']?.toString() ?? 'unauthorized';
    state = (reason == ':expired' || reason == 'expired')
        ? 'expired'
        : (reason == ':suspended' || reason == 'suspended') ? 'suspended' : 'invalid';
  } else {
    final msg = result['error'] ?? result['message'];
    stderr.writeln('${red}Error: HTTP $code${msg != null ? ' $msg' : ''}$reset');
    exit(4);
  }

  if (state == 'expired') {
    print('${red}Expired$reset');
    print('Public Key: $publicKey');
    print('${yellow}To renew: Visit $portalBase/keys/extend$reset');
    exit(3);
  } else if (state == 'suspended') {
    print('${red}Suspended$reset');
    print('Public Key: $publicKey');
    exit(3);
  } else if (state != 'active') {
    print('${red}Invalid$reset');
    print('Public Key: $publicKey');
    print('Reason: $reason');
    exit(3);
  }

  var expires = 'N/A';
  var remaining = 'N/A';
  final vt = result['valid_through'];
  if (vt is num) {
    final secs = vt.toInt() ~/ 1000;
    final dt = DateTime.fromMillisecondsSinceEpoch(secs * 1000, isUtc: true);
    String two(int n) => n.toString().padLeft(2, '0');
    expires = '${dt.year.toString().padLeft(4, '0')}-${two(dt.month)}-${two(dt.day)}T${two(dt.hour)}:${two(dt.minute)}:${two(dt.second)}Z';
    final left = secs - DateTime.now().millisecondsSinceEpoch ~/ 1000;
    remaining = left <= 0
        ? 'expired'
        : '${left ~/ 86400}d ${(left % 86400) ~/ 3600}h ${(left % 3600) ~/ 60}m';
  }
  print('${green}Valid$reset');
  print('Public Key: $publicKey');
  print('Tier: ${result['tier'] ?? 'N/A'}');
  print('Status: ${result['status'] ?? 'N/A'}');
  print('Expires: $expires');
  print('Time Remaining: $remaining');
}

// Normalize a --os value: linux or freebsd (case-insensitive), else error and exit
String normalizeTargetOs(String value) {
  final os = value.toLowerCase();
  if (os != 'linux' && os != 'freebsd') {
    stderr.writeln("${red}Error: invalid --os '$value' (expected linux or freebsd)$reset");
    exit(1);
  }
  return os;
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
      case '-p':
      case '--public-key':
        args.publicKey = argv[++i];
        break;
      case '--account':
        args.account = int.parse(argv[++i]);
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
      case '--rotate':
        args.keyRotate = true;
        break;
      case '--otp':
        args.keyOtp = argv[++i];
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
      case '--os':
        if (i + 1 >= argv.length) {
          stderr.writeln('${red}Error: --os requires a value: linux or freebsd$reset');
          exit(1);
        }
        args.targetOs = normalizeTargetOs(argv[++i]);
        break;
      default:
        if (argv[i].startsWith('--os=')) {
          args.targetOs = normalizeTargetOs(argv[i].substring(5));
        } else if (argv[i].startsWith('-')) {
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
  --os OS           Target OS: linux (default) or freebsd (execute, session, service)
  -v N              vCPU count (1-8)
  -p KEY            Public key (use with -k for secret key)
  -k KEY            Secret/API key
  --account N       Use row N from accounts.csv (0-based)

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
  if (arguments.isNotEmpty && arguments[0] == 'paas') {
    final rc = await cmdPaas(arguments.sublist(1));
    await stdout.flush();
    exit(rc);
  }
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
